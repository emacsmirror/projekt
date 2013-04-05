;;; -*- lexical-binding: t -*-
;;; projekt.el --- some kind of staging for CSV

;; Author: Engelke Eschner <tekai@gmx.li>
;; Version: 0.1
;; Created: 2013-04-04

;; LICENSE
;; Copyright (c) 2013 Engelke Eschner
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.
;;     * Neither the name of the projekt.el nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT
;; HOLDER> BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
;; OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; DESCRIPTION
;; This package implements the Growl Notification Protocol GNTP
;; described at http://www.growlforwindows.com/gfw/help/gntp.aspx

;; Ideas
;; - Add list of files  in commit to menu (with function
;;   switch-to-buffer or find-file)
;; - Better remove/add (no dups)
;; - deployment ?
;; - foo deploy ?
;; - bak?
;; - keep patch/rev stuff
;; - use vc instead of straight cvs
;; - parse .project for more data?

(defvar projekt-mode-map
  (let ((map (make-sparse-keymap)))

    ;; Keybindings
    (define-key map (kbd "C-c p a") 'projekt-add-file)
    (define-key map (kbd "C-c p e") 'projekt-edit-list)

    ;; Menu
    (define-key map [menu-bar projekt]
      (cons "Projekt" (make-sparse-keymap "Projekt")))

    (define-key map [menu-bar projekt add-file]
      '(menu-item "Add current file to commit list" projekt-add-file))

    (define-key map [menu-bar projekt edit-list]
      '(menu-item "Edit commit list" projekt-edit-list))

    map))

(define-minor-mode projekt-mode
    "Toggle projekt mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When projekt mode is enabled it allows to to create a commit list
and add files or edit it."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " .prj"
  ;; The minor mode bindings.
  projekt-mode-map
  :group 'projekt)

(defvar projekts-list nil "keeps a list of open projects")
; '((<tld> :dir "/path/to/project" :commit "commit-file-name OR buffer")))


(defun projekt-hook ()
  (when (and buffer-file-name (projekt-get buffer-file-name))
    (projekt-mode t)))
(add-hook 'find-file-hook 'projekt-hook)

(defun projekt-get (file)
  (when (projekt-find-root)
    (let* ((root (projekt-find-root))
           (name (file-name-nondirectory (directory-file-name root)))
           (prj (assoc name projekts-list)))
      (when (not prj)
        (setq prj (list name :dir root :commit (concat root "commit")))
        (setq projekts-list (cons prj projekts-list)))
      prj)))

(defun projekt-file-p ()
  "Checks if the file associated with the current buffer is in a project"
  (when buffer-file-name
    (let (projekt-root)
      (dolist (prj projekts-list)
        (setq projekt-root (plist-get (cdr prj) :dir))
        (when (string-equal
               projekt-root
               (substring buffer-file-name 0 (length projekt-root)))
          prj)))))

(defvar projekt-comment-buf nil
  "Buffer zum kommentieren eines Commits")

(defun projekt-find-root ()
  (let ((max-depth 10)
        (depth 0)
        (file ".project"))
    (while (and (< depth max-depth)
                (not (file-exists-p file)))
      (setq file (concat "../" file))
      (setq depth (+ depth 1)))
    (when (file-exists-p file)
      (expand-file-name (substring (file-truename file) 0 -8)))))

(defun projekt-add-file ()
  "Add current file to commit list"
  (interactive)
  (let* ((prj (projekt-get buffer-file-name))
         (root (plist-get (cdr prj) :dir))
         (commit (plist-get (cdr prj) :commit))
         (file (expand-file-name (file-truename (buffer-file-name)))))
    (save-current-buffer
      (let ((buf (if (bufferp commit)
                     buf
                   (find-file-noselect commit))))
        (set-buffer buf)
        (goto-char (point-max))
        (when (not (looking-at ?\n))
          (insert ?\n))
        (insert (substring file (length root)))
        (insert ?\n)
        (save-buffer buf)))))

(defun projekt-edit-list ()
  "Edit the commit list"
  (interactive)
  (let* ((root (projekt-find-root))
         (commit (concat root "commit")))
    (when (file-exists-p commit)
      (find-file commit))))

(defun projekt-commit ()
  "open commit list then commit files in list"
  (interactive)
  (if (eq (current-buffer) projekt-comment-buf)
      (projekt-cvs-commit)
    (let ((root (projekt-find-root))
          (file (buffer-file-name)))
      (if (not (string= file (concat root "commit")))
          (projekt-edit-list)
        (projekt-get-comment)))))

(defun projekt-get-comment ()
  (setq projekt-comment-buf (generate-new-buffer "cvs commit comment"))
  (switch-to-buffer projekt-comment-buf))

(defun projekt-cvs-commit ()
  (let ((comment (buffer-string))
        (root (projekt-find-root))
        (buf nil)
        (files nil))
    (setq buf (find-file-noselect (concat root "commit")))
    (set-buffer buf)
    (setq files (buffer-string))
    (setq files (replace-regexp-in-string "\n" " " files))
    (setq files (replace-regexp-in-string "^ *" "" files))
    (setq files (replace-regexp-in-string " *$" "" files))
    (setq comment (replace-regexp-in-string "\\\\" "\\\\\\\\" comment))
    (setq comment (replace-regexp-in-string "\"" "\\\\\"" comment))
    (cd-absolute root)
    (shell-command (concat "cvs commit -m \"" comment "\" " files))
    (kill-buffer projekt-comment-buf)
    (setq projekt-comment-buf nil)))

(defun projekt-cvs-remove ()
  (interactive)
  (projekt-add-file)
  (delete-file (buffer-file-name))
  (shell-command (concat "cvs remove " (projekt-buffer-file))))

(defun projekt-cvs-add ()
  (interactive)
  (projekt-add-file)
  (shell-command (concat "cvs add " (projekt-buffer-file))))

(defun projekt-buffer-file ()
  (string-match ".*/" (buffer-file-name))
  (substring (buffer-file-name) (match-end 0)))
