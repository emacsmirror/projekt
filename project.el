;; make this a minor mode
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

(defvar project-mode-map
  (let ((map (make-sparse-keymap)))
    ;; ([menu-bar] (make-sparse-keymap))
    ;; ([menu-bar project] (cons "Project" (make-sparse-keymap "Project")))
    ;; ([menu-bar project])
    (define-key map (kbd "C-c p a") 'project-add-file)
    (define-key map (kbd "C-c p e") 'project-edit-list)
    (define-key map [menu-bar project]
      (cons "Project" (make-sparse-keymap "Project")))

    (define-key map [menu-bar project add-file]
      '(menu-item "Add current file to commit list" project-add-file))

    (define-key map [menu-bar project edit-list]
      '(menu-item "Edit commit list" project-edit-list))

    map))

(define-minor-mode project-mode
    "Toggle project mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When project mode is enabled it allows to to create a commit list
and add files or edit it."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " .prj"
  ;; The minor mode bindings.
  project-mode-map
  :group 'project)

(defvar projects-list nil "keeps a list of open projects")
; '((<tld> :dir "/path/to/project" :commit "commit-file-name OR buffer")))


(defun project-hook ()
  (when (and buffer-file-name (project-get buffer-file-name))
    (project-mode t)))
(add-hook 'find-file-hook 'project-hook)

(defun project-get (file)
  (when (project-find-root)
    (let* ((root (project-find-root))
           (name (file-name-nondirectory (directory-file-name root)))
           (prj (assoc name projects-list)))
      (when (not prj)
        (setq prj (list name :dir root :commit (concat root "commit")))
        (setq projects-list (cons prj projects-list)))
      prj)))

(defun project-file-p ()
  "Checks if the file associated with the current buffer is in a project"
  (when buffer-file-name
    (let (project-root)
      (dolist (prj projects-list)
        (setq project-root (plist-get (cdr prj) :dir))
        (when (string-equal
               project-root
               (substring buffer-file-name 0 (length project-root)))
          prj)))))

(defvar project-comment-buf nil
  "Buffer zum kommentieren eines Commits")

(defun project-find-root ()
  (let ((max-depth 10)
        (depth 0)
        (file ".project"))
    (while (and (< depth max-depth)
                (not (file-exists-p file)))
      (setq file (concat "../" file))
      (setq depth (+ depth 1)))
    (when (file-exists-p file)
      (expand-file-name (substring (file-truename file) 0 -8)))))

(defun project-add-file ()
  "Add current file to commit list"
  (interactive)
  (let* ((prj (project-get buffer-file-name))
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

(defun project-edit-list ()
  "Edit the commit list"
  (interactive)
  (let* ((root (project-find-root))
         (commit (concat root "commit")))
    (when (file-exists-p commit)
      (find-file commit))))

(defun project-commit ()
  "open commit list then commit files in list"
  (interactive)
  (if (eq (current-buffer) project-comment-buf)
      (project-cvs-commit)
    (let ((root (project-find-root))
          (file (buffer-file-name)))
      (if (not (string= file (concat root "commit")))
          (project-edit-list)
        (project-get-comment)))))

(defun project-get-comment ()
  (setq project-comment-buf (generate-new-buffer "cvs commit comment"))
  (switch-to-buffer project-comment-buf))

(defun project-cvs-commit ()
  (let ((comment (buffer-string))
        (root (project-find-root))
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
    (kill-buffer project-comment-buf)
    (setq project-comment-buf nil)))

(defun project-cvs-remove ()
  (interactive)
  (project-add-file)
  (delete-file (buffer-file-name))
  (shell-command (concat "cvs remove " (project-buffer-file))))

(defun project-cvs-add ()
  (interactive)
  (project-add-file)
  (shell-command (concat "cvs add " (project-buffer-file))))

(defun project-buffer-file ()
  (string-match ".*/" (buffer-file-name))
  (substring (buffer-file-name) (match-end 0)))
