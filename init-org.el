;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org mode configuration.

;;; Code:

(require 'org-capture)
(require 'org-id)
(require 'project)
(require 'seq)

(defun my/org-project-org-files ()
  "Return Org files belonging to the current project."
  (if-let ((project (project-current nil)))
      (seq-filter
       (lambda (file)
         (string-match-p "\\.org\\'" file))
       (project-files project))
    (and buffer-file-name
         (list buffer-file-name))))

(defun my/org-project-agenda ()
  "Open `org-agenda' using Org files in the current project."
  (interactive)
  (setq org-agenda-files (my/org-project-org-files))
  (call-interactively #'org-agenda))

(defun my/org-insert-project-link ()
  "Select a project Org heading and insert an ID link to it."
  (interactive)
  (let* ((id
          (org-id-get-with-outline-path-completion
           '((my/org-project-org-files :maxlevel . 9))))
         (marker (org-id-find id 'marker))
         (description
          (if marker
              (unwind-protect
                  (org-with-point-at marker
                    (org-get-heading t t t t))
                (set-marker marker nil))
            id)))
    (org-insert-link nil
                     (concat "id:" id)
                     description)))

(setq org-refile-targets
      '((my/org-project-org-files :maxlevel . 3))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

(defun my/org-capture-origin-project-root ()
  "Return the project root of the buffer that started capture."
  (when-let* ((buffer (org-capture-get :original-buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (when-let ((project (project-current nil)))
        (file-name-as-directory
         (expand-file-name (project-root project)))))))

(defun my/org-read-notes-target ()
  "Prompt for a project and return its notes.org file name."
  (let* ((current-root (my/org-capture-origin-project-root))
         (known-roots
          (mapcar
           (lambda (root)
             (file-name-as-directory (expand-file-name root)))
           (project-known-project-roots)))
         (roots
          (seq-filter
           #'file-directory-p
           (delete-dups (delq nil (cons current-root known-roots)))))
         (project-choices
          (mapcar
           (lambda (root)
             (cons
              (format "%s%s — %s"
                      (if (equal root current-root) "[current] " "")
                      (file-name-nondirectory (directory-file-name root))
                      (abbreviate-file-name root))
              (expand-file-name "notes.org" root)))
           roots))
         (global-label "Global — ~/notes.org")
         (global-file (expand-file-name "~/notes.org"))
         (choices
          (append project-choices
                  (list (cons global-label global-file))))
         (default
          (if current-root
              (car (rassoc (expand-file-name "notes.org" current-root)
                           project-choices))
            global-label))
         (selection
          (completing-read "Capture note in: "
                           choices nil t nil nil default)))
    (cdr (assoc selection choices))))

(defun my/org-capture-project-note-target ()
  "Visit the selected project's notes.org capture target."
  (find-file (my/org-read-notes-target))
  (when (= (buffer-size) 0)
    (insert "#+title: Notes\n\n"))
  (goto-char (point-max))
  (unless (bolp)
    (insert "\n")))

(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline "" "Tasks")
         "* TODO %?\n  %u\n  %a")
        ("p" "Project note" entry
         (function my/org-capture-project-note-target)
         nil
         :empty-lines 1)))

