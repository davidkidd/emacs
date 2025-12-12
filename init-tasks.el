(ensure-vc-package
 'task-find
 "https://github.com/davidkidd/task-find")

(use-package task-find
  :init
  (setq task-find-highlight-scope 'custom
        task-find-highlight-custom-predicate
        #'my-task-find-highlight-not-in-code-string)

  :config
  (set-face-attribute 'task-find-face-category nil
                      :foreground "#999999"
                      :weight 'bold)
  (set-face-attribute 'task-find-face-tag nil
                      :foreground "#999999")
  (global-task-find-hl-mode 1))

;; Helper: custom predicate for highlighting
(defun my-task-find-highlight-not-in-code-string ()
  "Return non-nil when `task-find' should highlight at point.

Allows highlighting everywhere *except* code string literals.
Docstrings (using `font-lock-doc-face') are still treated as
documentation, so they are allowed."
  (let* ((ppss      (syntax-ppss))
         (in-string (nth 3 ppss))
         (face      (or (get-text-property (point) 'face)
                        (get-text-property (point) 'font-lock-face)))
         (faces     (if (listp face) face (list face)))
         (doc-p     (memq 'font-lock-doc-face faces)))
    (or (not in-string) doc-p)))

;; Helper: example canned search
(defun my/task-find-test-search ()
  "Test function for a canned search."
  (interactive)
  (task-find-run-this "BUG" '("urgent" "re:dav.*") nil))
