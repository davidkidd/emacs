;; I'm mosly using my local dev version of task-find, if it's available.
;; Using Emacs on my non-dev machines will pull the latest version from the repo.
;; Both the local dev path and package URL are defcustoms (task-find-bootstrap group).
;; If it doesn't have a dev path or a github URL, it will display a warning on boot.
;; If you don't want the warning, then you shouldn't load this file in the first place.

(require 'package)
(unless package--initialized
  (package-initialize))

(defgroup task-find-bootstrap nil
  "Bootstrap logic for loading task-find."
  :group 'tools)

(defcustom task-find-dev-path nil
  "Local development checkout directory for task-find.

If nil, local development loading is disabled."
  :type '(choice
          (const :tag "Disabled" nil)
          directory)
  :group 'task-find-bootstrap)

(defcustom task-find-github-url nil
  "Git repository URL used to install task-find if not present.

If nil, automatic installation is disabled."
  :type '(choice
          (const :tag "Disabled" nil)
          string)
  :group 'task-find-bootstrap)


(defun task-find--dev-present-p ()
  "Return non-nil if a usable local dev checkout exists."
  (and (stringp task-find-dev-path)
       (file-directory-p task-find-dev-path)
       (file-exists-p (expand-file-name "task-find.el" task-find-dev-path))))

(defun task-find--installed-p ()
  "Return non-nil if task-find is already installed."
  (locate-library "task-find"))

(defun task-find--load-dev ()
  "Load task-find from local dev checkout."
  (add-to-list 'load-path task-find-dev-path)
  (require 'task-find)
  (message "task-find: loaded from local dev path: %s" task-find-dev-path))

(defun task-find--load-installed ()
  "Load task-find from installed package."
  (require 'task-find)
  (message "task-find: loaded installed package"))

(defun task-find--install-from-github ()
  "Install task-find from GitHub using package-vc (Emacs 29+)."
  (unless (fboundp 'package-vc-install)
    (error "package-vc-install not available (requires Emacs 29+)"))
  (unless (and (stringp task-find-github-url)
               (not (string-empty-p task-find-github-url)))
    (error "task-find-github-url is not set"))
  (package-vc-install task-find-github-url))

;; decide what to do 

(cond
 ;; 1) Prefer local dev, if configured and present
 ((and (stringp task-find-dev-path)
       (file-directory-p task-find-dev-path))
  (add-to-list 'load-path task-find-dev-path)
  (require 'task-find)
  (message "task-find: loaded from dev path: %s" task-find-dev-path))

 ;; 2) Otherwise, if already installed, load the packaged version
 ((locate-library "task-find")
  (require 'task-find)
  (message "task-find: loaded installed package"))

 ;; 3) Otherwise, only *ask to download* if we actually have a URL
 ((and (stringp task-find-github-url)
       (not (string-empty-p (string-trim task-find-github-url))))
  (when (yes-or-no-p (format "task-find not found. Install from %s ? " task-find-github-url))
    ;; call your install function here
    (ensure-vc-package 'task-find task-find-github-url)
    (require 'task-find)
    (message "task-find: installed and loaded from GitHub")))

 ;; 4) Otherwise: nothing available, don’t prompt, just warn
 (t
  (display-warning
   'task-find
   "task-find: not available (no local dev path found, not installed, and no GitHub URL configured)."
   :warning)))


;; Actual user task-find config (runs only if task-find loaded)

(with-eval-after-load 'task-find
  (setq task-find-highlight-scope 'custom
        task-find-highlight-custom-predicate
        #'my-task-find-highlight-not-in-code-string)

  (set-face-attribute 'task-find-face-category nil
                      :foreground "#999999"
                      :weight 'bold)
  (set-face-attribute 'task-find-face-tag nil
                      :foreground "#999999")

  (global-task-find-hl-mode 1))
