;; I'm mosly using my local dev version of novadb-emacs, if it's available.
;; Using Emacs on my non-dev machines will pull the latest version from the repo.
;; Both the local dev path and package URL are defcustoms (novadb-bootstrap group).
;; If it doesn't have a dev path or a github URL, it will display a warning on boot.
;; If you don't want the warning, then you shouldn't load this file in the first place.

(require 'package)
(unless package--initialized
  (package-initialize))

(defgroup novadb-bootstrap nil
  "Bootstrap logic for loading novadb."
  :group 'tools)

(defcustom novadb-dev-path nil
  "Local development checkout directory for novadb.

If nil, local development loading is disabled."
  :type '(choice
          (const :tag "Disabled" nil)
          directory)
  :group 'novadb-bootstrap)

(defcustom novadb-github-url nil
  "Git repository URL used to install novadb if not present.

If nil, automatic installation is disabled."
  :type '(choice
          (const :tag "Disabled" nil)
          string)
  :group 'novadb-bootstrap)


(defun novadb--dev-present-p ()
  "Return non-nil if a usable local dev checkout exists."
  (and (stringp novadb-dev-path)
       (file-directory-p novadb-dev-path)
       (file-exists-p (expand-file-name "novadb.el" novadb-dev-path))))

(defun novadb--installed-p ()
  "Return non-nil if novadb is already installed."
  (locate-library "novadb"))

(defun novadb--load-dev ()
  "Load novadb from local dev checkout."
  (add-to-list 'load-path novadb-dev-path)
  (require 'novadb)
  (message "novadb: loaded from local dev path: %s" novadb-dev-path))

(defun novadb--load-installed ()
  "Load novadb from installed package."
  (require 'novadb)
  (message "novadb: loaded installed package"))

(require 'subr-x) ;; string-trim, string-empty-p

(defun novadb--normalize-url (s)
  (when (stringp s)
    (replace-regexp-in-string "/+$" "" (string-trim s))))

(defun novadb--install-from-github ()
  "Install novadb from GitHub using package-vc (Emacs 29+)."
  (unless (fboundp 'package-vc-install)
    (error "package-vc-install not available (requires Emacs 29+)"))
  (let ((url (novadb--normalize-url novadb-github-url)))
    (unless (and (stringp url) (not (string-empty-p url)))
      (error "novadb-github-url is not set"))
    ;; IMPORTANT: provide the package name explicitly
    (package-vc-install `(novadb :url ,url))))

;; decide what to do

;; Clean up stale dev path if dev checkout is gone
(when (and (stringp novadb-dev-path)
           (not (novadb--dev-present-p)))
  (setq load-path (delete novadb-dev-path load-path)))

(cond
 ;; 1) Prefer local dev, if configured and present
 ;; ((and (stringp novadb-dev-path)
 ;;       (file-directory-p novadb-dev-path))
 ((novadb--dev-present-p)
  (add-to-list 'load-path novadb-dev-path)
  (require 'novadb)
  (message "novadb: loaded from dev path: %s" novadb-dev-path))

 ;; 2) Otherwise, if already installed, load the packaged version
 ((locate-library "novadb")
  (require 'novadb)
  (message "novadb: loaded installed package"))

 ;; 3) Otherwise, only *ask to download* if we actually have a URL
 ((and (stringp novadb-github-url)
       (not (string-empty-p (string-trim novadb-github-url))))
(when (yes-or-no-p (format "novadb not found. Install from %s ? " novadb-github-url))
  (novadb--install-from-github)
  (require 'novadb)
  (message "novadb: installed and loaded from GitHub")))

 ;; 4) Otherwise: nothing available, don’t prompt, just warn
 (t
  (display-warning
   'novadb
   "novadb: not available (no local dev path found, not installed, and no GitHub URL configured)."
   :warning)))

