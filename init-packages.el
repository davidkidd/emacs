;;; init-packages.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Set up package archives
;;; Code:

;; Basic setup
(require 'package)

;; TLS tweak
(defvar gnutls-algorithm-priority)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives
      '(("melpa"  . "https://melpa.org/packages/")
        ("org"    . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu"    . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; For custom package downloads
(defun ensure-vc-package (name repo)
  "Ensure NAME is installed from Git REPO."
  (unless (package-installed-p name)
    (package-vc-install repo)))

(require 'use-package)
(setq use-package-always-ensure t)
