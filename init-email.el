;;; init-email.el --- Generic Proton Bridge email setup for mu4e -*- lexical-binding: t; -*-

;;; Commentary:

;; Optional email module for one or more local mail accounts, intended for use
;; with Proton Bridge and/or other IMAP accounts synced by `mbsync` for use in
;; `mu4e`.
;;
;; This file is written to be safe for a public dotfiles repository, do not add
;; any private information here.
;;
;; Assumptions:
;; - mail is synced locally with `mbsync` into `~/Mail/ACCOUNT/`
;; - Proton Bridge IMAP is on 127.0.0.1:1143 using STARTTLS
;; - Proton Bridge SMTP is on 127.0.0.1:1025 using STARTTLS
;; - the Bridge-generated password is already stored in `~/.mbsyncrc`
;;
;; Enable it by adding `("init-email.el" t)` to `my/init-files`.
;; Then set the user-specific variables via Custom or a private, untracked file.
;;
;; Minimal private setup:
;;
;;   (setq my/email-enabled t
;;         my/email-address "user@example.com"
;;         my/email-bridge-user "user@example.com"
;;         my/email-full-name "User Name"
;;         my/email-maildir-root "/primary-account"
;;         my/email-mbsync-command "mbsync --pull-new primary-account:INBOX"
;;         my/email-mbsync-state-command
;;         "mbsync primary-account:INBOX,Trash"
;;         my/email-mbsync-full-command "mbsync primary-account"
;;         my/email-account-folders
;;         '(("/primary-account"
;;            (drafts . "/primary-account/Drafts")
;;            (sent . "/primary-account/Sent")
;;            (trash . "/primary-account/Trash")
;;            (refile . "/primary-account/Archive")))
;;         my/email-maildir-shortcuts
;;         '(("/primary-account/INBOX" . ?i)
;;           ("/primary-account/Sent" . ?s)
;;           ("/primary-account/Archive" . ?a))
;;         my/email-bookmarks
;;         '((:name "Unread inbox"
;;            :query "flag:unread AND maildir:/primary-account/INBOX"
;;            :key ?u)))
;;
;; If your local Bridge / Maildir setup differs from the defaults, also set:
;;
;;   my/email-imap-host             ; usually 127.0.0.1
;;   my/email-imap-port             ; often 1143
;;   my/email-smtp-host             ; usually 127.0.0.1
;;   my/email-smtp-port             ; often 1025
;;   my/email-maildir               ; usually ~/Mail
;;   my/email-maildir-root          ; usually /primary-account
;;   my/email-mbsync-command        ; fast new-mail check command for `U`
;;   my/email-mbsync-state-command  ; slower move/trash/flag sync command for `M-U`
;;   my/email-mbsync-full-command   ; full reconciliation command
;;   my/email-account-folders       ; per-account drafts/sent/trash/archive folders
;;   my/email-maildir-shortcuts     ; account-specific shortcut list
;;   my/email-bookmarks             ; account-specific bookmark list
;;   my/email-bridge-cert-file      ; exported Bridge cert path
;;   my/email-mbsyncrc-file         ; mbsync config containing Pass line
;;
;; Do these steps:
;;
;; 1. Proton Bridge is installed, logged in, and running.
;; 2. `mbsync` can already sync the chosen account successfully.
;; 3. `mu init` and `mu index` have already been run for the maildir.
;; 4. `~/.mbsyncrc` contains the Bridge-generated password on a `Pass` line.
;;
;; `~/.mbsyncrc` example/notes
;; ========================================
;;
;; Proton Bridge account example:
;;
;;   IMAPAccount primary-bridge
;;   Host 127.0.0.1
;;   Port 1143
;;   User YOUR_PROTON_BRIDGE_USERNAME
;;   Pass YOUR_PROTON_BRIDGE_PASSWORD
;;   SSLType STARTTLS
;;   CertificateFile /home/YOU/.mbsync-proton-cert.pem
;;
;;   IMAPStore primary-bridge-remote
;;   Account primary-bridge
;;
;;   MaildirStore primary-bridge-local
;;   SubFolders Verbatim
;;   Path /home/YOU/Mail/primary-bridge/
;;   Inbox /home/YOU/Mail/primary-bridge/INBOX
;;
;;   Channel primary-bridge
;;   Far :primary-bridge-remote:
;;   Near :primary-bridge-local:
;;   Patterns INBOX Archive Drafts Sent Spam Trash Folders/*
;;   Create Both
;;   Remove Both
;;   Expunge None
;;   Sync All
;;   SyncState *
;;
;; Gmail account example:
;;
;;   IMAPAccount secondary-gmail
;;   Host imap.gmail.com
;;   Port 993
;;   User YOUR_GMAIL_ADDRESS
;;   Pass "YOUR_GMAIL_APP_PASSWORD"
;;   SSLType IMAPS
;;
;;   IMAPStore secondary-gmail-remote
;;   Account secondary-gmail
;;
;;   MaildirStore secondary-gmail-local
;;   SubFolders Verbatim
;;   Path /home/YOU/Mail/secondary-gmail/
;;   Inbox /home/YOU/Mail/secondary-gmail/INBOX
;;
;;   Channel secondary-gmail
;;   Far :secondary-gmail-remote:
;;   Near :secondary-gmail-local:
;;   Patterns INBOX "[Gmail]/All Mail" "[Gmail]/Sent Mail" "[Gmail]/Drafts" "[Gmail]/Trash" "[Gmail]/Spam"
;;   Create Both
;;   Remove Both
;;   Expunge None
;;   Sync All
;;   SyncState *
;;
;; Duplicate that Gmail block for any other Gmail / Workspace account, changing
;; the account, store, path, channel, and user names.
;;
;; Ubuntu / AppArmor note:
;;
;; Some Ubuntu setups confine `mbsync` with AppArmor and require a local rule so
;; it can read the exported Proton Bridge cert and create Maildir directories.
;; A local override may need entries like:
;;
;;   owner @{HOME}/.mbsync-proton-cert.pem r,
;;   owner @{HOME}/Mail/**/ rw,
;;
;; followed by reloading the profile.
;;
;; First-run checklist:
;;
;; 1. Install `isync`, `mu`, and `mu4e`.
;; 2. Install Proton Bridge, sign in, and export its certificate.
;; 3. Recreate Gmail app passwords (in Gmail's account settings, search for "App passwords").
;; 4. Rebuild `~/.mbsyncrc` from the private file/template.
;; 5. Restore any needed AppArmor override and reload it.
;; 6. Create local maildirs under `~/Mail/`.
;; 7. Run `mbsync --list CHANNEL` for each account.
;; 8. Run first syncs with `mbsync CHANNEL...`.
;; 9. Run `mu init` and `mu index`.
;; 10. Start Emacs and load the private `defcustom` values.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'smtpmail)
(require 'auth-source)

(defvar message-send-mail-function)
(defvar gnutls-trustfiles)
(defvar mu4e-maildir)
(defvar mu4e-get-mail-command)
(defvar mu4e-update-interval)
(defvar mu4e-change-filenames-when-moving)
(defvar mu4e-drafts-folder)
(defvar mu4e-sent-folder)
(defvar mu4e-trash-folder)
(defvar mu4e-refile-folder)
(defvar mu4e-sent-messages-behavior)
(defvar mu4e-maildir-shortcuts)
(defvar mu4e-bookmarks)
(defvar mu4e-headers-fields)
(defvar mu4e-main-mode-map)
(defvar mu4e-headers-mode-map)
(defvar mu4e-view-mode-map)
(declare-function mu4e "ext:mu4e")
(declare-function mu4e-update-mail-and-index "mu4e-update")

(defgroup my/email nil
  "Personal email settings for Proton Bridge and mu4e."
  :group 'mail)

;; Setup notes for the custom variables below:
;;
;; Required for a working account:
;; - `my/email-enabled`
;; - `my/email-address`
;; - `my/email-bridge-user`
;;
;; Commonly overridden, depending on provider and local layout:
;; - `my/email-imap-host`
;; - `my/email-imap-port`
;; - `my/email-smtp-host`
;; - `my/email-smtp-port`
;; - `my/email-maildir`
;; - `my/email-maildir-root`
;; - `my/email-mbsync-command`
;; - `my/email-mbsync-state-command`
;; - `my/email-mbsync-full-command`
;; - `my/email-account-folders`
;; - `my/email-maildir-shortcuts`
;; - `my/email-bookmarks`
;; - `my/email-headers-fields`
;; - `my/email-bridge-cert-file`
;; - `my/email-mbsyncrc-file`
;; - `my/email-authinfo-file`
;;
;; In a public repo, keep the defaults generic here and set the actual values in
;; a private file or via Custom.

(defcustom my/email-enabled nil
  "When non-nil, enable this email configuration.

Keep this nil in a public repo if you prefer, and enable it in a private file
or through Custom."
  :type 'boolean
  :group 'my/email)

(defcustom my/email-address nil
  "Primary email address for this account.

Example: `user@example.com`."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-full-name nil
  "Display name for outgoing mail.

If nil or empty, leave `user-full-name` unchanged."
  :type '(choice (const :tag "Use existing user-full-name" nil) string)
  :group 'my/email)

(defcustom my/email-imap-host "127.0.0.1"
  "Proton Bridge IMAP host."
  :type 'string
  :group 'my/email)

(defcustom my/email-imap-port 1143
  "Proton Bridge IMAP port."
  :type 'integer
  :group 'my/email)

(defcustom my/email-smtp-host "127.0.0.1"
  "Proton Bridge SMTP host."
  :type 'string
  :group 'my/email)

(defcustom my/email-smtp-port 1025
  "Proton Bridge SMTP port."
  :type 'integer
  :group 'my/email)

(defcustom my/email-bridge-user nil
  "Username shown by Proton Bridge for IMAP/SMTP auth.

For many setups this is the same as the email address, but it is kept separate
here so the public config does not assume that."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-maildir (expand-file-name "~/Mail")
  "Top-level maildir used by mu and mu4e."
  :type 'directory
  :group 'my/email)

(defcustom my/email-mbsync-command nil
  "Fast command `mu4e` should run for routine new-mail checks.

Keep this lightweight, typically a pull of inboxes or other folders where new
mail arrives.  Leave nil to avoid publishing account-specific channel names in
this file."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-mbsync-state-command nil
  "Command for syncing slower state changes such as moves, trash, and flags.

Use this for bidirectional reconciliation that is too slow for the normal
new-mail check but faster than a full all-folders sync."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-mbsync-full-command nil
  "Full `mbsync` command for explicit reconciliation of all maildirs.

This can include All Mail, Sent, Trash, flags, and deletions that are too slow
for the normal new-mail check.  Keep account-specific channel names private."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-maildir-root "/primary-account"
  "Root maildir prefix for the default account as seen by mu4e.

Use a generic public default here and override it privately."
  :type 'string
  :group 'my/email)

(defcustom my/email-account-folders nil
  "Per-account special folders for multi-account `mu4e` setups.

Each entry has the form:

  (MAILDIR-ROOT
   (drafts . PATH-TO-DRAFTS)
   (sent   . PATH-TO-SENT)
   (trash  . PATH-TO-TRASH)
   (refile . PATH-TO-ARCHIVE))

When nil, `my/email-maildir-root` falls back to standard folder names under the
account root: `Drafts`, `Sent`, `Trash`, and `Archive`.  For providers whose
folder names differ, set overrides privately so delete/refile operations target
the correct remote folders."
  :type '(choice (const :tag "Use primary-account defaults" nil)
                 (repeat sexp))
  :group 'my/email)

(defcustom my/email-maildir-shortcuts nil
  "Maildir shortcuts for `mu4e`.

Keep account-specific maildir names in private configuration, such as
`custom/custom.el`."
  :type '(choice (const :tag "Use generated default" nil)
                 (alist :key-type (string :tag "Maildir")
                        :value-type character
                        :tag "Alist (old format)")
                 (repeat (plist
                          :key-type (choice (const :tag "Maildir" :maildir)
                                            (const :tag "Shortcut" :key)
                                            (const :tag "Display name" :name))
                          :tag "Plist shortcut")))
  :group 'my/email)

(defcustom my/email-bookmarks nil
  "Bookmark list for `mu4e`.

Keep account-specific bookmark queries in private configuration, such as
`custom/custom.el`."
  :type '(choice (const :tag "Use generated default" nil)
                 (repeat sexp))
  :group 'my/email)

(defcustom my/email-headers-fields
  '((:human-date . 12)
    (:flags . 6)
    (:maildir . 32)
    (:from . 22)
    (:subject . nil))
  "Columns displayed in the `mu4e` headers view.

The `:maildir` column makes the source account and folder explicit, for example
`/primary-account/INBOX` or `/secondary-account/Archive`."
  :type '(repeat (cons symbol
                       (choice integer
                               (const :tag "Unrestricted width" nil))))
  :group 'my/email)

(defcustom my/email-bridge-cert-file (expand-file-name "~/.config/protonmail/cert.pem")
  "Path to the exported Proton Bridge certificate."
  :type 'file
  :group 'my/email)

(defcustom my/email-mbsyncrc-file (expand-file-name "~/.mbsyncrc")
  "Path to the `mbsync` configuration file containing the Bridge password."
  :type 'file
  :group 'my/email)

(defcustom my/email-authinfo-file (expand-file-name "~/.emacs.d/proton-bridge.authinfo")
  "Auto-generated authinfo file for SMTP.

This is generated from the password stored in `my/email-mbsyncrc-file` so
Emacs can send mail without prompting every time."
  :type 'file
  :group 'my/email)

(defun my/email--folder (name &optional root)
  "Return a mu4e folder path for NAME under ROOT or `my/email-maildir-root`."
  (concat (directory-file-name (or root my/email-maildir-root)) "/" name))

(defun my/email--message-root (&optional msg)
  "Return the top-level maildir root for MSG.

If MSG is nil or does not have a maildir, fall back to
`my/email-maildir-root`."
  (let ((maildir (and msg
                      (if (fboundp 'mu4e-message-field)
                          (mu4e-message-field msg :maildir)
                        (plist-get msg :maildir)))))
    (if (and (stringp maildir)
             (string-match "^/[^/]+" maildir))
        (match-string 0 maildir)
      my/email-maildir-root)))

(defun my/email--account-folder-overrides (&optional root)
  "Return folder overrides for ROOT or the primary account."
  (or (cdr (assoc (or root my/email-maildir-root) my/email-account-folders))
      (cdr (assoc my/email-maildir-root my/email-account-folders))))

(defun my/email--folder-for-role (role &optional msg)
  "Return the `mu4e` folder for ROLE, using MSG to infer the account.

ROLE should be one of `drafts`, `sent`, `trash`, or `refile`."
  (let* ((root (my/email--message-root msg))
         (overrides (my/email--account-folder-overrides root))
         (folder (alist-get role overrides)))
    (or folder
        (my/email--folder
         (pcase role
           ('drafts "Drafts")
           ('sent "Sent")
           ('trash "Trash")
           ('refile "Archive")
           (_ (error "Unsupported email folder role: %S" role)))
         root))))

(defun my/email--default-maildir-shortcuts ()
  "Return a minimal default `mu4e` shortcut list."
  `((,(my/email--folder "INBOX") . ?i)
    (,(my/email--folder "Sent") . ?s)
    (,(my/email--folder "Archive") . ?a)))

(defun my/email--resolve-maildir-shortcuts ()
  "Return the configured `mu4e` maildir shortcuts."
  (or my/email-maildir-shortcuts
      (my/email--default-maildir-shortcuts)))

(defun my/email--default-bookmarks ()
  "Return a minimal default `mu4e` bookmark list."
  `((:name "Unread inbox"
     :query ,(format "flag:unread AND maildir:%s"
                     (my/email--folder "INBOX"))
     :key ?u)))

(defun my/email--configured-p ()
  "Return non-nil when enough information exists to enable email setup."
  (and my/email-enabled
       (stringp my/email-address)
       (not (string-empty-p my/email-address))
       (stringp my/email-bridge-user)
       (not (string-empty-p my/email-bridge-user))))

(defun my/email--mbsync-password ()
  "Extract the Proton Bridge password from `my/email-mbsyncrc-file`.

Accept either an unquoted or quoted `Pass` value.  Return nil if no
usable `Pass` line can be found."
  (when (file-readable-p my/email-mbsyncrc-file)
    (with-temp-buffer
      (insert-file-contents my/email-mbsyncrc-file)
      (goto-char (point-min))
      (when (re-search-forward "^Pass[[:space:]]+\\(?:\"\\([^\"]+\\)\"\\|\\([^[:space:]]+\\)\\)$" nil t)
        (or (match-string-no-properties 1)
            (match-string-no-properties 2))))))

(defun my/email--write-authinfo ()
  "Generate `my/email-authinfo-file` from the Bridge password.

Returns the path written, or nil if no password could be found."
  (when-let ((password (my/email--mbsync-password)))
    (make-directory (file-name-directory my/email-authinfo-file) t)
    (with-temp-file my/email-authinfo-file
      (insert (format "machine %s login %s port %d password %s\n"
                      my/email-smtp-host
                      my/email-bridge-user
                      my/email-smtp-port
                      password)))
    (set-file-modes my/email-authinfo-file #o600)
    my/email-authinfo-file))

(defun my/email--configure-auth-sources ()
  "Ensure auth-source can find the Bridge SMTP password."
  (when-let ((authinfo (my/email--write-authinfo)))
    (setq auth-sources
          (delete-dups
           (append (list authinfo)
                   (if (listp auth-sources)
                       auth-sources
                     (list auth-sources)))))
    (when (fboundp 'auth-source-forget-all-cached)
      (auth-source-forget-all-cached))))

(defun my/email--configure-common ()
  "Apply mail settings shared across mu4e and message sending."
  (setq user-mail-address my/email-address)
  (when (and (stringp my/email-full-name)
             (not (string-empty-p my/email-full-name)))
    (setq user-full-name my/email-full-name))
  (setq mail-user-agent 'mu4e-user-agent
        read-mail-command #'mu4e
        send-mail-function #'smtpmail-send-it
        message-send-mail-function #'smtpmail-send-it
        smtpmail-smtp-server my/email-smtp-host
        smtpmail-smtp-service my/email-smtp-port
        smtpmail-stream-type 'starttls
        smtpmail-smtp-user my/email-bridge-user
        smtpmail-debug-info nil)
  ;; Proton Bridge presents a local self-signed cert; trust the exported cert.
  ;; `gnutls-trustfiles` is not guaranteed to be bound during early init.
  (when (and (boundp 'gnutls-trustfiles)
             (stringp my/email-bridge-cert-file)
             (file-readable-p my/email-bridge-cert-file))
    (add-to-list 'gnutls-trustfiles my/email-bridge-cert-file))
  (my/email--configure-auth-sources))

(defun my/email-sync-state (&optional run-in-background)
  "Run the slower state sync and refresh the mu index.

With prefix argument RUN-IN-BACKGROUND, hide the update buffer.  This is for
moves, trash, and similar changes that are slower than `my/email-mbsync-command`
but smaller than a full all-folders sync.  By default this command is bound to
`M-U` in the main, headers, and view buffers."
  (interactive "P")
  (unless (and (stringp my/email-mbsync-state-command)
               (not (string-empty-p my/email-mbsync-state-command)))
    (user-error "`my/email-mbsync-state-command` is not configured"))
  (let ((mu4e-get-mail-command my/email-mbsync-state-command))
    (mu4e-update-mail-and-index run-in-background)))

(defun my/email-full-update (&optional run-in-background)
  "Run the full mail sync and refresh the mu index.

With prefix argument RUN-IN-BACKGROUND, hide the update buffer.  The normal
`mu4e` update command continues to use the faster `my/email-mbsync-command`."
  (interactive "P")
  (unless (and (stringp my/email-mbsync-full-command)
               (not (string-empty-p my/email-mbsync-full-command)))
    (user-error "`my/email-mbsync-full-command` is not configured"))
  (let ((mu4e-get-mail-command my/email-mbsync-full-command))
    (mu4e-update-mail-and-index run-in-background)))

(defun my/email--configure-mu4e ()
  "Apply mu4e-specific settings for the configured maildir.

Automatic `mu4e` polling is disabled here so manual `U` checks stay responsive."
  (when (require 'mu4e nil 'noerror)
    (setq mu4e-maildir my/email-maildir
          mu4e-update-interval nil
          mu4e-change-filenames-when-moving t
          mu4e-drafts-folder (lambda (msg) (my/email--folder-for-role 'drafts msg))
          mu4e-sent-folder (lambda (msg) (my/email--folder-for-role 'sent msg))
          mu4e-trash-folder (lambda (msg) (my/email--folder-for-role 'trash msg))
          mu4e-refile-folder (lambda (msg) (my/email--folder-for-role 'refile msg))
          ;; Proton/Bridge already stores the sent copy server-side.
          mu4e-sent-messages-behavior 'delete
          mu4e-headers-fields my/email-headers-fields
          mu4e-maildir-shortcuts
          (my/email--resolve-maildir-shortcuts)
          mu4e-bookmarks
          (or my/email-bookmarks
              (my/email--default-bookmarks)))
    (when (and (stringp my/email-mbsync-command)
               (not (string-empty-p my/email-mbsync-command)))
      (setq mu4e-get-mail-command my/email-mbsync-command))
    (when (boundp 'mu4e-main-mode-map)
      (define-key mu4e-main-mode-map (kbd "M-U") #'my/email-sync-state))
    (when (boundp 'mu4e-headers-mode-map)
      (define-key mu4e-headers-mode-map (kbd "M-U") #'my/email-sync-state))
    (when (boundp 'mu4e-view-mode-map)
      (define-key mu4e-view-mode-map (kbd "M-U") #'my/email-sync-state))))

(when (my/email--configured-p)
  (my/email--configure-common)
  (my/email--configure-mu4e))

(provide 'init-email)
;;; init-email.el ends here
