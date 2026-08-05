;;; init-email.el --- Multi-account mu4e and mbsync setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Public, provider-aware email module for local Maildirs synchronized by
;; `mbsync` and indexed by `mu` for use in `mu4e`.  It supports ordinary
;; folder-based accounts, Proton Bridge, and Gmail accounts that use All Mail
;; as the sole ordinary local corpus alongside operational Trash, Drafts, and
;; Spam mailboxes.
;;
;; Keep all addresses, account names, passwords, and machine-specific paths in
;; Custom or a private, untracked file.  The examples here use reserved
;; `.invalid` addresses and generic channel names.
;;
;; Core assumptions
;; ================================
;;
;; - `mbsync` stores mail under `~/Mail/ACCOUNT/`.
;; - `mu init` and `mu index` have initialized that top-level Maildir.
;; - every account is described in `my/email-contexts`.
;; - the first context is the initial/default context.
;; - each context supplies its real identity and SMTP values; the first context
;;   provides harmless startup defaults before Mu4e selects a context.
;; - SMTP authinfo generation reads simple literal `User` and `Pass` directives
;;   from matching `IMAPAccount` blocks in `my/email-mbsyncrc-file`.
;;
;; The authinfo helper deliberately supports only a restricted mbsyncrc subset:
;; conventional account names and simple quoted or unquoted `User` and `Pass`
;; values without escape processing or trailing comments.  It does not evaluate
;; `PassCmd`, OAuth, credential agents, or encrypted password stores.  It assumes
;; an account's IMAP and SMTP passwords are the same and writes a second
;; plaintext credential file with mode 0600.  That generated file is replaced
;; in full whenever this module configures auth sources, so use a dedicated file
;; and never point `my/email-authinfo-file` at a hand-maintained authinfo file.
;; Keep both files out of version control and private backups.  Providers with
;; separate SMTP credentials need a different `auth-source` setup.
;;
;; Sanitized multi-account setup
;; ================================
;;
;; Enable this module through `my/init-files`, then set private values like:
;;
;;   (setq my/email-enabled t
;;         my/email-address "user@primary.example.invalid"
;;         my/email-bridge-user "bridge-user@example.invalid"
;;         my/email-full-name "Example User"
;;         my/email-maildir-root "/primary"
;;         my/email-mbsync-command
;;         "mbsync --pull-new primary-main:INBOX gmail-all-mail"
;;         my/email-mbsync-full-command "mbsync primary gmail-example"
;;         my/email-account-folders
;;         '(("/primary"
;;            (drafts . "/primary/Drafts")
;;            (sent . "/primary/Sent")
;;            (trash . "/primary/Trash")
;;            (refile . "/primary/Archive"))
;;           ("/gmail-example"
;;            (drafts . "/gmail-example/[Gmail]/Drafts")
;;            (trash . "/gmail-example/[Gmail]/Trash")))
;;         my/email-contexts
;;         '((:name "Primary"
;;            :maildir-root "/primary"
;;            :address "user@primary.example.invalid"
;;            :full-name "Example User"
;;            :smtp-server "127.0.0.1"
;;            :smtp-port 1025
;;            :smtp-user "bridge-user@example.invalid"
;;            :smtp-stream-type starttls
;;            :mbsync-account "primary")
;;           (:name "Gmail example"
;;            :maildir-root "/gmail-example"
;;            :address "user@gmail.example.invalid"
;;            :full-name "Example User"
;;            :smtp-server "smtp.gmail.com"
;;            :smtp-port 587
;;            :smtp-user "user@gmail.example.invalid"
;;            :smtp-stream-type starttls
;;            :mbsync-account "gmail-example"
;;            :all-mail-mailbox "[Gmail]/All Mail"
;;            :archive-style gmail))
;;         my/email-bookmarks
;;         '((:name "Unread mail"
;;            :query "flag:unread AND (maildir:\"/primary/INBOX\" OR maildir:\"/gmail-example/[Gmail]/All Mail\")"
;;            :key ?u)))
;;
;; `:maildir-root` must match the first component of the maildirs indexed by mu.
;; `:mbsync-account` is currently dual-purpose: it names both the `IMAPAccount`
;; block used for credentials and the Group or Channel run by `Ua`.  Use the
;; same identifier for the IMAPAccount and account Group.
;;
;; If `:all-mail-mailbox` is present, the account's mbsync Group must include a
;; separately restricted aggregate channel like `gmail-all-mail` in the example
;; below.  `Ua` runs the whole Group so its ordinary and aggregate channels each
;; retain their own Sync and Expunge policies.
;;
;; Canonical mail models
;; ================================
;;
;; Mu4e can project searches across all mail that has already been downloaded;
;; it cannot find messages absent from local Maildirs.
;;
;; For an ordinary folder-based provider, synchronize the real Inbox, Archive,
;; Sent, Drafts, Trash, Spam, and intentional custom folders.  Do not mirror
;; provider-internal rescue mailboxes or an aggregate All Mail mailbox: doing so
;; duplicates or repeatedly restores messages that are not normal server mail,
;; increases indexing work, and makes moves and flags ambiguous.
;;
;; Gmail is different: this configuration intentionally does not synchronize
;; Gmail's Inbox or Sent label projections.  `[Gmail]/All Mail` is the sole
;; ordinary local corpus, while Drafts, Trash, and Spam remain separate because
;; they have operational behavior or are excluded from the ordinary corpus.
;; This removes persistent Inbox/Sent duplication at the cost of not representing
;; those Gmail labels in Mu4e.  Use unread status as the incoming-work queue and
;; search `from:` fields when looking for sent messages.  Duplicate suppression
;; remains enabled for transient Draft/All Mail overlap.
;;
;; On a context with `:archive-style gmail`, `r` is unavailable because every
;; ordinary Gmail message is already in canonical All Mail and there is no local
;; Inbox label to remove.  Folder-based accounts continue to refile normally to
;; their Archive folder; refile marks need `x` before synchronization.
;;
;; Lowercase `d` marks a message to move to the configured Trash folder.
;; Uppercase `D` and the Delete keys use Mu4e's permanent-delete mark instead;
;; neither kind of mark takes effect until `x` executes it.
;;
;; Mu4e normally adds the Maildir `T` flag while moving a message to Trash.
;; mbsync treats Deleted/Trashed as a synchronized flag, so the destination can
;; be considered already deleted and may be removed instead of retained as a
;; normal Trash copy.  This module sets `mu4e-trash-without-flag`, making
;; lowercase `d` a plain move into the account's real Trash mailbox; an account
;; sync can then propagate that move.
;;
;; An interrupted mbsync run can rarely leave an aggregate All Mail file whose
;; local UID was never committed to `.mbsyncstate`.  Such a file is invisible to
;; later synchronization and otherwise remains as a false local duplicate even
;; after Gmail has moved the real message to Trash.  After the interrupted sync
;; has stopped, run `M-x my/email-prune-mbsync-orphans`.  The built-in Elisp
;; repair checks only configured aggregate Maildirs, copies untracked files to
;; `my/email-mbsync-orphan-quarantine-directory` outside `~/Mail`, verifies each
;; copy, and only then removes the indexed original.  It never contacts a server;
;; run `mu index` afterward if anything was quarantined.
;;
;; `~/.mbsyncrc` examples
;; ================================
;;
;; Proton Bridge / folder-based account:
;;
;;   IMAPAccount primary
;;   Host 127.0.0.1
;;   Port 1143
;;   User BRIDGE-USERNAME
;;   Pass BRIDGE-PASSWORD
;;   SSLType STARTTLS
;;   CertificateFile /home/YOU/.mbsync-proton-cert.pem
;;
;;   IMAPStore primary-remote
;;   Account primary
;;   PathDelimiter /
;;
;;   MaildirStore primary-local
;;   SubFolders Verbatim
;;   Path /home/YOU/Mail/primary/
;;   Inbox /home/YOU/Mail/primary/INBOX
;;
;;   Channel primary-main
;;   Far :primary-remote:
;;   Near :primary-local:
;;   Patterns INBOX Archive Drafts Sent Spam Trash Folders/*
;;   Create Both
;;   Remove Both
;;   Expunge Both
;;   Sync All
;;   SyncState *
;;
;;   Group primary
;;   Channels primary-main
;;
;; Gmail account with canonical All Mail:
;;
;; Before relying on this deletion model, disable Gmail IMAP auto-expunge and
;; verify deletion/expunge behavior with disposable messages.  isync's Gmail
;; recommendations require auto-expunge to be disabled; provider settings UIs
;; and labels can change, so verify the effective behavior rather than relying
;; only on a particular settings-page name.
;;
;;   IMAPAccount gmail-example
;;   Host imap.gmail.com
;;   Port 993
;;   User USER@GMAIL.EXAMPLE.INVALID
;;   Pass "PROVIDER-SUPPORTED-NONINTERACTIVE-PASSWORD"
;;   SSLType IMAPS
;;
;;   IMAPStore gmail-remote
;;   Account gmail-example
;;
;;   MaildirStore gmail-local
;;   SubFolders Verbatim
;;   Path /home/YOU/Mail/gmail-example/
;;
;;   Channel gmail-operational
;;   Far :gmail-remote:
;;   Near :gmail-local:
;;   Patterns "[Gmail]/Drafts" "[Gmail]/Trash" "[Gmail]/Spam"
;;   Create Both
;;   Remove Both
;;   Expunge Both
;;   Sync All
;;   SyncState *
;;
;;   Channel gmail-all-mail
;;   Far :gmail-remote:
;;   Near :gmail-local:
;;   Patterns "[Gmail]/All Mail"
;;   Create Near
;;   Remove Near
;;   Expunge Near
;;   Sync Pull PushFlags
;;   SyncState *
;;
;;   Group gmail-example
;;   Channels gmail-operational gmail-all-mail
;;
;; In isync 1.4.4, `Sync Pull PushFlags` pulls every class of remote change but
;; pushes only local flag changes.  It intentionally does not upload new local
;; files or propagate removed local files to remote All Mail.  In mbsync terms,
;; Deleted/Trashed is also a flag, so `PushFlags` can still propagate it; this is
;; one reason the Trash workflow above avoids adding Maildir `T`.  `Expunge
;; Near` may clean the local mirror but cannot expunge the remote aggregate
;; mailbox.
;;
;; `Expunge` is independent of `Sync`: running a channel may expunge messages
;; already marked deleted on the configured side, even when the command line
;; says `--pull-new`.  `Expunge Both` keeps an ordinary bidirectional mirror
;; clean on both sides, but its Far half is permanently destructive and belongs
;; only on tested ordinary reconciliation channels.  Using only `Expunge Far`
;; can retain local `T`-flagged tombstones after remote removal.  Keep aggregate
;; All Mail at `Expunge Near` or `Expunge None`; never override a mixed account
;; group with a global `--expunge-far` command-line option.
;;
;; Update commands
;; ================================
;;
;; - `Uf` runs `my/email-mbsync-command`, conventionally a lightweight selected
;;   mailbox fetch.  Its exact behavior comes from that command and the channel
;;   policies it references; `--pull-new` still inherits Create/Remove/Expunge.
;; - `Ua` asks for one context and runs its `:mbsync-account` Group/Channel.  A
;;   Gmail group combines bidirectional Drafts/Trash/Spam with restricted All Mail.
;; - `UA` runs `my/email-mbsync-full-command`.  It covers every account only if
;;   the private command names every required Group/Channel.
;; - `Up` runs the local-only Elisp orphan repair after an interrupted sync.
;;
;; Gmail deletion workflow: press lowercase `d`, press `x` to execute the move,
;; then run `Ua` or `UA`.  The message should remain only in Trash.  If mbsync
;; was interrupted, finish a successful sync and then run
;; `my/email-prune-mbsync-orphans` before reindexing.  `r` is intentionally
;; unavailable for Gmail.
;;
;; Maildir navigation
;; ================================
;;
;; Press `j` in Mu4e and select a Maildir through normal minibuffer completion.
;; Vertico displays the complete list, so this module intentionally disables
;; Mu4e's separate two-key Maildir shortcuts and removes their dashboard section.
;;
;; Address completion
;; ================================
;;
;; In a Mu4e compose buffer, type part of a name or address in `To`, `Cc`,
;; `Bcc`, `Reply-To`, `From`, or `Sender`, then press `TAB`.  Mu4e supplies
;; candidates from contacts discovered in indexed mail; Consult routes the
;; completion through the minibuffer and Vertico displays it.  Outside those
;; fields, `TAB` keeps normal `message-mode` behavior.
;;
;; Address completion requires `mu4e-compose-complete-addresses` to remain
;; non-nil.  If an expected contact is absent, run `mu index` and inspect
;; `mu4e-compose-complete-only-after`, which limits how old a message may be for
;; its contacts to be offered.  `mu init --my-address` identifies your own
;; addresses; indexing mail is what populates contact candidates.
;;
;; Certificates and credentials
;; ================================
;;
;; `.mbsyncrc`'s `CertificateFile` controls trust for mbsync's IMAP connection.
;; `my/email-bridge-cert-file` is separately added to Emacs's global
;; `gnutls-trustfiles` list.  This is not SMTP-scoped and can affect other TLS
;; connections made by Emacs, so add only a certificate whose fingerprint you
;; have independently verified.  Configuring one trust path does not configure
;; the other.
;;
;; This module globally sets `mu4e-sent-messages-behavior` to `delete`, so every
;; configured SMTP service must save sent messages server-side.  A provider
;; that does not save a server copy needs different or per-context behavior or
;; the outgoing local copy will not be filed in Sent by Mu4e.
;;
;; For Gmail, configure a provider-supported noninteractive credential.  An
;; app password may be available when account security policy permits it; this
;; module does not implement OAuth.
;;
;; Ubuntu / AppArmor note:
;; Some Ubuntu installations confine `mbsync`.  A local profile override may
;; need read access to the exported Bridge certificate and write access under
;; the Maildir, followed by an AppArmor profile reload.
;;
;; First-run checklist
;; ================================
;;
;; 1. Install `isync`, `mu`, and `mu4e`.
;; 2. Configure each provider and any required Bridge certificate.
;; 3. Create a private `~/.mbsyncrc` and protect it with mode 0600.
;; 4. Create Maildir roots under `~/Mail/`.
;; 5. Run `mbsync --list CHANNEL-OR-GROUP` and inspect every mailbox mapping.
;; 6. Test each ordinary and aggregate channel separately before using Groups,
;;    for example `mbsync -V gmail-operational:"[Gmail]/Trash"` and
;;    `mbsync -V gmail-all-mail:"[Gmail]/All Mail"` with disposable messages.
;; 7. Run initial syncs, then initialize mu with every personal address (repeat
;;    `--my-address` as needed) and index the store:
;;
;;      mu init --maildir="$HOME/Mail" \
;;        --my-address='first@example.invalid' \
;;        --my-address='second@example.invalid'
;;      mu index
;;
;; 8. Set the private Custom values, load this module, and test `Ua` and `UA`
;;    before relying on the fast update command.

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
(defvar mu4e-trash-without-flag)
(defvar mu4e-maildir-shortcuts)
(defvar mu4e-bookmarks)
(defvar mu4e-headers-fields)
(defvar mu4e-search-skip-duplicates)
(defvar mu4e-completing-read-function)
(defvar mu4e-compose-context-policy)
(defvar mu4e-context-policy)
(defvar mu4e-contexts)
(defvar mu4e-main-buffer-name)
(defvar mu4e-main-mode-map)
(defvar mu4e-headers-mode-map)
(defvar mu4e-view-mode-map)
(declare-function consult-completion-in-region "consult")
(declare-function make-mu4e-context "mu4e-context")
(declare-function mu4e "ext:mu4e")
(declare-function mu4e--main-action "mu4e-main")
(declare-function message-tab "message")
(declare-function mu4e-context-name "mu4e-context")
(declare-function mu4e-headers-mark-and-next "mu4e-headers")
(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-update-mail-and-index "mu4e-update")
(declare-function mu4e-view-mark-for-refile "mu4e-view")

(defgroup my/email nil
  "Multi-account settings for mbsync, mu4e, SMTP, and Proton Bridge."
  :group 'mail)

;; Setup notes for the custom variables below:
;;
;; Required for this multi-account setup:
;; - `my/email-enabled`
;; - `my/email-contexts`, with the required keys documented below
;;
;; `my/email-address` and `my/email-bridge-user` remain optional compatibility
;; defaults; a complete first context supplies the startup identity and SMTP.
;;
;; Commonly overridden, depending on provider and local layout:
;; - `my/email-smtp-host`
;; - `my/email-smtp-port`
;; - `my/email-maildir`
;; - `my/email-maildir-root`
;; - `my/email-mbsync-command`
;; - `my/email-mbsync-full-command`
;; - `my/email-mbsync-orphan-quarantine-directory`
;; - `my/email-account-folders`
;; - `my/email-contexts`
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
  "Bootstrap/default email address for common configuration.

This is an optional compatibility fallback.  In a context-driven setup, the
first context supplies the startup address and every context supplies its own
From address through `:address`.  Example: `user@example.invalid`."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-full-name nil
  "Display name for outgoing mail.

If nil or empty, leave `user-full-name` unchanged."
  :type '(choice (const :tag "Use existing user-full-name" nil) string)
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
  "Bootstrap/default SMTP username used by common configuration.

This is an optional compatibility fallback.  For Proton Bridge it is the
Bridge-provided login; complete contexts should set `:smtp-user` directly."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-maildir (expand-file-name "~/Mail")
  "Top-level maildir used by mu and mu4e."
  :type 'directory
  :group 'my/email)

(defcustom my/email-mbsync-command nil
  "Lightweight command run by the normal Mu4e update and `Uf`.

A typical value pulls new messages from selected arrival sources: ordinary
Inboxes for folder-based providers and canonical All Mail for Gmail.  Its exact
behavior is determined by the command and referenced mbsync channels.  In isync
1.4.4, `--pull-new` restricts Sync operations but still inherits each channel's
Create, Remove, and Expunge policy.  Leave nil to disable external fetching."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-mbsync-full-command nil
  "Command run by `UA` for configured full reconciliation.

This is conventionally an `mbsync` command naming every account Group.  Its
actual scope and safety are entirely determined by the command and referenced
channel policies; do not apply a global `--expunge-far` override to Groups that
contain a restricted All Mail channel."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'my/email)

(defcustom my/email-mbsync-orphan-quarantine-directory
  (expand-file-name "~/mail-repair-backups/mbsync-all-mail-orphans")
  "Directory outside `my/email-maildir` used to preserve stale aggregate files."
  :type 'directory
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
the correct remote folders.

The `refile` value is Mu4e's ordinary refile destination.  A context using
`:archive-style gmail` needs only `drafts` and `trash` overrides here: sent mail
is saved server-side and appears through All Mail, while `r` is unavailable
because no local Inbox projection is synchronized."
  :type '(choice (const :tag "Use primary-account defaults" nil)
                 (repeat sexp))
  :group 'my/email)

(defcustom my/email-contexts nil
  "Account definitions used to build automatic `mu4e` contexts.

Each entry is a plist with these keys:

  :name                     context name shown by Mu4e
  :maildir-root             first maildir component as seen by Mu4e
  :address                  From address
  :full-name                optional display name
  :smtp-server              SMTP host
  :smtp-port                SMTP port
  :smtp-user                SMTP login; defaults to :address
  :smtp-stream-type         `starttls`, `ssl`, or another smtpmail value
  :mbsync-account           IMAPAccount credential block and Ua sync target
  :all-mail-mailbox         exact mailbox relative to the context root
  :archive-style            optional provider behavior; currently `gmail`

`:mbsync-account` currently serves two namespaces: it identifies the
`IMAPAccount` block parsed for literal User/Pass credentials and the Group or
Channel passed to mbsync by `Ua`.  Use the same identifier for both.

For Gmail, configure `:all-mail-mailbox` as `[Gmail]/All Mail` and route it to a
dedicated channel using `Sync Pull PushFlags` with no remote expunge.  Treat it
as the sole ordinary local corpus; synchronize only Drafts, Trash, and Spam in
the companion operational channel.  Do not configure aggregate All Mail for
ordinary folder-based providers when their real folders already contain the
complete corpus.

Replies and forwards select the first context whose maildir root matches the
source message.  New messages have no source account, so Mu4e asks which
context to use.  The first context is initially selected.  Keep addresses and
account names in private configuration."
  :type '(choice (const :tag "No contexts" nil)
                 (repeat sexp))
  :group 'my/email)

(defvar my/email-current-maildir-root nil
  "Maildir root belonging to the currently selected mu4e context.")

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
  "Path to the private mbsync configuration containing literal credentials.

The deliberately restricted parser supports conventional `IMAPAccount`
sections and simple quoted or unquoted literal `User` and `Pass` values.  It
does not process escapes or trailing comments and does not evaluate `PassCmd`,
OAuth, credential agents, or encrypted stores."
  :type 'file
  :group 'my/email)

(defcustom my/email-authinfo-file (expand-file-name "~/.emacs.d/proton-bridge.authinfo")
  "Auto-generated plaintext authinfo file for SMTP.

Usable literal passwords are copied from `my/email-mbsyncrc-file`; the file is
written with mode 0600 and replaced in full whenever auth sources are
configured.  Use only a dedicated generated path, not a hand-maintained
`authinfo` file with unrelated entries.  This duplicates reusable credentials
and assumes the IMAP and SMTP passwords are identical, so keep it private and
untracked."
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
      (or my/email-current-maildir-root
          my/email-maildir-root))))

(defun my/email--account-folder-overrides (&optional root)
  "Return folder overrides for ROOT or the primary account when ROOT is nil."
  (cdr (assoc (or root my/email-maildir-root)
              my/email-account-folders)))

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

(defun my/email--account-for-root (root)
  "Return the configured account whose maildir root is ROOT."
  (seq-find (lambda (account)
              (string= (plist-get account :maildir-root) root))
            my/email-contexts))

(defun my/email--gmail-archive-style-p (msg)
  "Return non-nil when MSG belongs to a Gmail-style account."
  (let* ((root (my/email--message-root msg))
         (account (my/email--account-for-root root)))
    (eq (plist-get account :archive-style) 'gmail)))

(defun my/email-headers-archive ()
  "Archive the message at point using provider-appropriate semantics.

Gmail accounts use All Mail as their only ordinary local corpus, so there is no
local Inbox label to remove and `r` is unavailable.  Folder-based providers use
ordinary Mu4e refile behavior."
  (interactive)
  (let ((msg (mu4e-message-at-point)))
    (if (my/email--gmail-archive-style-p msg)
        (user-error "Gmail mail is already in canonical All Mail; use d to trash")
      (mu4e-headers-mark-and-next 'refile))))

(defun my/email-view-archive ()
  "Archive the viewed message using provider-appropriate semantics."
  (interactive)
  (let ((msg (mu4e-message-at-point)))
    (if (my/email--gmail-archive-style-p msg)
        (user-error "Gmail mail is already in canonical All Mail; use d to trash")
      (mu4e-view-mark-for-refile))))

(defun my/email--mu-query-string (value)
  "Return VALUE quoted as one string literal in a `mu` query."
  (unless (stringp value)
    (error "Mu query value must be a string: %S" value))
  (concat "\""
          (replace-regexp-in-string "[\\\"]" "\\\\&" value)
          "\""))

(defun my/email--maildir-query (maildir)
  "Return an exact `mu` maildir query for MAILDIR."
  (concat "maildir:" (my/email--mu-query-string maildir)))

(defun my/email--default-bookmarks ()
  "Return a minimal default `mu4e` bookmark list."
  `((:name "Unread inbox"
     :query ,(format "flag:unread AND %s"
                     (my/email--maildir-query
                      (my/email--folder "INBOX")))
     :key ?u)))

(defun my/email--configured-p ()
  "Return non-nil when context-driven email setup is enabled."
  (and my/email-enabled
       (consp my/email-contexts)))

(defun my/email--parse-simple-config-value (value)
  "Return a supported simple mbsync VALUE, or nil.

Accept one unquoted non-whitespace token or one double-quoted value without
escapes.  Reject trailing comments and other mbsync syntax rather than
silently turning it into an incorrect credential."
  (let ((value (string-trim value)))
    (cond
     ((string-match "\\`\"\\([^\"\\\\]*\\)\"\\'" value)
      (match-string-no-properties 1 value))
     ((string-match-p "\\`[^[:space:]\"\\\\]+\\'" value)
      value))))

(defun my/email--mbsync-account-credentials (account-name)
  "Return literal User and Pass values for mbsync ACCOUNT-NAME.

The result is a plist with :user and :password, or nil when the IMAPAccount
section lacks either supported literal directive.  Keywords are matched
case-insensitively.  `PassCmd` and complex quoting are not evaluated."
  (when (and (stringp account-name)
             (not (string-empty-p account-name))
             (file-readable-p my/email-mbsyncrc-file))
    (with-temp-buffer
      (insert-file-contents my/email-mbsyncrc-file)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward
               (format "^IMAPAccount[[:space:]]+%s[[:space:]]*$"
                       (regexp-quote account-name))
               nil t)
          (let* ((start (line-beginning-position 2))
                 ;; Blank lines are only formatting in mbsyncrc, not section
                 ;; boundaries.  Stop at the next account declaration so a
                 ;; malformed account can never borrow another account's
                 ;; credentials.
                 (end (save-excursion
                        (goto-char start)
                        (if (re-search-forward
                             "^IMAPAccount[[:space:]]+" nil t)
                            (line-beginning-position)
                          (point-max))))
                 user password duplicate)
            (save-restriction
              (narrow-to-region start end)
              (goto-char (point-min))
              (when (re-search-forward
                     "^[[:space:]]*User[[:space:]]+\\(.+\\)$" nil t)
                (setq user
                      (my/email--parse-simple-config-value
                       (match-string-no-properties 1)))
                (when (re-search-forward
                       "^[[:space:]]*User[[:space:]]+" nil t)
                  (setq duplicate t)))
              (goto-char (point-min))
              (when (re-search-forward
                     "^[[:space:]]*Pass[[:space:]]+\\(.+\\)$" nil t)
                (setq password
                      (my/email--parse-simple-config-value
                       (match-string-no-properties 1)))
                (when (re-search-forward
                       "^[[:space:]]*Pass[[:space:]]+" nil t)
                  (setq duplicate t))))
            (when (and user password (not duplicate))
              (list :user user :password password))))))))

(defun my/email--authinfo-value (value)
  "Return VALUE safely quoted for an authinfo file."
  (prin1-to-string (format "%s" value)))

(defun my/email--authinfo-line (account)
  "Return an authinfo line for ACCOUNT, or nil without credentials."
  (let* ((mbsync-account (plist-get account :mbsync-account))
         (credentials (and mbsync-account
                           (my/email--mbsync-account-credentials
                            mbsync-account)))
         (password (plist-get credentials :password))
         (login (or (plist-get account :smtp-user)
                    (plist-get credentials :user)
                    (plist-get account :address)))
         (server (plist-get account :smtp-server))
         (port (plist-get account :smtp-port)))
    (when (and password login server (integerp port))
      (format "machine %s login %s port %d password %s\n"
              (my/email--authinfo-value server)
              (my/email--authinfo-value login)
              port
              (my/email--authinfo-value password)))))

(defun my/email--write-authinfo ()
  "Generate `my/email-authinfo-file` for supported SMTP accounts.

Literal passwords are read from corresponding IMAPAccount blocks in
`my/email-mbsyncrc-file` and are assumed to work for SMTP.  Write atomically
with mode 0600 and return the path.  If no supported credentials are found,
remove an obsolete managed file and return nil."
  (let* ((destination (expand-file-name my/email-authinfo-file))
         (directory (file-name-directory destination))
         (lines (delq nil
                      (mapcar #'my/email--authinfo-line
                              my/email-contexts))))
    (when (file-symlink-p destination)
      (error "Refusing symlink for generated email authinfo: %s" destination))
    (when (and (file-exists-p destination)
               (not (file-regular-p destination)))
      (error "Generated email authinfo is not a regular file: %s" destination))
    (if (not lines)
        (progn
          (when (file-exists-p destination)
            (delete-file destination))
          nil)
      (make-directory directory t)
      (let ((temporary (make-temp-file
                        (expand-file-name ".email-authinfo-" directory))))
        (unwind-protect
            (progn
              (with-temp-file temporary
                (set-file-modes temporary #o600)
                (insert (mapconcat #'identity lines "")))
              (rename-file temporary destination t)
              (set-file-modes destination #o600))
          (when (file-exists-p temporary)
            (delete-file temporary))))
      destination)))

(defun my/email--configure-auth-sources ()
  "Refresh the managed authinfo entry in `auth-sources`."
  (let* ((managed (expand-file-name my/email-authinfo-file))
         (authinfo (my/email--write-authinfo))
         (existing (if (listp auth-sources)
                       auth-sources
                     (list auth-sources))))
    (setq auth-sources
          (delete-dups
           (delq nil
                 (append (and authinfo (list authinfo))
                         (delete managed existing)))))
    (when (fboundp 'auth-source-forget-all-cached)
      (auth-source-forget-all-cached))))

(defun my/email--configure-common ()
  "Apply mail settings shared across mu4e and message sending."
  (let* ((primary (car my/email-contexts))
         (address (or (plist-get primary :address) my/email-address))
         (full-name (or (plist-get primary :full-name) my/email-full-name))
         (smtp-user (or (plist-get primary :smtp-user)
                        address my/email-bridge-user)))
    (setq user-mail-address address)
    (when (and (stringp full-name)
               (not (string-empty-p full-name)))
      (setq user-full-name full-name))
    (setq mail-user-agent 'mu4e-user-agent
          read-mail-command #'mu4e
          send-mail-function #'smtpmail-send-it
          message-send-mail-function #'smtpmail-send-it
          smtpmail-smtp-server (or (plist-get primary :smtp-server)
                                   my/email-smtp-host)
          smtpmail-smtp-service (or (plist-get primary :smtp-port)
                                    my/email-smtp-port)
          smtpmail-stream-type (or (plist-get primary :smtp-stream-type)
                                   'starttls)
          smtpmail-smtp-user smtp-user
          smtpmail-debug-info nil))
  ;; Proton Bridge presents a local self-signed cert; trust the exported cert.
  ;; `gnutls-trustfiles` is not guaranteed to be bound during early init.
  (when (and (boundp 'gnutls-trustfiles)
             (stringp my/email-bridge-cert-file)
             (file-readable-p my/email-bridge-cert-file))
    (add-to-list 'gnutls-trustfiles my/email-bridge-cert-file))
  (my/email--configure-auth-sources))

(defun my/email--aggregate-maildir-paths (accounts)
  "Return local aggregate Maildir paths configured by ACCOUNTS."
  (delq nil
        (mapcar
         (lambda (account)
           (when-let ((mailbox (plist-get account :all-mail-mailbox)))
             (expand-file-name
              (concat (string-remove-prefix
                       "/" (plist-get account :maildir-root))
                      "/" mailbox)
              my/email-maildir)))
         accounts)))

(defun my/email--mbsync-tracked-near-uids (maildir)
  "Return Near UIDs tracked by MAILDIR's `.mbsyncstate`.

Refuse missing, empty, or visibly pending state rather than guessing."
  (let ((state (expand-file-name ".mbsyncstate" maildir))
        (pending (expand-file-name ".mbsyncstate.new" maildir))
        (uids (make-hash-table :test #'eql)))
    (when (file-exists-p pending)
      (user-error "Refusing orphan scan with pending state: %s" pending))
    (unless (file-readable-p state)
      (user-error "Missing readable mbsync state: %s" state))
    (with-temp-buffer
      (insert-file-contents state)
      (goto-char (point-min))
      (while (re-search-forward
              "^[-0-9]+[[:space:]]+\\([0-9]+\\)[[:space:]]" nil t)
        (puthash (string-to-number (match-string 1)) t uids)))
    (when (= (hash-table-count uids) 0)
      (user-error "No tracked Near UIDs in mbsync state: %s" state))
    uids))

(defun my/email--mbsync-maildir-file-uid (file)
  "Return mbsync's assigned local UID from FILE, or nil."
  (when (string-match ",U=\\([0-9]+\\)\\(?:[:,]\\)" (file-name-nondirectory file))
    (string-to-number (match-string 1 (file-name-nondirectory file)))))

(defun my/email--mbsync-orphans (maildir)
  "Return UID-assigned files in MAILDIR absent from `.mbsyncstate`."
  (let ((tracked (my/email--mbsync-tracked-near-uids maildir))
        orphans)
    (dolist (subdirectory '("cur" "new"))
      (let ((directory (expand-file-name subdirectory maildir)))
        (when (file-directory-p directory)
          (dolist (file (directory-files directory t directory-files-no-dot-files-regexp))
            (when-let ((uid (and (file-regular-p file)
                                 (my/email--mbsync-maildir-file-uid file))))
              (unless (gethash uid tracked)
                (push file orphans)))))))
    (nreverse orphans)))

(defun my/email--unique-file-name (file)
  "Return FILE or a numbered variant that does not already exist."
  (let ((candidate file)
        (counter 0))
    (while (file-exists-p candidate)
      (setq counter (1+ counter)
            candidate (format "%s.%d" file counter)))
    candidate))

(defun my/email-prune-mbsync-orphans (&optional accounts)
  "Quarantine untracked aggregate-mailbox files for ACCOUNTS.

ACCOUNTS defaults to `my/email-contexts`.  This is a conservative repair command
for use after an interrupted mbsync run has finished.  It scans only configured
aggregate Maildirs, copies each orphan outside `my/email-maildir`, verifies the
copy size, and only then removes the original.  It never contacts a server."
  (interactive)
  (let* ((accounts (or accounts my/email-contexts))
         (mail-root (file-name-as-directory (expand-file-name my/email-maildir)))
         (quarantine-root
          (file-name-as-directory
           (expand-file-name my/email-mbsync-orphan-quarantine-directory)))
         (maildirs (my/email--aggregate-maildir-paths accounts))
         (run-directory
          (expand-file-name (format-time-string "%Y%m%d-%H%M%S%z")
                            quarantine-root))
         plan)
    (when (or (string= mail-root quarantine-root)
              (file-in-directory-p quarantine-root mail-root)
              (file-in-directory-p mail-root quarantine-root))
      (user-error "Mbsync orphan quarantine must not overlap the indexed Maildir"))
    (unless maildirs
      (user-error "No aggregate Maildirs are configured"))
    ;; Complete validation and planning before changing any mailbox.
    (dolist (maildir maildirs)
      (let ((maildir (file-name-as-directory (expand-file-name maildir))))
        (unless (and (file-directory-p maildir)
                     (file-in-directory-p maildir mail-root)
                     (not (string= maildir mail-root)))
          (user-error "Unsafe or missing aggregate Maildir: %s" maildir))
        (dolist (source (my/email--mbsync-orphans maildir))
          (let* ((relative (file-relative-name source mail-root))
                 (destination
                  (my/email--unique-file-name
                   (expand-file-name relative run-directory))))
            (push (cons source destination) plan)))))
    (setq plan (nreverse plan))
    (dolist (move plan)
      (let ((source (car move))
            (destination (cdr move)))
        (make-directory (file-name-directory destination) t)
        (copy-file source destination nil t t)
        (unless (= (file-attribute-size (file-attributes source))
                   (file-attribute-size (file-attributes destination)))
          (delete-file destination)
          (error "Orphan backup verification failed: %s" source))
        (delete-file source)))
    (if plan
        (message "Quarantined %d untracked mbsync file%s under %s; run mu index"
                 (length plan) (if (= (length plan) 1) "" "s") run-directory)
      (message "No untracked mbsync files found in aggregate Maildirs"))
    (length plan)))

(defun my/email-full-update (&optional run-in-background)
  "Run `my/email-mbsync-full-command` and refresh the mu index.

The configured command conventionally names every account Group, but its actual
scope and channel policies are user-defined.  With prefix argument
RUN-IN-BACKGROUND, hide the update buffer.  The normal Mu4e update command uses
`my/email-mbsync-command` instead."
  (interactive "P")
  (unless (and (stringp my/email-mbsync-full-command)
               (not (string-empty-p my/email-mbsync-full-command)))
    (user-error "`my/email-mbsync-full-command` is not configured"))
  (let ((mu4e-get-mail-command my/email-mbsync-full-command))
    (mu4e-update-mail-and-index run-in-background)))

(defun my/email--message-in-root-p (msg root)
  "Return non-nil when MSG belongs to maildir ROOT."
  (when-let ((maildir (and msg (mu4e-message-field msg :maildir))))
    (or (string= maildir (directory-file-name root))
        (string-prefix-p (concat (directory-file-name root) "/")
                         maildir))))

(defun my/email--context-vars (account)
  "Return mu4e context variable settings for ACCOUNT."
  (let* ((address (plist-get account :address))
         (full-name (plist-get account :full-name))
         (smtp-user (or (plist-get account :smtp-user) address)))
    (append
     `((user-mail-address . ,address)
       (smtpmail-smtp-server . ,(plist-get account :smtp-server))
       (smtpmail-smtp-service . ,(plist-get account :smtp-port))
       (smtpmail-smtp-user . ,smtp-user)
       (smtpmail-stream-type . ,(or (plist-get account :smtp-stream-type)
                                    'starttls))
       (my/email-current-maildir-root . ,(plist-get account :maildir-root)))
     (when (and (stringp full-name) (not (string-empty-p full-name)))
       `((user-full-name . ,full-name))))))

(defun my/email--make-context (account)
  "Build a `mu4e-context` from ACCOUNT, validating operational settings."
  (let ((name (plist-get account :name))
        (root (plist-get account :maildir-root))
        (mbsync-account (plist-get account :mbsync-account))
        (all-mail-mailbox (plist-get account :all-mail-mailbox))
        (archive-style (plist-get account :archive-style)))
    (unless (and (stringp name) (not (string-empty-p name))
                 (stringp root)
                 (string-match-p "\\`/[^/]+\\'" root)
                 (stringp (plist-get account :address))
                 (not (string-empty-p (plist-get account :address)))
                 (stringp (plist-get account :smtp-server))
                 (not (string-empty-p (plist-get account :smtp-server)))
                 (integerp (plist-get account :smtp-port))
                 (<= 1 (plist-get account :smtp-port) 65535)
                 (memq (or (plist-get account :smtp-stream-type) 'starttls)
                       '(starttls ssl plain))
                 (memq archive-style '(nil gmail))
                 (stringp mbsync-account)
                 (not (string-empty-p mbsync-account)))
      (error "Invalid or incomplete email context definition: %S" account))
    (when (and (eq archive-style 'gmail)
               (not (and (stringp all-mail-mailbox)
                         (not (string-empty-p all-mail-mailbox)))))
      (error "Gmail archive context needs an All Mail mailbox: %S" account))
    (when all-mail-mailbox
      (unless (and (stringp all-mail-mailbox)
                   (not (string-empty-p all-mail-mailbox))
                   (not (file-name-absolute-p all-mail-mailbox))
                   (not (seq-some (lambda (component)
                                    (member component '("" "." "..")))
                                  (split-string all-mail-mailbox "/"))))
        (error "Unsafe All Mail mailbox: %S" all-mail-mailbox)))
    (make-mu4e-context
     :name name
     :match-func (lambda (msg)
                   (my/email--message-in-root-p msg root))
     :vars (my/email--context-vars account))))

(defun my/email--ask-context-with-completion (prompt)
  "Select a mu4e context with PROMPT using standard completion."
  (unless mu4e-contexts
    (user-error "No email contexts are configured"))
  (let* ((choices (mapcar (lambda (context)
                            (cons (mu4e-context-name context) context))
                          mu4e-contexts))
         (name (completing-read prompt choices nil t)))
    (or (cdr (assoc name choices))
        (user-error "No such email context: %s" name))))

(defun my/email--configure-contexts ()
  "Build and configure mu4e contexts from `my/email-contexts`."
  (when my/email-contexts
    (dolist (key '(:name :maildir-root :mbsync-account))
      (let ((values (mapcar (lambda (account) (plist-get account key))
                            my/email-contexts)))
        (unless (= (length values) (length (delete-dups (copy-sequence values))))
          (error "Duplicate %s in email contexts" key))))
    (setq mu4e-contexts (mapcar #'my/email--make-context my/email-contexts)
          ;; Enter mu4e quietly with the primary (first) account selected.
          mu4e-context-policy 'pick-first
          ;; Replies match automatically; a brand-new message asks because
          ;; there is no parent message whose maildir can identify an account.
          mu4e-compose-context-policy 'ask)
    (unless (advice-member-p #'my/email--ask-context-with-completion
                             'mu4e--context-ask-user)
      (advice-add 'mu4e--context-ask-user :override
                  #'my/email--ask-context-with-completion))))

(defun my/email--ask-account-with-completion (prompt)
  "Select an account with PROMPT using standard completion."
  (unless my/email-contexts
    (user-error "No email accounts are configured"))
  (let* ((choices
          (mapcar (lambda (account)
                    (cons (plist-get account :name) account))
                  my/email-contexts))
         (name (completing-read prompt choices nil t)))
    (or (cdr (assoc name choices))
        (user-error "No such email account: %s" name))))

(defun my/email-sync-account (account &optional run-in-background)
  "Sync ACCOUNT and refresh the mu index.

ACCOUNT is one entry from `my/email-contexts`.  Each member channel retains its
own Sync and Expunge policy, so a Group may combine bidirectional ordinary
mailboxes with a restricted aggregate mirror.  With prefix argument
RUN-IN-BACKGROUND, hide the update buffer."
  (interactive
   (list (my/email--ask-account-with-completion "Sync account: ")
         current-prefix-arg))
  (let ((channel (plist-get account :mbsync-account)))
    (unless (and (stringp channel) (not (string-empty-p channel)))
      (user-error "Account has no mbsync Group/Channel target: %S" account))
    (let ((mu4e-get-mail-command
           (format "mbsync %s" (shell-quote-argument channel))))
      (mu4e-update-mail-and-index run-in-background))))

(defvar my/email-update-prefix-map (make-sparse-keymap)
  "Prefix map for email update commands on the mu4e dashboard.")

(define-key my/email-update-prefix-map (kbd "f") #'mu4e-update-mail-and-index)
(define-key my/email-update-prefix-map (kbd "a") #'my/email-sync-account)
(define-key my/email-update-prefix-map (kbd "A") #'my/email-full-update)
(define-key my/email-update-prefix-map (kbd "p") #'my/email-prune-mbsync-orphans)

(defun my/email--main-action-command-at-point ()
  "Return the command attached to the dashboard action at point."
  (when-let ((map (get-text-property (point) 'keymap)))
    (lookup-key map (kbd "RET"))))

(defun my/email--main-delete-actions (commands)
  "Delete dashboard action lines whose commands are in COMMANDS."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((start (line-beginning-position))
          (end (line-beginning-position 2)))
      (if (memq (my/email--main-action-command-at-point) commands)
          (delete-region start end)
        (forward-line 1)))))

(defun my/email--customize-main-dashboard (&rest _)
  "Remove the Maildirs section and add grouped update actions."
  (when-let ((buffer (get-buffer mu4e-main-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (my/email--main-delete-actions
           '(mu4e-update-mail-and-index
             mu4e-news
             mu4e-about
             mu4e-display-manual
             mu4e-quit))
          (goto-char (point-min))
          (when (search-forward "  Maildirs\n\n" nil t)
            (let ((start (match-beginning 0)))
              (when (search-forward "  Misc\n\n" nil t)
                (delete-region start (match-beginning 0)))))
          (goto-char (point-min))
          (when (search-forward "  Misc\n\n" nil t)
            (goto-char (match-beginning 0))
            (insert
             (propertize "  Updates\n\n" 'face 'mu4e-title-face)
             (mu4e--main-action
              "\t* [@] Fast update — run configured lightweight sync\n"
              #'mu4e-update-mail-and-index "Uf")
             (mu4e--main-action
              "\t* [@] Account sync — run one account Group/Channel\n"
              #'my/email-sync-account "Ua")
             (mu4e--main-action
              "\t* [@] Full update — run configured full reconciliation\n"
              #'my/email-full-update "UA")
             (mu4e--main-action
              "\t* [@] Repair — quarantine interrupted-sync All Mail orphans\n"
              #'my/email-prune-mbsync-orphans "Up")
             "\n")))))))

(defun my/email-compose-tab ()
  "Run `completion-at-point`, falling back to normal message TAB behavior.

Mu4e's completion function supplies contacts in To, Cc, Bcc, Reply-To, From,
and Sender headers.  Outside applicable completion fields, preserve
`message-tab` behavior."
  (interactive)
  (unless (completion-at-point)
    (message-tab)))

(defun my/email--configure-compose-completion ()
  "Use Consult's minibuffer UI for Mu4e completion when available."
  (when (require 'consult nil t)
    (setq-local completion-in-region-function
                #'consult-completion-in-region))
  (local-set-key (kbd "TAB") #'my/email-compose-tab)
  (local-set-key (kbd "<tab>") #'my/email-compose-tab))

(defun my/email--configure-mu4e ()
  "Apply mu4e-specific settings for the configured maildir.

Automatic `mu4e` polling is disabled here so manual `U` checks stay responsive."
  (when (require 'mu4e nil 'noerror)
    ;; Use ordinary `completing-read` throughout mu4e so the configured
    ;; minibuffer frontend (Vertico in `init-general.el`) handles candidates.
    (setq mu4e-completing-read-function #'completing-read)
    (add-hook 'mu4e-compose-mode-hook
              #'my/email--configure-compose-completion)
    (unless (advice-member-p #'my/email--customize-main-dashboard
                             'mu4e--main-redraw)
      (advice-add 'mu4e--main-redraw :after
                  #'my/email--customize-main-dashboard))
    (when (boundp 'mu4e-main-mode-map)
      (define-key mu4e-main-mode-map (kbd "U")
                  my/email-update-prefix-map))
    (when (boundp 'mu4e-headers-mode-map)
      (define-key mu4e-headers-mode-map (kbd "r")
                  #'my/email-headers-archive))
    (when (boundp 'mu4e-view-mode-map)
      (define-key mu4e-view-mode-map (kbd "r")
                  #'my/email-view-archive))
    (my/email--configure-contexts)
    (setq mu4e-maildir my/email-maildir
          mu4e-update-interval nil
          mu4e-change-filenames-when-moving t
          ;; Moving a message to a real Trash folder is sufficient.  Mu4e's
          ;; default Maildir T flag lets mbsync treat the destination as already
          ;; deleted, so it may be removed instead of retained in remote Trash.
          mu4e-trash-without-flag t
          mu4e-drafts-folder (lambda (msg) (my/email--folder-for-role 'drafts msg))
          mu4e-sent-folder (lambda (msg) (my/email--folder-for-role 'sent msg))
          mu4e-trash-folder (lambda (msg) (my/email--folder-for-role 'trash msg))
          mu4e-refile-folder (lambda (msg) (my/email--folder-for-role 'refile msg))
          ;; All configured SMTP providers are expected to save sent mail
          ;; server-side.  This is global; a provider that does not save a copy
          ;; needs different or per-context handling.
          mu4e-sent-messages-behavior 'delete
          mu4e-headers-fields my/email-headers-fields
          ;; Drafts can transiently share a Message-ID with their All Mail
          ;; projection; broad searches should still display one logical item.
          mu4e-search-skip-duplicates t
          ;; Keep `j` as a completion-driven Maildir picker without a parallel
          ;; set of fragile two-key shortcuts or a dashboard shortcut section.
          mu4e-maildir-shortcuts nil
          mu4e-bookmarks
          (or my/email-bookmarks
              (my/email--default-bookmarks)))
    (when (and (stringp my/email-mbsync-command)
               (not (string-empty-p my/email-mbsync-command)))
      (setq mu4e-get-mail-command my/email-mbsync-command))))

(when (my/email--configured-p)
  (my/email--configure-common)
  (my/email--configure-mu4e))

(provide 'init-email)
;;; init-email.el ends here
