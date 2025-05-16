;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Colin Uken"
      user-mail-address "cuken@cuken.dev")

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")
      auth-source-cache-expiry nil)

(setq auto-save-default t
      make-backup-files t)

(setq confirm-kill-emacs nil)

(setq which-key-idle-delay 0.2)

(use-package! gcmh
  :init
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 256 1024 1024))
  :config
  (gcmh-mode 1))

(setq gc-cons-threshold 200000000)

(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))

(set-email-account! "gmail"
  '((mu4e-sent-messages-behavior 'delete)
    (mu4e-sent-folder       . "/gmail/Sent")
    (mu4e-drafts-folder     . "/gmail/Drafts")
    (mu4e-trash-folder      . "/gmail/Trash")
    (mu4e-refile-folder     . "/gmail/Archive")
    (smtpmail-smtp-user     . "colin.uken@gmail.com")
    (smtpmail-smtp-server   . "smtp.gmail.com")
    (mu4e-compose-signature . "Colin Uken"))
  t)

(set-email-account! "icloud"
  '((mu4e-sent-folder       . "/icloud/Sent Messages")
    (mu4e-drafts-folder     . "/icloud/Drafts")
    (mu4e-trash-folder      . "/icloud/Deleted Messages")
    (mu4e-refile-folder     . "/icloud/Archive")
    (smtpmail-smtp-user     . "cuken@cuken.dev")
    (smtpmail-smtp-server   . "smtp.mail.me.com")
    (smtpmail-smtp-service  . 465)
    (mu4e-compose-signature . "Colin Uken"))
  t)

;;(setq +mu4e-gmail-accounts '(("colin.uken@gmail.com" . "/gmail")))

;; Enable automatic email retrieval and udpate.
(setq mu4e-update-interval (* 60 5))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;; (setq doom-font (font-spec :family "GeistMono Nerd Font" :size 15)
;;      doom-variable-pitch-font (font-spec :family "Alegreya" :size 18)
;;      doom-big-font (font-spec :family "GeistMono Nerd Font" :size 22))
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

(setq doom-theme `doom-one)
(setq display-line-numbers-type t)

(after! doom-themes
  (unless (display-graphic-p)
    (set-face-background 'default' "undefined")))

(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-major-mode-color-icon t)

(set-frame-parameter (selected-frame) 'alpha '(96 . 97))
(add-to-list 'default-frame-alist '(alpha . (96 . 97)))

(setq display-line-numbers-type 'relative)

(add-hook 'window-setup-hook #'toggle-frame-maximized)

(setq org-directory "~/org/")
(setq org-hide-emphasis-markers t)

(setq org-roam-directory "~/org-roam"
      org-noter-notes-search-path '("~/org-roam"))

(setq org-noter-highlight-selected-text t)

(defun org-syntax-convert-keyword-case-to-lower ()
  "Convert all #+KEYWORDS to #+keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (case-fold-search nil))
      (while (re-search-forward "^[ \t]*#\\+[A-Z_]+" nil t)
        (unless (string-match-p "RESULTS" (match-string 0))
          (replace-match (downcase (match-string 0)) t)
          (setq count (1+ count))))
    (message "Replaced %d occurances" count))))
