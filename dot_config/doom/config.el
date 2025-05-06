;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Colin Uken"
      user-mail-address "cuken@cuken.dev")

(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo")
      auth-source-cache-expiry nil)

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

(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-major-mode-color-icon t)

(set-frame-parameter (selected-frame) 'alpha '(96 . 97))
(add-to-list 'default-frame-alist '(alpha . (96 . 97)))

(setq org-direectory "~/org/")
(setq org-hide-emphasis-markers t)

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
