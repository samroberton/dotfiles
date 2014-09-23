(defvar *emacs-load-start* (float-time))
(require 'cl)

(setq debug-on-error t)

;;;; General options -----------------------------------------------------------
(set-language-environment "UTF-8")      ; default is "English"

(setq inhibit-startup-message t
      initial-scratch-message nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indicate-empty-lines t
              show-trailing-whitespace t
              indent-tabs-mode nil
              tab-width 4
              require-final-newline t
              save-place t
              fill-column 80
              comment-column 40)

(column-number-mode t)
(line-number-mode t)
(mouse-wheel-mode t)
(show-paren-mode t)
(savehist-mode t)                       ; save minibuffer history

(add-hook 'text-mode-hook 'longlines-mode)
(setq longlines-wrap-follows-window-size t)

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;;; Auto-saves ----------------------------------------------------------------
(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq make-backup-files t
      vc-make-backup-files t            ; backup version-controlled files too
      backup-by-copying t               ; copy to backups dir, don't rename
                                        ; avoids clobbering symlinks
      backup-directory-alist `(("." . ,--backup-directory))
      version-control t                 ; create multiple numbered backups
      delete-old-versions t
      kept-new-versions 6               ; keep the most recent 6
      kept-old-versions 2)              ; keep the original 2

(setq mouse-yank-at-point t
      x-select-enable-clipboard t
      echo-keystrokes 0.1
      browse-url-generic-program (executable-find "firefox")
      browse-url-browser-function 'browse-url-generic
      vc-follow-symlinks t
      apropos-do-all t)


;;;; Emacs 24 Package setup ----------------------------------------------------
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)


;;;; Interactively do ----------------------------------------------------------
(ido-mode t)
(setq ido-case-fold  t                  ; case-insensitive
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-enable-flex-matching t
      ido-confirm-unique-completion t)


;;;; Uniquify ------------------------------------------------------------------
(require 'uniquify)                     ; make buffer names unique
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*") ; don't screw with special buffers


;;;; Paredit -------------------------------------------------------------------
(when (require 'paredit nil 'noerror)
  (autoload 'enable-paredit-mode "paredit" t)
  (eval-after-load 'paredit
    '(progn
       (define-key paredit-mode-map "{" 'paredit-open-curly)
       (define-key paredit-mode-map "}" 'paredit-close-curly)
       (define-key paredit-mode-map "\C-w" 'backward-kill-sexp)))

  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode))


;;;; Browse Kill Ring ----------------------------------------------------------
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))



;;;; Keybindings ---------------------------------------------------------------
(global-unset-key "\C-x\C-c")

;; use C-x C-m (or C-c C-m if I miss!) for M-x
;; - from http://steve.yegge.googlepages.com/effective-emacs (item 2)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; use C-w to delete back a word to match the shortcut in the shell
;; Rebind kill-region (normally C-w) to C-x C-k (or C-c C-k if I miss)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-c\C-g" 'what-line)
(global-set-key "\C-cg" 'goto-line)

(global-set-key "\C-cb" 'bury-buffer)

;; searches might as well be regexps
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

(global-set-key "\C-x\C-b" 'electric-buffer-list)
(global-set-key "\M-/" 'hippie-expand)

(global-set-key "\C-m" 'newline-and-indent)

(global-set-key "\C-xw" 'delete-trailing-whitespace)


;;;; Registers -----------------------------------------------------------------
;; usage:  C-x r j <register-identifier>  ('register jump')
;; from http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?c '(file . "~/.emacs.d/cheat-sheet.txt"))
(set-register ?t '(file . "~/todo.txt"))


;;;; Useful functions ----------------------------------------------------------
(defun fromdos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

(defun todos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))

;; multiline paragraph -> single line of text
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defalias 'qrr 'query-replace-regexp)


;;;; Emacsclient ---------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (add-hook 'after-init-hook 'server-start))


;;;; Themes --------------------------------------------------------------------
(with-demoted-errors
  (load-theme 'junio))


(message "init.el loaded in %fs" (- (float-time) *emacs-load-start*))
