(defvar *emacs-load-start* (float-time))
(require 'cl)

(setq debug-on-error t)

;;;; General options -----------------------------------------------------------
(set-language-environment "UTF-8")      ; default is "English"

(setq inhibit-startup-message t
      initial-scratch-message nil)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)))

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
(global-linum-mode t)

(if (fboundp 'visual-line-mode)
    (add-hook 'text-mode-hook 'visual-line-mode)
  (add-hook 'text-mode-hook 'longlines-mode))
(setq longlines-wrap-follows-window-size t)

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(add-hook 'after-init-hook 'global-company-mode)

(setq magit-last-seen-setup-instructions "1.4.0")


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
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
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

;; path on Mac OS X missing ~/bin because it's not launched from bash
;; http://stackoverflow.com/a/15728130
(if (string-equal "darwin" (symbol-name system-type))
    (progn
      (setq explicit-bash-args (list "--login" "-i"))
      (let ((path-from-shell
             (shell-command-to-string "$SHELL -i -l -c 'echo $PATH'")))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator)))))

;; magit needs to use PuTTY/Pageant for remotes which connect via SSH
(if (string-equal "windows-nt" (symbol-name system-type))
    (setenv "GIT_SSH" "C:\\Program Files (x86)\\PuTTY\\plink.exe"))


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
  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode))


;;;; Clj-refactor --------------------------------------------------------------
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas/minor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-a")))


;;;; Flycheck-clojure ----------------------------------------------------------
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))


;;;; Browse Kill Ring ----------------------------------------------------------
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))


;;;; Keybindings ---------------------------------------------------------------
(global-unset-key "\C-x\C-c")
(global-unset-key "\C-t")

;; use C-x C-m (or C-c C-m if I miss!) for M-x
;; - from http://steve.yegge.googlepages.com/effective-emacs (item 2)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; use C-w to delete back a word to match the shortcut in the shell
;; Rebind kill-region (normally C-w) to C-x C-k (or C-c C-k if I miss)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-cb" 'bury-buffer)

;; searches might as well be regexps
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

(global-set-key "\C-x\C-b" 'electric-buffer-list)

(global-set-key "\M-/" 'company-complete)

(global-set-key "\C-m" 'newline-and-indent)

(global-set-key "\C-xw" 'delete-trailing-whitespace)

(global-set-key "\C-x|" 'align-regexp)

(global-set-key "\C-cg" 'magit-status)


;;;; Registers -----------------------------------------------------------------
;; usage:  C-x r j <register-identifier>  ('register jump')
;; from http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks
(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))
(set-register ?c `(file . ,(concat user-emacs-directory "cheat-sheet.txt")))
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
