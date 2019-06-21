(defvar *emacs-load-start* (float-time))

;; Set to `t' to debug problems loading init.el
(setq debug-on-error nil)



;;;; Keybindings ---------------------------------------------------------------

(global-unset-key (kbd "C-x c"))        ; let's not accidentally kill Emacs
(global-unset-key (kbd "C-x C-c"))      ; let's not accidentally kill Emacs
(global-unset-key (kbd "C-t"))          ; transposing characters not useful

(global-set-key (kbd "C-c f") 'toggle-frame-fullscreen)

;; 'C-h C' describes coding system
(global-set-key (kbd "C-c C") 'set-buffer-file-coding-system)


(global-set-key (kbd "C-w") 'backward-kill-word) ; make C-w match readline behaviour
(global-set-key (kbd "C-x C-k") 'kill-region)

(global-set-key "\C-cb" 'bury-buffer)

;; Searches might as well be regexps.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(global-set-key (kbd "M-/") 'company-complete)

(global-set-key (kbd "C-x w") 'delete-trailing-whitespace)

(global-set-key (kbd "C-x |") 'align-regexp)



;;;; Registers -----------------------------------------------------------------

;; usage:  C-x r j <register-identifier>  ('register jump')
;; from http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks
(set-register ?i `(file . ,(concat user-emacs-directory "init.el")))
(set-register ?c `(file . ,(concat user-emacs-directory "cheat-sheet.txt")))
(set-register ?t '(file . "~/todo.txt"))



;;;; General options -----------------------------------------------------------

(require 'cl)                           ; provides `case'

(set-language-environment "UTF-8")      ; default is "English"
(prefer-coding-system 'utf-8-unix)

(setq inhibit-startup-message t
      initial-scratch-message nil)

(setq-default indicate-empty-lines     t
              show-trailing-whitespace t
              indent-tabs-mode         nil
              tab-width                4
              require-final-newline    t
              save-place               t
              fill-column              80
              comment-column           40)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode t)
(line-number-mode t)
(global-linum-mode t)
(mouse-wheel-mode t)
(show-paren-mode t)
(savehist-mode t)                       ; save minibuffer history

(add-hook 'text-mode-hook 'visual-line-mode)

(setq mouse-yank-at-point                 t
      select-enable-clipboard             t
      select-enable-primary               t
      save-interprogram-paste-before-kill t
      echo-keystrokes                     0.1
      vc-follow-symlinks                  t)

;; Avoid trying to ping some machine in Hong Kong just because you typed
;; something.hk: https://github.com/technomancy/emacs-starter-kit/issues/39
(setq-default ffap-machine-p-known 'reject)

(defalias 'yes-or-no-p 'y-or-n-p)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Use Source Code Pro when available:
;; https://github.com/adobe-fonts/source-code-pro
(when (x-list-fonts "Source Code Pro Light")
  (set-frame-font "Source Code Pro Light")
  (set-face-attribute 'default nil :height (case system-type
                                             (darwin     120)
                                             (windows-nt 120)
                                             (otherwise  100))))

(setq-default wdired-allow-to-change-permissions t)



;;;; Emacsclient ---------------------------------------------------------------

(require 'server)
(unless (server-running-p)
  (add-hook 'after-init-hook 'server-start))



;;;; Platform-specific customisations ------------------------------------------

;; PATH on Mac OS X missing ~/bin because it's not launched from bash
;; http://stackoverflow.com/a/15728130
(when (equal system-type 'darwin)
  (setq-default explicit-bash-args (list "--login" "-i"))
  (let ((path-from-shell
         (shell-command-to-string "$SHELL -i -l -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; magit needs to use PuTTY/Pageant for remotes which connect via SSH
(when (equal system-type 'windows-nt)
  (setenv "GIT_SSH" "C:\\Program Files (x86)\\PuTTY\\plink.exe"))



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



;;;; ediff ---------------------------------------------------------------------

(setq ediff-window-setup-function #'ediff-setup-windows-plain
      ediff-split-window-function #'split-window-vertically)


;;;; Emacs Package setup -------------------------------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))



;;;; Packages ------------------------------------------------------------------

(use-package windmove
  :ensure t
  :config
  ;; shift + arrows to move between windows
  (windmove-default-keybindings)
  (setq windmove-wrap-around t))


(use-package helm
  :ensure t
  :bind   (("M-x"     . helm-M-x)
           ("M-y"     . helm-show-kill-ring)
           ("C-x b"   . helm-mini)
           ("C-x C-f" . helm-find-files)
           ("C-c h o" . helm-occur)
           ("C-c h b" . helm-resume)

           :map helm-map
           ("[tab]" . helm-execute-persistent-action)
           ("C-i"   . helm-execute-persistent-action)
           ("C-z"   . helm-select-action))

  :config
  (setq helm-split-window-inside-p        t ; open helm buffer in current window
        helm-move-to-line-cycle-in-source t)
  (setq-default helm-ff-file-name-history-use-recentf t
                helm-M-x-fuzzy-match                  t
                helm-buffers-fuzzy-matching           t
                helm-recentf-fuzzy-match              t
                helm-apropos-fuzzy-match              t)
  (helm-mode 1))



(use-package projectile
  :ensure   t
  :commands (projectile-find-file projectile-switch-project)
  :bind     (:map projectile-mode-map
                  ("C-c p" . projectile-command-map))
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm))



(use-package helm-projectile
  :ensure t
  :config (helm-projectile-on))


(use-package helm-ag
  :ensure   t
  :commands (helm-ag helm-projectile-ag)
  :bind     ("C-c G" . helm-projectile-ag))



(use-package uniquify                   ; make buffer names unique
  :ensure nil
  :config
  (setq uniquify-buffer-name-style   'reverse
        uniquify-ignore-buffers-re   "^\\*" ; don't screw with special buffers
        uniquify-after-kill-buffer-p t))



(use-package paredit
  :ensure t
  :bind   (:map paredit-mode-map
                ("{"   . paredit-open-curly)
                ("}"   . paredit-close-curly)
                ("C-w" . backward-kill-sexp))
  :init
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'list-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'slime-mode-hook #'paredit-mode)
  (add-hook 'slime-repl-mode #'paredit-mode))



(use-package cider
  :ensure t
  :bind   ("C-c M-o" . cider-repl-clear-buffer)
  :config
  (setq cider-repl-display-help-banner nil))



(use-package avy
  :ensure t
  :bind   ("C-c j" . avy-goto-word-1))



(use-package magit
  :ensure t
  :bind   (("C-c g" . magit-status))
  :config
  (setq-default magit-last-seen-setup-instructions "1.4.0"))



(use-package git-timemachine
  :ensure t)



(use-package company
  :ensure t
  :config
  (global-company-mode))



(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))



(use-package multiple-cursors
  :ensure t
  :bind   (("C-S-c C-S-c" . mc/edit-lines)
           ("C->"         . mc/mark-next-like-this)
           ("C-<"         . mc/mark-previous-like-this)
           ("C-c C-<"     . mc/mark-all-like-this)))



(use-package whitespace
  :ensure t

  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'text-mode-hook #'whitespace-mode)

  :config
  (setq whitespace-line-column 100
        whitespace-style       '(face lines-tail)))



(use-package haskell-mode
  :ensure   t
  :commands haskell-mode
  :config
  (setq haskell-stylish-on-save t))



(use-package dante
  :ensure   t
  :after    haskell-mode
  :commands dante-mode

  :init
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)

  :config
  (add-hook 'dante-mode-hook
            '(lambda ()
               (flycheck-add-next-checker 'haskell-dante
                                          '(warning . haskell-hlint)))))



(use-package sublime-themes             ; Not in melpa-stable
  :ensure nil
  :config
  (with-demoted-errors                  ; Don't error when no window system
      (load-theme 'junio)))



(use-package default-text-scale
  :ensure t
  :bind   (("C-M-=" . default-text-scale-increase)
           ("C-M--" . default-text-scale-decrease)
           ;; Note: doesn't yet exist in melpa-stable version.
           ("C-M-0" . default-text-scale-reset)))


(use-package markdown-mode
  :ensure t)



(use-package yaml-mode
  :ensure t)



(use-package expand-region
  :ensure t
  :bind   (("C-=" . er/expand-region)
           ("C--" . er/contract-region)))



(use-package undo-tree                  ; "C-x u" to visualize
  :ensure t
  :bind   (("C-?" . undo-tree-redo))
  :config
  (global-undo-tree-mode 1))




;;;; The End -------------------------------------------------------------------

(message "init.el loaded in %fs" (- (float-time) *emacs-load-start*))
