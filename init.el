;;; init.el --- Emacs init
;;; Commentary: USE Mac port of Emacs by Mitsuharu Yamamoto
;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
; (load custom-file)

(setq gc-cons-threshold (* 500 1024 1024))

(require 'package)
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq default-directory (getenv "HOME"))

(unless (package-installed-p 'org)
  (package-refresh-contents)
  (package-install 'org))

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;; Appearance
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0))

;; Initial window
(setq initial-frame-alist
      '((width . 152)   ; characters in a line
        (height . 54))) ; number of lines

;; Sebsequent frame
(setq default-frame-alist
      '((width . 150)   ; characters in a line
        (height . 52)
        (fullscreen . maximized)))

(set-face-attribute 'default nil :family "Hasklig")
(set-face-attribute 'default nil :height 130)

;; Startup
(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t)

;; Backups
(setq make-backup-files nil)

;; UTF-8
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; Error messages
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; No wrapping
(setq-default truncate-lines t)
(setq column-number-mode t)
(global-visual-line-mode t)

;; Lazy confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;; Multi window gdb by default
(setq gdb-many-windows t
      gdb-show-main t)

;; Parenthesis
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-pair-mode 1)

;; Identation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Buffer navigation
(global-set-key [remap list-buffers] 'bs-show)
(global-set-key (kbd "C-x b") 'list-buffers)

;; Compilation
(setq compilation-window-height 10)

(defun my-compilation-hook ()
  "Set compilation window height."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(setq-default c-default-style "java"
              c-basic-offset 4)

(global-set-key [(f5)] 'compile)
(global-set-key [(f6)] 'recompile)
(global-set-key [(f7)] 'shell-command)

;; Discard all themes on load-theme
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable theme before loading new."
  (mapc #'disable-theme custom-enabled-themes))

;; Ligatures
(if (fboundp 'mac-auto-operator-composition-mode) (mac-auto-operator-composition-mode))

;; Keys on mac
(setq ;mac-option-key-is-meta t
      mac-command-key-is-meta nil
      mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-option-modifier nil)

(define-key global-map [?\s-x] 'kill-region)
(define-key global-map [?\s-c] 'kill-ring-save)
(define-key global-map [?\s-v] 'yank)
(define-key global-map [?\s-z] 'undo)
(define-key global-map [?\s-a] 'mark-whole-buffer)

;; Makes russian keyboard layout work for keybindings
(require 'quail)

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method 'russian-computer)
(reverse-input-method 'ukrainian-computer)

;; Make C-c C-c behave like C-u C-c C-c in Python mode
(require 'python)
(define-key python-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (python-shell-send-buffer t)))

(add-to-list 'auto-mode-alist '("\\.ino$" . c++-mode))

;; -- PACKAGES --

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key) ;; :bind requirement

;; Get environment variables
(use-package exec-path-from-shell
  :ensure t
  :config
  (add-to-list 'exec-path-from-shell-variables "GOROOT")
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Color theme

;(use-package base16-theme
;  :ensure t
;  :config (load-theme 'base16-tomorrow-night t)
;  (set-cursor-color "#dddddd"))

(use-package spacemacs-theme
  :defer t
  :ensure t
  :init (load-theme 'spacemacs-light t))

;; Better M-x
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; Better undo
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;; Key hints
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Dired navigation
(use-package dired
  :bind (:map dired-mode-map
              ("C-c C-e" . wdired-change-to-wdired-mode))
  :init
  (setq dired-dwim-target t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-listing-switches "-alh")
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; Neotree - navigation tree
(use-package neotree
  :ensure t
  :bind* (("<f8>". neotree-toggle))
  :init
  (setq neo-theme 'nerd)
  (setq neo-smart-open t))

;; Auto-Complete
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "SPC") nil)
  (setq company-auto-complete nil))

;; Snippets
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; The Silver Searcher
(use-package ag
  :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Octave
(use-package octave
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
  (setf octave-block-offset 4))

;; Emmet support for Emacs (Web)
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

;; C/C++/ObjC/GLSL
(use-package rtags
  :ensure t
  :init
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running))

(use-package irony
  :ensure t
  :config
  (add-to-list 'irony-supported-major-modes 'glsl-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(use-package dash :ensure t)
(use-package cmake-ide
  :ensure t
  :config (cmake-ide-setup))

(use-package glsl-mode
  :ensure t)

;; Python
(use-package elpy
  :ensure t)

(use-package company-jedi
  :ensure t
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Magit and Gist - GitHub integration
(use-package magit
  :ensure t)

(use-package gist
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t)

;; LaTeX
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; Go
(use-package go-mode
  :ensure t
  :config
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook 'gofmt-before-save) ; Call Gofmt before saving
    (if (not (string-match "go" compile-command)) ; Customize compile command to run go build
        (set (make-local-variable 'compile-command)
             "go run ."))
    (local-set-key (kbd "M-.") 'godef-jump) ; Godef jump key binding
    (local-set-key (kbd "M-*") 'pop-tag-mark))
  (add-hook 'go-mode-hook 'my-go-mode-hook)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package company-go
  :ensure t
  :config
  (add-to-list 'company-backends 'company-go))

(use-package rust-mode
  :ensure t
  :config
  (defun my-rust-mode-hook ()
    (if (not (string-match "cargo" compile-command))
        (set (make-local-variable 'compile-command)
             "cargo build")))
  (add-hook 'rust-mode-hook 'my-rust-mode-hook)
  (setq rust-format-on-save t))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'init)
;;; init.el ends here

;;Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
