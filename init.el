;;; init.el --- Emacs init
;;; Commentary:
;;; Code:

;;; --- Some utility functions ---

(defun buffer-exists (bufname)
  "Return t if buffer with a name BUFNAME exists."
  (not (eq nil (get-buffer bufname))))

(defadvice load-theme (before theme-dont-propagate activate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

;;; --- Various small tweaks ---

(setq gc-cons-threshold (* 256 1024 1024))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file) ;; Customize is not used

;; Appearance
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode 0)

  ;;  Initial frame
  (setq initial-frame-alist '((fullscreen . maximized)))

  ;; My preferred font
  (set-face-attribute 'default nil :family "Iosevka")
  (set-face-attribute 'default nil :weight 'regular)
  (set-face-attribute 'default nil :height 130))

;; No startup splash screen
(setq inhibit-startup-message t
      inhibit-splash-screen t)

;; No backup file polution
(setq make-backup-files nil)

;; UTF-8 everywhere by default
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; Error bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Lazy confirmation
(fset 'yes-or-no-p 'y-or-n-p)

;; No wrapping, rather truncate
(setq-default truncate-lines t)
(setq column-number-mode t)

;; (global-visual-line-mode t)

;; Multi window gdb by default
(setq gdb-many-windows t
      gdb-show-main t)

;; Parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)

;; Identation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq css-indent-offset 2)
(setq js-indent-level 2)

(setq-default c-default-style "k&r"
              c-basic-offset 4)

;; Calc
(setq-default calc-multiplication-has-precedence nil)

;; Compilation
(defvar compilation-window-height 10)
(defun my/compilation-hook ()
  "Set compilation window height."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my/compilation-hook)
(make-variable-buffer-local 'compile-command)

(global-set-key [(f5)] 'compile)
(global-set-key [(f6)] 'recompile)

;; Keys on mac
(setq mac-command-key-is-meta nil
      mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-option-modifier nil)

(define-key global-map [?\s-x] 'kill-region)
(define-key global-map [?\s-c] 'kill-ring-save)
(define-key global-map [?\s-v] 'yank)
(define-key global-map [?\s-z] 'undo)
(define-key global-map [?\s-a] 'mark-whole-buffer)

;; Make C-c C-c behave like C-u C-c C-c in Python mode
(require 'python)
(define-key python-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (python-shell-send-buffer t)))

(add-to-list 'auto-mode-alist '("\\.ino$" . c++-mode))

;; Remove trailing whitespaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -- PACKAGES --

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key) ;; :bind requirement

(use-package quail
  :demand t
  :config
  ;; Makes russian keyboard layout work for keybindings
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
  (reverse-input-method 'ukrainian-computer))

;; org-mode
(use-package org
  :demand t
  :preface
  (defvar my/org "~/org")
  (defvar my/org-inbox (concat (file-name-as-directory my/org) "inbox.org"))
  (defvar my/org-journal (concat (file-name-as-directory my/org) "journal.org"))
  (setq initial-buffer-choice my/org-inbox)
  :config
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c i")
    (lambda ()
      (interactive)
      (find-file my/org-inbox)))

  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%A, %e. %B %Y>" . "<%A, %e. %B %Y %H:%M>"))
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  (setq org-agenda-files (list my/org))
  (setq org-capture-templates
        '(("i" "Inbox" entry (file+headline my/org-inbox "New")
           "* TODO %i%?")
          ("j" "Journal" entry (file+datetree my/org-journal)
           "* %i%?\n  %T" :time-prompt t))))

;; dired
(use-package dired
  :config
  (setq dired-dwim-target t
        dired-recursive-copies 'top
        dired-recursive-deletes 'top
        dired-listing-switches "-alh")
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; M-x history
(use-package smex
  :ensure t
  :config
  (smex-initialize))

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

(use-package ivy
  :ensure t
  :demand t
  :bind
  (:map ivy-minibuffer-map ; bind in the ivy buffer
        ("RET" . ivy-alt-done)
        ("C-f" . ivy-immediate-done))
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy))))

(use-package counsel
  :ensure t
  :demand t
  :after ivy
  :config
  (counsel-mode 1))

(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode t)
  (evil-set-initial-state 'term-mode 'emacs)
  (add-to-list 'evil-emacs-state-modes 'bs-mode)

  (defun save-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))

  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" 'save-kill-this-buffer))

(use-package dash :ensure t)

;; Get environment variables
(use-package exec-path-from-shell
  :ensure t
  :config
  (add-to-list 'exec-path-from-shell-variables "GOROOT")
  (add-to-list 'exec-path-from-shell-variables "GOPATH")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Color theme
(use-package spacemacs-theme
  :defer t
  :ensure t
  :init
  (load-theme 'spacemacs-light t)
  (dolist (face '(org-level-1
                org-level-2
                org-level-3
                org-level-4
                org-level-5))
  (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

;; Neotree - navigation tree
(use-package neotree
  :ensure t
  :defer t
  :bind* (("<f8>". neotree-toggle))
  :init
  (add-to-list 'evil-emacs-state-modes 'neotree-mode)
  (setq neo-theme 'nerd)
  (setq neo-smart-open t))

;; Better terminal emulator
(use-package vterm
  :ensure t
  :defer t
  :bind* (("<f7" . vterm-open))
  :init
  (evil-set-initial-state 'vterm-mode 'emacs)
  (defun vterm-open ()
      (interactive)
    (if (buffer-exists "vterm")
        (if (get-buffer-process "vterm")
            (switch-to-buffer "vterm")
          (kill-buffer "vterm")
          (vterm))
      (vterm))))

;; Auto-Complete
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "SPC") nil)
  (setq company-auto-complete nil))

;; The Silver Searcher
(use-package ag
  :ensure t)

(use-package bufler
  :ensure t
  :bind* (("C-x C-b" . bufler)
          ("C-x b" . bufler-switch-buffer))
  :init
  (add-to-list 'evil-emacs-state-modes 'bufler-list-mode))

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

;;;;; Web ;;;;;

;; web-mode
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package js2-mode
  :ensure t
  :defer t
  :mode "\\.js\\'")

;; LSP
(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'objc-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'js2-mode-hook #'lsp)
  (setq lsp-enable-indentation nil)
  (setq lsp-rust-server 'rust-analyzer)
  (setq lsp-enable-on-type-formatting nil))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil)) ; Disable giant hovering pop-up boxes.

(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp))

;; C/C++/ObjC/GLSL

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$")
  (setq ccls-initialization-options '(:clang (:extraArgs ("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))))

;; (use-package cmake-ide
;;   :ensure t
;;   :config (cmake-ide-setup)
;;   (setq cmake-ide-flags-c++ '("-I/Library/Developer/CommandLineTools/usr/include/c++/v1"))
;;   (add-to-list 'cmake-ide-cmake-args "-DCMAKE_EXPORT_COMPILE_COMMANDS=1")
;;   (setq-default cmake-ide-rdm-rc-path (concat (getenv "HOME") "/.emacs.d/rdmrc")))

(use-package cmake-mode
  :ensure t)

(use-package glsl-mode
  :ensure t)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'native)
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
  (add-to-list 'projectile-globally-ignored-directories ".git")
  (add-to-list 'projectile-globally-ignored-directories "build/CMakeFiles")
  (add-to-list 'projectile-globally-ignored-directories "CMakeFiles")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store"))

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

;;(use-package rustic
;;  :ensure t
;;  :config
;;  (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
;;  (setq rustic-format-on-save t))

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

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

(use-package minions
  :ensure t
  :config (minions-mode 1))

;; install livedown with
;; npm install -g livedown

(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

(provide 'init)
;;; init.el ends here

;;Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
