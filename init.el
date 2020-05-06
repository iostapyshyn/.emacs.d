;;; init.el --- Emacs init
;;; Commentary:
;;; Code:

(when (version< emacs-version "27")
  (let ((early-init-file (concat user-emacs-directory "early-init.el")))
    (when (file-exists-p early-init-file)
      (load-file early-init-file))))

;;; --- Some utility functions ---

(defun buffer-exists (bufname)
  "Return t if buffer with a name BUFNAME exists."
  (not (eq nil (get-buffer bufname))))

(defadvice load-theme (before theme-dont-propagate activate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;;; --- Various small tweaks ---

;; Start the server
(load "server")
(unless (server-running-p) (server-start))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file) ;; Customize is not used

(setq confirm-kill-emacs 'y-or-n-p)

(when window-system
  ;; My preferred fonts
  (set-face-attribute 'default nil
                      :family "Iosevka"
                      :weight 'regular
                      :height 130)

  (set-face-attribute 'variable-pitch nil
                      :family "PT Sans"
                      :weight 'regular
                      :height 1.1))

;; No startup splash screen
(setq inhibit-startup-message t
      inhibit-splash-screen t)

;; No backup file polution
(setq make-backup-files nil)

;; UTF-8 everywhere by default
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; Error bell and y/n confirmation
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; No wrapping, rather truncate
(setq-default truncate-lines t)
(setq column-number-mode t)

;; (global-visual-line-mode t)

;; Scrolling behaviour
(setq scroll-conservatively most-positive-fixnum)

;; Parenthesis
(electric-pair-mode 1)

(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Highlight the whole expression in lisp modes
;; And disable electric-pair-mode
(make-variable-buffer-local 'show-paren-style)
(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook
            (lambda ()
              (setq show-paren-style 'expression)
              (electric-pair-mode 0))))

;; Identation
(setq c-default-style "k&r")
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              css-indent-offset 2
              js-indent-level 2)

;; Calc
(setq-default calc-multiplication-has-precedence nil)

;; Compilation
(setq compilation-window-height 10)
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

(defun c-switch-source-header ()
  "Switch to a header/source file with the same name as current if present in the directory."
  (interactive)
  (defconst c-extensions '("c" "cpp" "cc" "cxx" "m"))
  (defconst h-extensions '("h" "hpp" "hh" "hxx"))
  (let ((extensions (cond ((member (file-name-extension buffer-file-name) c-extensions) h-extensions)
                          ((member (file-name-extension buffer-file-name) h-extensions) c-extensions))))
    (dolist (ext extensions)
      (let ((file-name (concat (file-name-sans-extension buffer-file-name) "." ext)))
        (when (file-exists-p file-name)
          (find-file file-name))))))

(dolist (hook '(c-mode-hook c++-mode-hook objc-mode-hook))
  (add-hook hook
            (lambda ()
              (local-set-key (kbd "C-c h") 'c-switch-source-header))))

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
  (require 'use-package)
  (setq use-package-always-defer t))
(require 'bind-key) ;; :bind requirement

(use-package dash :ensure t)

(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode t)

  (dolist (e '(vterm-mode
               calc-mode
               process-menu-mode
               xref--xref-buffer-mode))
    (add-to-list 'evil-emacs-state-modes e))

  ;; Sorry, Emacs, for rebinding these
  (define-key global-map (kbd "C-f") 'evil-scroll-page-down)
  (define-key global-map (kbd "C-b") 'evil-scroll-page-up)

  (define-key global-map (kbd "C-M-f") 'scroll-other-window)
  (define-key global-map (kbd "C-M-b") 'scroll-other-window-down)

  ;; make :q and :wq close buffer instead of emacs
  (defun save-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))

  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" 'save-kill-this-buffer))

;; org-mode
(use-package org
  :demand t
  :preface
  (defvar my/org "~/org")
  (defvar my/org-index (concat (file-name-as-directory my/org) "index.org"))

  ;; Open the inbox but still keeping the home as default directory
  (when (file-exists-p my/org-index)
    (setq initial-buffer-choice my/org-index))
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq default-directory "~")))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . (lambda ()
                (interactive)
                (org-capture nil "i")))
   ("C-c i" . (lambda ()
                (interactive)
                (find-file my/org-index))))
  :config
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%A, %e. %B %Y>" . "<%A, %e. %B %Y %H:%M>"))
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  (add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("[ ]" . "☐") prettify-symbols-alist)
   (push '("[X]" . "☑") prettify-symbols-alist)
   (push '("[-]" . "❍") prettify-symbols-alist)
   (prettify-symbols-mode)))

  (setq org-cycle-separator-lines 1)

  ;; No security whatsoever..
  (setq org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-link-shell-confirm-function nil
        org-export-use-babel nil)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))

  (setq org-agenda-files (list my/org))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-capture-templates '(("i" "Inbox" entry
                                 (file+headline my/org-index "Inbox")
                                 "* %i%?" :empty-lines 1)))

  ;; My org files may contain bookmarks. They fail to open without this:
  (require 'bookmark)
  (bookmark-maybe-load-default-file)

  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point))

(use-package python
  :bind (:map python-mode-map
              ("C-c C-c" . (lambda () (interactive) (python-shell-send-buffer t)))))

;; dired
(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'top)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alh")
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; Make keybindings work in other keyboard layouts
(use-package reverse-im
  :ensure t
  :demand t
  :custom
  (reverse-im-input-methods '("russian-computer" "ukrainian-computer" "german"))
  :config
  (reverse-im-mode t))

;; M-x history
(use-package smex
  :ensure t
  :demand t
  :config
  (smex-initialize))

;; Better undo
(use-package undo-tree
  :ensure t
  :demand t
  :config
  (global-undo-tree-mode))

;; Key hints
(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

(use-package swiper
  :ensure t
  :bind* ("C-s" . swiper-isearch))

(use-package ivy
  :ensure t
  :demand t
  :bind
  (:map ivy-minibuffer-map ; bind in the ivy buffer
        ("RET"      . ivy-alt-done)
        ("C-f"      . ivy-immediate-done)
        ("M-<down>" . ivy-next-history-element)
        ("M-<up>"   . ivy-previous-history-element))
  :config
  (ivy-mode 1)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))

(use-package ivy-hydra
  :ensure t
  :after ivy)

(use-package counsel
  :ensure t
  :demand t
  :after ivy smex
  :config
  (counsel-mode 1))

(use-package avy
  :ensure t
  :bind* (("C-c SPC" . avy-goto-char-timer))
  :preface
  ;; This package autoloads it's functions
  (evil-define-key '(visual normal motion operator) global-map (kbd "SPC") #'avy-goto-char-timer))

;; Get environment variables
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package minions
  :ensure t
  :demand t
  :config
  (minions-mode 1))

;; Color theme
(when (window-system)
  (use-package modus-operandi-theme
    :ensure t
    :demand t
    :config
    (setq modus-operandi-theme-slanted-constructs t)
    (setq modus-operandi-theme-distinct-org-blocks t)
    (load-theme 'modus-operandi t)))

;; Neotree - navigation tree
(use-package neotree
  :ensure t
  :bind* (("<f8>" . neotree-toggle))
  :config
  (setq neo-theme 'ascii)
  (setq neo-smart-open t)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh))

;; Better terminal emulator
(use-package vterm
  :ensure t
  :bind* (("<f7>" . vterm-open))
  :init
  (defun vterm-open ()
    (interactive)
    (let ((dir default-directory))
      (if (buffer-exists "vterm")
          (if (get-buffer-process "vterm")
              (switch-to-buffer "vterm")
            (kill-buffer "vterm")
            (vterm))
        (vterm))
      (unless (eq dir default-directory)
        ;; Clear the prompt and cd into current directory
        ;; of the buffer from where vterm-open was called
        (vterm-send-C-u)
        (vterm-send-string (concat "cd " dir "\n")))))
  :config
  (define-key vterm-mode-map (kbd "<M-left>") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "<M-right>") 'vterm-send-M-f))

(use-package deadgrep
  :ensure t
  :config
  (add-to-list 'evil-motion-state-modes 'deadgrep-mode)
  (evil-add-hjkl-bindings deadgrep-mode-map 'motion))

(use-package company
  :demand t
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "SPC") nil)
  (setq company-auto-complete nil))

(use-package flycheck
  :demand t
  :ensure t
  :init (global-flycheck-mode))

(use-package octave
  :mode ("\\.m\\'" . octave-mode)
  :config
  (setf octave-block-offset 4))

;; web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

;; Language Server Protocol
(use-package lsp-mode
  :ensure t
  :hook
  ((c-mode
    c++-mode
    objc-mode
    python-mode
    rust-mode
    js2-mode) . lsp-deferred)
  :custom
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-rust-server 'rust-analyzer)
  :config
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$"))

(use-package lsp-ui
  :after lsp-mode
  :ensure t
  :custom
  (lsp-ui-doc-enable nil)) ; Disable giant hovering pop-up boxes.

(use-package company-lsp
  :after lsp-mode company
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp))

;; I don't use snippets but this is used by company-lsp for function arguments
(use-package yasnippet
  :ensure t
  :after company-lsp
  :config
  (yas-global-mode))

;; C/C++/ObjC/GLSL

(use-package ccls
  :after lsp-mode
  :ensure t
  :config
  (setq ccls-initialization-options '(:clang (:extraArgs ("-I/Library/Developer/CommandLineTools/usr/include/c++/v1")))))

(use-package cmake-mode :ensure t)
(use-package glsl-mode :ensure t)

;; Projectile
(use-package projectile
  :demand t
  :ensure t
  :custom
  (projectile-switch-project-action #'projectile-find-file)
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Magit
(use-package magit :ensure t)
(use-package webpaste
  :ensure t
  :bind ("C-c / p" . webpaste-paste-buffer-or-region)
  :config
  (setq webpaste-provider-priority '("dpaste.org" "dpaste.com")))

(use-package markdown-mode
  :ensure t)

;; Install livedown with:
;; $ npm install -g livedown
(use-package livedown
  :commands livedown-preview
  :load-path "emacs-livedown/")

(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; Rust
(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

(use-package gnus
  :commands gnus
  :config
  (setq gnus-init-file (concat (file-name-as-directory my/org) ".gnus.el")))

(use-package pdf-tools
  :ensure t
  :preface
  (pdf-loader-install)
  :config
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))

  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)

  (require 'pdf-outline)
  (define-key pdf-outline-buffer-mode-map (kbd "RET") 'pdf-outline-follow-link-and-quit)
  (define-key pdf-outline-buffer-mode-map (kbd "M-RET") 'pdf-outline-follow-link))

(use-package darkroom :ensure t)
(use-package nov
  :commands nov-bookmark-jump-handler
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :config

  (add-to-list 'evil-motion-state-modes 'nov-mode)
  (evil-define-key 'motion nov-mode-map (kbd "SPC") 'nov-scroll-up)
  (evil-define-key 'motion nov-mode-map (kbd "DEL") 'nov-scroll-down)
  (evil-define-key 'motion nov-mode-map (kbd "]") 'nov-next-document)
  (evil-define-key 'motion nov-mode-map (kbd "[") 'nov-previous-document)
  (evil-define-key 'motion nov-mode-map (kbd "o") 'nov-goto-toc)
  (evil-define-key 'motion nov-mode-map (kbd "g") 'nov-render-document)

  (defun nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "PT Serif" :height 1.1))
  (add-hook 'nov-mode-hook 'nov-font-setup))

(use-package google-this
  :ensure t
  :bind* (("C-c / g" . google-this)))

;;; Show startup time:
(add-hook 'emacs-startup-hook
          (lambda ()
            (message (format "Emacs loaded in %s" (emacs-init-time)))))

(provide 'init)
;;; init.el ends here

;;Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
