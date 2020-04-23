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
  ;; My preferred font
  (set-face-attribute 'default nil
                      :family "Iosevka"
                      :weight 'regular
                      :height 130))

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

;; (global-visual-line-mode t) ; I don't like this

;; Scrolling behaviour
(setq scroll-conservatively most-positive-fixnum)

;; Parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)

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

(use-package python
  :bind (:map python-mode-map
              ("C-c C-c" . (lambda () (interactive) (python-shell-send-buffer t)))))

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
  ;; Keep org headlines the same size
  (add-hook 'after-load-theme-hook
            (lambda ()
              (dolist (face '(org-level-1
                              org-level-2
                              org-level-3
                              org-level-4
                              org-level-5
                              org-tag))
                (set-face-attribute face nil :weight 'semi-bold :height 1.0))))

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
   ("C-c i" . (lambda ()
              (interactive)
              (find-file my/org-index))))
  :config
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%A, %e. %B %Y>" . "<%A, %e. %B %Y %H:%M>"))
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  ;; No security whatsoever..
  (setq org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-link-shell-confirm-function nil
        org-export-use-babel nil)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))

  (setq org-agenda-files (list my/org))

  ;; My org files may contain bookmarks. They fail to open without this:
  (require 'bookmark)
  (bookmark-maybe-load-default-file))

;; dired
(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'top)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alh")
  :config
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

(use-package dash :ensure t)

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
      '((swiper-isearch . ivy--regex-plus)
        (swiper         . ivy--regex-plus)
        (t              . ivy--regex-fuzzy))))

(use-package counsel
  :ensure t
  :demand t
  :after ivy smex
  :config
  (counsel-mode 1))

(use-package evil
  :ensure t
  :demand t
  :config
  (evil-mode t)
  (add-to-list 'evil-emacs-state-modes 'term-mode)
  (add-to-list 'evil-emacs-state-modes 'neotree-mode)
  (add-to-list 'evil-emacs-state-modes 'bs-mode)
  (add-to-list 'evil-emacs-state-modes 'vterm-mode)
  (add-to-list 'evil-emacs-state-modes 'bufler-list-mode)
  (add-to-list 'evil-emacs-state-modes 'calculator-mode)
  (add-to-list 'evil-emacs-state-modes 'calc-mode)
  (add-to-list 'evil-emacs-state-modes 'deadgrep-mode)
  (add-to-list 'evil-emacs-state-modes 'process-menu-mode)

  ;; make :q and :wq close buffer instead of emacs
  (defun save-kill-this-buffer ()
    (interactive)
    (save-buffer)
    (kill-this-buffer))

  (evil-ex-define-cmd "q" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" 'save-kill-this-buffer))

(use-package avy
  :ensure t
  :bind* (("C-c C-'" . avy-goto-char-timer))
  :preface
  ;; This package autoloads it's functions
  (evil-define-key '(visual normal operator) global-map (kbd "s") #'avy-goto-char-timer))

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
  (use-package atom-one-dark-theme
    :ensure t
    :demand t
    :config
    (load-theme 'atom-one-dark t))

  (use-package spacemacs-theme
    :disabled
    :ensure t
    :defer t
    :init
    (defvar current-theme nil)
    (defun synchronize-theme ()
      "Set theme depending on the time of the day."
      (let* ((hour
              (string-to-number
               (substring (current-time-string) 11 13))) ; extract the hour
             (now
              (if (and (> hour 6) (< hour 17)) ; if the hour is between 6 and 17
                  'spacemacs-light ; use light
                'spacemacs-dark)))
        (if (equal now current-theme)
            nil
          (setq current-theme now)
          (load-theme now t))))

    ;; Run synchronize-theme now and repeat every 30 minutes
    (run-at-time nil (* 30 60) 'synchronize-theme)))

;; Neotree - navigation tree
(use-package neotree
  :ensure t
  :bind* (("<f8>" . neotree-toggle))
  :config
  (setq neo-theme 'nerd)
  (setq neo-smart-open t))

;; Better terminal emulator
(use-package vterm
  :ensure t
  :bind* (("<f7>" . vterm-open))
  :init
  (defun vterm-open ()
    (interactive)
    (if (buffer-exists "vterm")
        (if (get-buffer-process "vterm")
            (switch-to-buffer "vterm")
          (kill-buffer "vterm")
          (vterm))
      (vterm)))
  :config
  (define-key vterm-mode-map (kbd "<M-left>") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "<M-right>") 'vterm-send-M-f))

(use-package deadgrep :ensure t)

(use-package bufler
  :ensure t
  :bind* (("C-x C-b" . bufler)
          ("C-x b" . bufler-switch-buffer)))

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
  (projectile-enable-caching t)
  (projectile-indexing-method 'native)
  :config
  (projectile-mode 1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
  (add-to-list 'projectile-globally-ignored-directories ".git")
  (add-to-list 'projectile-globally-ignored-directories "build/CMakeFiles")
  (add-to-list 'projectile-globally-ignored-directories "CMakeFiles")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store"))

;; Magit and Gist - GitHub integration
(use-package magit :ensure t)
(use-package gist :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package livedown
  :after markdown-mode
  :demand t
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

;;; Show startup time:
(add-hook 'emacs-startup-hook
          (lambda ()
            (message (format "Emacs loaded in %s" (emacs-init-time)))))

(provide 'init)
;;; init.el ends here

;;Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
