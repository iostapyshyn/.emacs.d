;;; init.el --- Personal Emacs configuration.       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Load early-init.el if emacs < 27
(when (version< emacs-version "27")
  (let ((early-init-file (concat user-emacs-directory "early-init.el")))
    (when (file-exists-p early-init-file)
      (load-file early-init-file))))

;; (setq load-prefer-newer t) ;; this was causing some errors

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file) ;; Customize is not used

;; My preferred fonts
(when window-system
  (let ((monospace  '(:family "Iosevka"             :weight regular :height 130))
        (typewriter '(:family "CMU Typewriter Text" :weight regular :height 150))
        (serif      '(:family "CMU Serif"           :weight regular :height 150)))
    (apply 'set-face-attribute 'default nil           monospace)
    (apply 'set-face-attribute 'fixed-pitch nil       monospace)
    (apply 'set-face-attribute 'variable-pitch nil    serif)
    (apply 'set-face-attribute 'fixed-pitch-serif nil typewriter)))

;; No startup splash screen
(setq inhibit-startup-message t
      inhibit-splash-screen t)

;; No backup file polution
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; UTF-8 everywhere by default
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; Error bell and y/n confirmation
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; Zap up to char instead of zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Ask permission before killing emacs.
(setq confirm-kill-emacs 'yes-or-no-p)

;; No wrapping, truncate lines
(setq-default truncate-lines t)
(setq column-number-mode t)

;; Visual line mode in appropriate modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Highlight the current line (only in X)
(when window-system
  (global-hl-line-mode 1))

;; Scroll by single lines, not half-screens
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
              (electric-pair-local-mode 0)
              (setq show-paren-style 'expression))))

;; Identation settings:
(setq c-default-style "k&r")
(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4
              css-indent-offset 2
              js-indent-level 2)

;; Typed text replaces the selection
(delete-selection-mode 1)

;; Don't allow mark commands when the mark is inactive
(setq mark-even-if-inactive nil)

;; make re-builder not require double escaping
(setq reb-re-syntax 'string)

;; Recentf
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(setq recentf-exclude '("^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"
                        "\\.newsrc\\(\\|\\.eld\\)\\'"))

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Calc
(with-eval-after-load 'calc
  (add-hook 'calc-mode-hook (lambda ()
                              (require 'calc-math)
                              (calc-radians-mode)))
  (setq-default calc-multiplication-has-precedence nil))

;; German postfix input method:
;; C-\ to enable: ae -> ä
;; Is buffer-local
(setq default-input-method 'german-postfix)

(defun activate-default-input-method ()
  "Activate the default input method."
  (interactive)
  (activate-input-method default-input-method))
(add-hook 'text-mode-hook 'activate-default-input-method)

;; My org files may contain bookmarks. They fail to open non-interactively:
(defadvice bookmark-jump (before bookmarks-load activate)
  "Load bookmarks file before trying to jump non-interactively."
  (bookmark-maybe-load-default-file)
  (advice-remove 'bookmarks-load 'bookmark-jump))

;; Keys on mac
(when (or (eq window-system 'ns) (eq window-system 'mac))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-right-option-modifier nil))

(add-to-list 'auto-mode-alist '("\\.ino$" . c++-mode))

;; Remove trailing whitespaces on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; --- Personal custom modes and functions ---

(defun spw/exchange-point-and-mark (arg)
  "Exchange point and mark, but reactivate mark a bit less often.

Specifically, invert the meaning of ARG in the case where
Transient Mark mode is on but the region is inactive."
  (interactive "P")
  (exchange-point-and-mark
   (if (and transient-mark-mode (not mark-active))
       (not arg)
     arg)))

(global-set-key [remap exchange-point-and-mark] 'spw/exchange-point-and-mark)

(defun mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)))

(setq-default mode-line-format '((:eval (mode-line-render
                                         (list " "
                                               mode-line-mule-info
                                               mode-line-modified
                                               mode-line-remote
                                               mode-line-frame-identification
                                               mode-line-position " "
                                               mode-line-buffer-identification)
                                         (list '(vc-mode vc-mode)
                                               " %[%m%] "
                                               (format-time-string "│%H:%M%p│")
                                               " ")))))

;; Compilation window should be rather small
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

(defun goto-line-with-line-numbers ()
  "Show line numbers when querying for `goto-line'."
  (interactive)
  (let ((display-line-numbers t))
    (call-interactively #'goto-line)))
(global-set-key [remap goto-line] #'goto-line-with-line-numbers)

;; A place to define keybindings which shall not be shadowed:
(define-minor-mode my-minor-mode
  "A minor mode so that my keybindings won't be shadowed by other major modes."
  :global t
  :init-value t ;; On by default
  :lighter nil
  :keymap `((,(kbd "C-,") . previous-buffer)
            (,(kbd "C-.") . next-buffer)
            (,(kbd "C->") . (lambda ()
                              (interactive)
                              (other-window +1)))
            (,(kbd "C-<") . (lambda ()
                              (interactive)
                              (other-window -1)))))


;;; --- PACKAGES ---

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

;; org-mode
(use-package org
  :demand t
  :preface
  (defvar my/org "~/org")
  (defvar my/org-index (concat (file-name-as-directory my/org) "index.org"))

  ;; Don't show index on recent files
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude (expand-file-name my/org-index)))

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
                (push-mark)
                (find-file my/org-index)))
   ("C-c l" . org-store-link))
  :config
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%A, %e. %B %Y>" . "<%A, %e. %B %Y %H:%M>"))
  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  (add-hook 'org-mode-hook
            (lambda ()
              "Beautify Org Checkbox Symbol"
              (push '("[ ]" . "☐") prettify-symbols-alist)
              (push '("[X]" . "☑") prettify-symbols-alist)
              (push '("[-]" . "❍") prettify-symbols-alist)
              (prettify-symbols-mode)))

  ;; Leave a line between closed headings
  (setq org-cycle-separator-lines 1)

  ;; C-a/C-e stops before tags
  (setq org-special-ctrl-a/e t)

  ;; Fast navigation
  (setq org-use-speed-commands t)

  (setq org-goto-auto-isearch nil)

  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; No security whatsoever..
  (setq org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-link-shell-confirm-function nil
        org-export-use-babel nil)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        ;; imagemagick can preview tikz
        org-preview-latex-default-process 'imagemagick)

  (setq org-pretty-entities t)
  (setq org-agenda-follow-mode t)

  (setq org-agenda-files (list my/org))
  (setq org-agenda-custom-commands '(("a" "Agenda and all TODOs"
                                      ((agenda "")
                                       (todo "TODO"))
                                      nil)))

  (setq org-archive-location (concat my/org "/archive/%s::datetree/"))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-return-follows-link t))

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
  (require 'dired-x)
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files "^\\.[^.]\\|^\\.$")
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode))

(use-package flyspell
  :bind* ("C-c @" . flyspell-toggle)
  :config
  (defun flyspell-toggle ()
    "Turn on flyspell mode if off and run a check on
the buffer. Disable flyspell-mode otherwise."
    (interactive)
    (if (and (boundp 'flyspell-mode) flyspell-mode)
        (flyspell-mode 0)
      (if (derived-mode-p 'prog-mode)
          (flyspell-prog-mode)
        (flyspell-mode 1))
      (flyspell-buffer))))

;; Make key-bindings work in other keyboard layouts
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
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backward)
         (:map swiper-map
               ("C-s" . swiper-C-s)
               ("C-r" . swiper-C-r)))
  :config
  (defun swiper-C-r (&optional arg)
    "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
    (interactive "p")
    (if (string= ivy-text "")
        (ivy-next-history-element 1)
      (ivy-previous-line arg))))

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
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus))))

(use-package ivy-hydra
  :ensure t)

(use-package counsel
  :ensure t
  :demand t
  :after ivy smex
  :config
  (global-set-key [remap recentf-open-files] 'counsel-recentf)
  (counsel-mode 1))

(use-package avy
  :ensure t
  :bind* (("C-c SPC" . avy-goto-char-timer)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

(use-package goto-chg
  :ensure t
  :bind (("C-z" . goto-last-change)))

;; Get environment variables
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; Color theme
(when (window-system)
  (setq custom-safe-themes t)

  (defadvice load-theme (before theme-dont-propagate activate)
    "Discard all themes before loading new."
    (mapc #'disable-theme custom-enabled-themes))

  (defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")

  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))

  (use-package modus-operandi-theme
    :ensure t
    :demand t
    :config
    (setq modus-operandi-theme-slanted-constructs t)
    (setq modus-operandi-theme-distinct-org-blocks t))

  (use-package modus-vivendi-theme
    :ensure t
    :demand t
    :config
    (setq modus-vivendi-theme-slanted-constructs t)
    (setq modus-vivendi-theme-distinct-org-blocks t))

  (defvar current-theme nil)
  (defun synchronize-theme ()
    "Set theme depending on the time of the day."
    (let* ((hour
            (string-to-number
             (substring (current-time-string) 11 13))) ; extract the hour
           (now
            (if (and (>= hour 6) (<= hour 17)) ; if the hour is between 6 and 17
                'modus-operandi ; use light
              'modus-vivendi)))
      (if (equal now current-theme)
          nil
        (setq current-theme now)
        (load-theme now t))))

  ;; Run synchronize-theme now and repeat every 30 minutes
  (run-at-time nil (* 30 60) 'synchronize-theme))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode) . rainbow-delimiters-mode))

;; Neotree - navigation tree
(use-package neotree
  :ensure t
  :bind* (("<f8>" . neotree-toggle))
  :config
  (setq neo-theme 'ascii)
  (setq neo-smart-open t))

(use-package eshell
  :bind ("<s-return>" . eshell-open-with-directory)
  :config
  ;; Plan 9 Smart Shell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (require 'em-smart)
              (setq eshell-where-to-jump 'begin)
              (setq eshell-review-quick-commands nil)
              (setq eshell-smart-space-goes-to-end t)
              (eshell-smart-initialize)))

  ;; Pressing <s-return> twice will open eshell and cd into prev.
  ;; buffer directory.

  (defvar eshell-saved-directory "~"
    "Default directory of the buffer in which `eshell-open-with-directory'
was called last time.")

  (defun eshell-open-with-directory (&optional arg)
    "Opens eshell, but saves buffer directory in a variable `eshell-saved-directory'.
See `eshell-cd-saved-directory'.

If eshell is already open and no argument is specified, change to that directory.
"
    (interactive "P")
    (if (and (derived-mode-p 'eshell-mode) (not arg) eshell-saved-directory)
        (progn
          (cd eshell-saved-directory)
          (eshell-reset nil))
      (setq eshell-saved-directory default-directory)
      (eshell arg)
      (unless (equal default-directory eshell-saved-directory)
        (where-is 'eshell-open-with-directory)))))

;; Better terminal emulator
(use-package vterm
  :ensure t
  :bind* (("<f7>" . vterm-open))
  :init
  (defun vterm-open ()
    (interactive)
    (if (get-buffer "vterm")
        (if (get-buffer-process "vterm")
            (switch-to-buffer "vterm")
          (kill-buffer "vterm")
          (vterm))
        (vterm)))
  :config
  (define-key vterm-mode-map (kbd "<M-left>") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "<M-right>") 'vterm-send-M-f)
  (define-key vterm-mode-map (kbd "M-p") 'vterm-send-C-p)
  (define-key vterm-mode-map (kbd "M-n") 'vterm-send-C-n)
  (setq vterm-kill-buffer-on-exit t))

(use-package deadgrep
  :ensure t)

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
  :bind ("C-c ! !" . flycheck-mode)
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
  ((rust-mode ;; c-mode c++-mode objc-mode python-mode
    js2-mode) . lsp-deferred)
  :custom
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :commands lsp-ui-mode
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
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq TeX-auto-save t)
  (TeX-global-PDF-mode 1)

  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

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

  ;; Invert RET behavior
  (require 'pdf-outline)
  (define-key pdf-outline-buffer-mode-map (kbd "RET") 'pdf-outline-follow-link-and-quit)
  (define-key pdf-outline-buffer-mode-map (kbd "M-RET") 'pdf-outline-follow-link))

(use-package pdf-view-restore
  :ensure t
  :after pdf-tools
  :hook (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename "~/.emacs.d/pdf-view-restore"))

(use-package olivetti
  :ensure t
  :custom
  (olivetti-body-width 0.64)
  (olivetti-enable-visual-line-mode nil))

(use-package google-this
  :ensure t
  :bind (("C-c / g" . google-this)))


;;; --- Some final nuances ---

;; Start the server if not running
(load "server")
(unless (server-running-p) (server-start))

;;; Show startup time:
(add-hook 'emacs-startup-hook
          (lambda ()
            (message (format "Emacs loaded in %s." (emacs-init-time)))))

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
