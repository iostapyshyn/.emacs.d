;;; init.el --- Personal Emacs configuration.       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Load early-init.el if emacs < 27
(when (version< emacs-version "27")
  (let ((early-init-file (concat user-emacs-directory "early-init")))
    (load early-init-file t)))

(setq custom-file (concat user-emacs-directory "custom"))
;; (load custom-file t) ;; Customize is not used

(setq local-init-file (concat user-emacs-directory "local/"
                              (md5 (system-name))))
(load local-init-file t)

(when window-system
  (when (boundp 'font-monospaced)
    (apply 'set-face-attribute 'default        nil font-monospaced)
    (apply 'set-face-attribute 'fixed-pitch    nil font-monospaced))
  (when (boundp 'font-proportional)
    (apply 'set-face-attribute 'variable-pitch nil font-proportional))

  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(unless (member window-system '(ns mac))
  (menu-bar-mode -1))

(setq inhibit-startup-message t)

;; No backup file polution
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; UTF-8 everywhere by default
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

(setq epa-pinentry-mode 'loopback)

;; Error bell and y/n confirmation
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;; Zap up to char instead of zap to char
(global-set-key (kbd "M-z")             'zap-up-to-char)
(global-set-key (kbd "M-Z")             'zap-to-char)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "ESC M-DEL")       'backward-kill-sexp) ;; for terminal

;; Ask permission before killing emacs.
(setq confirm-kill-emacs 'yes-or-no-p)

;; Enable all disabled commands
(setq disabled-command-function nil)
(setq enable-remote-dir-locals t)

;; No wrapping, truncate lines
(setq-default truncate-lines t)
(setq-default fill-column 90)
(setq column-number-mode t)

(setq sentence-end-double-space nil)
(setq message-fill-column nil)

(add-hook 'help-mode-hook 'turn-on-visual-line-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

;; Scroll by single lines, not half-screens
(setq scroll-conservatively most-positive-fixnum)
(setq scroll-margin 1)

;; Parenthesis
(electric-pair-mode 1)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; disable electric-pair-mode for lisp
(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook
            (lambda ()
              (electric-pair-local-mode -1))))

;; Indentation settings:
(with-eval-after-load "cc-styles"
  ;; Use 4 spaces indentation instead of 5 for k&r
  (setf (cdr (assoc 'c-basic-offset (assoc "k&r" c-style-alist))) 4)
  (setq c-default-style "k&r"))

(setq-default indent-tabs-mode nil
              tab-width 8
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
(setq recentf-max-saved-items 50)
(setq recentf-exclude '("^/var/folders\\.*"
                        "COMMIT_EDITMSG\\'"
                        ".*-autoloads\\.el\\'"
                        "[/\\]\\.elpa/"
                        "\\.newsrc\\(\\|\\.eld\\)\\'"))

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Calc
(setq-default calc-multiplication-has-precedence nil)

;; German postfix input method:
;; C-\ to enable: ae -> ä
;; Is buffer-local
(setq default-input-method 'german-postfix)

;; Org files may contain bookmarks. They fail to open non-interactively:
(define-advice bookmark-jump (:before (&rest _r) bookmarks-load)
  "Load bookmarks file before trying to jump non-interactively."
  (bookmark-maybe-load-default-file)
  (advice-remove 'bookmark-jump 'bookmark-jump@bookmarks-load))
(with-eval-after-load "bookmark"
  (add-to-list 'recentf-exclude (regexp-quote (expand-file-name bookmark-file))))

;; Keys on mac
(when (or (eq window-system 'ns) (eq window-system 'mac))
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-right-option-modifier nil)
  (setq mac-frame-tabbing nil))

(add-to-list 'auto-mode-alist '("\\.ino$" . c++-mode))

;; Remove trailing whitespaces on save.
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)


;;; --- Personal custom modes and functions ---
(defun duden (word)
  "Search for the WORD definition on duden.de.  Requires github.com/radomirbosak/duden."
  (interactive
   (list (read-from-minibuffer "$ duden "
                               (if (region-active-p)
                                   (buffer-substring (region-beginning) (region-end))
                                 (thing-at-point 'word)))))
  (let* ((buffer "*Duden Output*")
         (buffer-name-function (lambda (_) buffer))
         (compilation-buffer-name-function buffer-name-function))
    (compile (concat "duden " word))
    (with-current-buffer buffer
      (turn-on-visual-line-mode))))

(global-set-key (kbd "C-c q d") 'duden)

(defun mode-line-render (left right)
  "Return a mode-line formatted string containing LEFT and RIGHT aligned respectively."
  (let* ((left (format-mode-line left))
         (right (format-mode-line right))
         (space (propertize " " 'display
                            `(space :align-to (- scroll-bar ,(string-width right))))))
    (replace-regexp-in-string "%" "%%" (concat left space right))))

(setq-default mode-line-format '((:eval (mode-line-render
                                         (list " "
                                               mode-line-mule-info
                                               mode-line-modified
                                               mode-line-remote
                                               mode-line-frame-identification
                                               mode-line-position " "
                                               mode-line-buffer-identification)
                                         (list mode-line-misc-info
                                               '(vc-mode vc-mode)
                                               " "
                                               mode-line-modes)))))
(setq mode-line-compact 'long)

(setq which-func-unknown "…")
(which-function-mode)

(setq compilation-scroll-output 'first-error)
;;(make-variable-buffer-local 'compile-command)
(defun compile-maybe-project ()
  "Call `project-compile' if buffer belongs to a project or `compile' otherwise."
  (interactive)
  (if (and (fboundp 'project-current)
           (project-current nil))
      (call-interactively 'project-compile)
    (call-interactively 'compile)))

(global-set-key (kbd "C-c c") 'compile-maybe-project)
(global-set-key (kbd "M-g o") 'ff-find-other-file)
(global-set-key (kbd "M-g i") 'imenu)

;; Obsoleted by consult:

;; (defun goto-line-with-line-numbers ()
;;   "Show line numbers when querying for `goto-line'."
;;   (interactive)
;;   (let ((display-line-numbers t))
;;     (call-interactively #'goto-line)))
;; (global-set-key [remap goto-line] #'goto-line-with-line-numbers)

(global-set-key (kbd "C-h M") 'man)


;;; --- Packages ---
(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (require 'bind-key)

  (setq use-package-always-defer t))

(use-package dash :ensure t)

(use-package window
  :bind* (("C-," . previous-buffer)
          ("C-." . next-buffer)
          ("C-;" . other-window)))

(use-package tab-bar
  :bind* (("M-`" . tab-next))
  :config
  (setq tab-bar-show nil))

(use-package minions
  :ensure t
  :demand t
  :config
  (add-to-list 'minions-direct 'lsp-mode)
  (minions-mode 1))

(use-package project
  :bind-keymap* ("C-x p" . project-prefix-map)
  :bind (:map project-prefix-map
              ("m" . magit)
              ("z" . vterm))
  :config
  (add-to-list 'project-switch-commands '(vterm "Vterm") t)
  (add-to-list 'project-switch-commands '(magit "Magit") t)
  ;; Credit to karthink for this .project detection snippet below
  (setq project-local-identifier ".project")
  (cl-defmethod project-root ((project (head local)))
    (cdr project))
  (defun project-try-local (dir)
    "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
    (if-let ((root (locate-dominating-file dir project-local-identifier)))
        (cons 'local root)))
  (add-hook 'project-find-functions 'project-try-local))


;;; --- Org Mode ---
(use-package org
  :preface
  (defvar my/org "~/org")
  (defun find-my/org ()
    (interactive)
    (push-mark)
    (find-file my/org))
  :bind*
  (("C-c a" . org-agenda)
   ("C-c i" . find-my/org)
   ("C-c l" . org-store-link))
  :config
  (setq-default org-display-custom-times t)
  (setq org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %d %b %Y %H:%M>"))

  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%-12t% s")
                                   (todo   . " %i %-12:c")
                                   (tags   . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (setq org-agenda-start-on-weekday 1)
  (setq calendar-week-start-day 1)

  ;; Make sure the time stamps are formatted in English across the systems
  (setq system-time-locale "C")

  (setq org-clock-mode-line-total 'today)
  (setq org-duration-format (quote h:mm))

  ;; Don't leave lines between closed headings
  (setq org-cycle-separator-lines 0)

  ;; C-a/C-e stops before tags
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  ;; Fast navigation
  (setq org-use-speed-commands t)

  (setq org-goto-auto-isearch nil)

  (setq org-fontify-done-headline nil)
  (setq org-fontify-todo-headline nil)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  ;; No security whatsoever..
  (setq org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-link-shell-confirm-function nil
        org-export-use-babel nil)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.25)
        org-preview-latex-default-process 'dvisvgm)

  (setq org-agenda-follow-mode t)

  (setq org-agenda-files (list my/org))
  (setq org-agenda-custom-commands '(("a" "Agenda and all TODOs"
                                      ((agenda "")
                                       (alltodo ""))
                                      nil)))

  (setq org-archive-location (concat my/org "/archive/%s::datetree/"))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-return-follows-link t)
  (setq org-startup-indented t))

(use-package python
  :bind (:map python-mode-map
              ("C-c C-c" . (lambda () (interactive) (python-shell-send-buffer t))))
  :config
  (setq python-shell-completion-native-enable t)
  (setq python-shell-interpreter "python3")
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i")))

(use-package eww
  :bind (("C-h 7" . eww-man7-index)
         (:map eww-mode-map
               ("j" . prot-eww-visit-url-on-page)))
  :config
  (setq eww-search-prefix "https://lite.duckduckgo.com/html/?q=")

  (defun eww-man7-index ()
    "Opens the list of man7.org pages in EWW."
    (interactive)
    (pop-to-buffer
     (get-buffer-create "*eww-man7.org*"))
    (let ((index "https://man7.org/linux/man-pages/dir_all_alphabetic.html")
          (prefix "https://man7.org/linux/man-pages/")
          (cur (and (boundp 'eww-data) (plist-get eww-data :url))))
      (unless (string-prefix-p prefix cur)
        (eww-mode) ;; forces to load in the current buffer
        (eww index))))

  (defun prot-eww-visit-url-on-page (&optional arg)
    "Visit URL from list of links on the page using completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
    (interactive "P")
    (when (derived-mode-p 'eww-mode)
      (let ((links))
        (save-excursion
          (goto-char (point-max))
          (while (text-property-search-backward 'shr-url nil nil t)
            (when (and (get-text-property (point) 'shr-url)
                       (not (get-text-property (point) 'eww-form)))
              (push (format "%s @ %s"
                            (button-label (point))
                            (propertize (get-text-property (point) 'shr-url) 'face 'link))
                    links))))
        (let* ((selection (completing-read "Browse URL from page: " links nil t))
               (url (replace-regexp-in-string ".*@ " "" selection)))
          (eww url (if arg 4 nil)))))))

(use-package isearch
  :bind (:map isearch-mode-map
              ("<C-return>" . isearch-done-other-end))
  :config
  (defun isearch-done-other-end ()
    "End current search in the opposite side of the match."
    (interactive)
    (isearch-done)
    (when isearch-other-end
      (goto-char isearch-other-end)))
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil)
  (setq isearch-lazy-highlight t)
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format nil)
  (setq lazy-count-suffix-format " [%s/%s]")
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'top)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-alh")
  (dired-auto-revert-buffer t)
  :config
  (require 'dired-x)
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq dired-omit-files "^\\.[^.]\\|^\\.$")
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook 'dired-mode-hook 'dired-omit-mode))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

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

;; (use-package flymake
;;   :bind* (("C-c ! !" . flymake-mode)
;;           (:map flymake-mode-map
;;                 ("C-c ! l" . flymake-show-diagnostics-buffer)
;;                 ("C-c ! n" . flymake-goto-next-error)
;;                 ("C-c ! p" . flymake-goto-prev-error)
;;                 ("C-c ! s" . flymake-start)))
;;   :hook ((prog-mode LaTeX-mode) . flymake-mode))

;; (use-package flymake-diagnostic-at-point
;;   :ensure t
;;   :after flymake
;;   :hook (flymake-mode . flymake-diagnostic-at-point-mode)
;;   :config
;;   (setq flymake-diagnostic-at-point-display-diagnostic-function
;;         'flymake-diagnostic-at-point-display-minibuffer)
;;   (setq flymake-diagnostic-at-point-error-prefix ""))

;; Make key-bindings work in other keyboard layouts
(use-package reverse-im
  :ensure t
  :demand t
  :custom
  (reverse-im-input-methods '("ukrainian-computer" "german"))
  :config
  (reverse-im-mode t))

(use-package undo-tree
  :ensure t
  :demand t
  :config
  (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))


;;; --- Completion ---
(setq enable-recursive-minibuffers t)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package savehist
  :demand t
  :config
  (savehist-mode))

(use-package vertico
  :ensure t
  :demand t
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
  (add-to-list 'orderless-matching-styles 'orderless-strict-initialism))

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("M-s l"                               . consult-line)
         ("M-s g"                               . consult-grep)
         ("M-s G"                               . consult-git-grep)
         ("M-s r"                               . consult-ripgrep)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap load-theme]                    . consult-theme)
         ([remap goto-line]                     . consult-goto-line)
         ;; ([remap man]                           . consult-man)
         ([remap yank-pop]                      . consult-yank-pop)
         ([remap imenu]                         . consult-imenu))
  :init
  (setq completion-in-region-function 'consult-completion-in-region)
  (add-hook 'gud-mode-hook
            (lambda ()
              (setq-local completion-in-region-function 'completion--in-region)
              (setq-local completion-styles '(basic partial-completion))))
  :config
  (consult-customize
   consult-bookmark consult-recent-file
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "C-o"))
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (project-root project)))))

(use-package embark
  :ensure t
  :bind ("C-c e" . embark-act))

(use-package embark-consult
  :ensure t
  :demand t
  :after (consult embark))

(use-package avy
  :ensure t
  :bind (("C-`" . avy-goto-char)))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;; Get environment variables
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; --- Color theme ---
(setq custom-safe-themes t)

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

(global-set-key (kbd "C-c t t") 'load-theme)

(use-package modus-themes
  :ensure t
  :bind ("C-c t m" . modus-themes-toggle)
  :config
  (setq modus-themes-mode-line '(accented)
        modus-themes-italic-constructs t
        modus-themes-completions 'opinionated
        modus-themes-scale-headings t
        modus-themes-org-blocks 'tinted-background
        modus-themes-headings '((1 . (no-bold overline))
                                (t . (no-bold)))))

(if (window-system)
    (use-package circadian
      :ensure t
      :demand t
      :config
      (setq calendar-latitude 52.38
            calendar-longitude 9.69)
      (setq circadian-themes '((:sunrise . modus-operandi)
                               (:sunset  . modus-vivendi)))
      (circadian-setup))
  (load-theme 'modus-vivendi t))

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode) . rainbow-delimiters-mode))

(use-package eshell
  :bind ("C-z" . eshell)
  :config
  (define-advice eshell
      (:around (orig-fun &rest args) save-directory)
    (if (and (derived-mode-p 'eshell-mode)
             (not (car args))
             (bound-and-true-p eshell-saved-directory))
        (progn
          (cd eshell-saved-directory)
          (eshell-reset nil))
      (setq eshell-saved-directory default-directory)
      (apply orig-fun args)))

  (defun eshell/last-remote (&optional _indices)
    (when-let ((r (if-let ((base (file-remote-p default-directory)))
                      base
                    (seq-some 'file-remote-p (ring-elements eshell-last-dir-ring)))))
      ;; remove the trailing semicolon, helps to multi-hop
      ;; i.e. cd $r|sudo::
      (replace-regexp-in-string ":\\'" "" r)))
  (require 'esh-var)
  (add-to-list 'eshell-variable-aliases-list '("r" eshell/last-remote))

  (setq eshell-destroy-buffer-when-process-dies t)
  (defalias 'eshell/v 'eshell-exec-visual)
  (defalias 'eshell/ff 'find-file)
  (defalias 'eshell/clear 'eshell/clear-scrollback))

(use-package vterm
  :ensure t
  :bind (("C-x C-z" . vterm)
         (:map vterm-mode-map
               ("C-c TAB"   . vterm-cd-saved-directory)
               ("C-c C-x"   . vterm-send-C-x)
               ("M-p"       . vterm-send-C-p)
               ("M-n"       . vterm-send-C-n)))
  :config
  (define-advice vterm
      (:before (&optional _arg) save-directory)
    (setq vterm-saved-directory default-directory))

  (defun vterm-cd-saved-directory ()
    (interactive)
    (when (bound-and-true-p vterm-saved-directory)
      (vterm-insert (concat "cd " vterm-saved-directory "\n"))))

  (setq vterm-max-scrollback 50000)
  (setq vterm-timer-delay 0.01)
  (setq vterm-kill-buffer-on-exit t))

(use-package eshell-vterm
  :load-path "site-lisp/eshell-vterm"
  :demand t
  :after eshell
  :config
  (eshell-vterm-mode))

;; Smart trailing whitespace trimming
(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package company
  :demand t
  :ensure t
  :config
  ;; To suppress orderless when using company-capf
  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    (let ((completion-styles '(basic partial-completion)))
      (apply orig-fun args)))

  (add-hook 'after-init-hook 'global-company-mode)
  ;; disable company for shells (causes lags on remotes)
  (setq company-global-modes '(not eshell-mode shell-mode gud-mode))
  ;; keep case
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

(use-package flycheck
  :demand t
  :ensure t
  :bind ("C-c ! !" . flycheck-mode)
  :init (global-flycheck-mode))

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

(use-package ggtags
  :config
  (setq ggtags-enable-navigation-keys nil))

(use-package lsp-mode
  :ensure t
  :hook
  ((rust-mode) . lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-clients-clangd-args '("--header-insertion=never"))
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection
                                     'lsp-clients--clangd-command)
                    :major-modes '(c-mode c++-mode objc-mode)
                    :priority -1
                    :remote? t
                    :server-id 'clangd-remote)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t
  :custom
  (lsp-ui-doc-enable nil)) ; Disable giant hovering pop-up boxes.

;; This is used by LSP for function arguments
(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode))

(use-package cmake-mode :ensure t)
(use-package glsl-mode :ensure t)

(use-package magit
  :ensure t)

(use-package markdown-mode
  :ensure t)

;; Install livedown with:
;; $ npm install -g livedown
(use-package livedown
  :commands livedown-preview
  :load-path "site-lisp/emacs-livedown")

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :config
  (setq TeX-auto-save t)
  (TeX-global-PDF-mode 1)

  (setq-default TeX-engine 'xetex)

  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  (setq preview-scale-function 1.0))

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

(use-package pdf-tools
  :ensure t
  :preface
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)

  ;; Fix blurriness on retina
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

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
  (olivetti-body-width 80)
  (olivetti-enable-visual-line-mode nil))

(use-package sprunge
  :ensure t
  :bind (("C-c q p" . sprunge-dwim))
  :config
  (defun sprunge-dwim ()
    (interactive)
    (if (region-active-p)
        (sprunge-region)
      (sprunge-buffer))))


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
