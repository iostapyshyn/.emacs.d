;;; init.el --- Personal Emacs configuration.       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq custom-file (locate-user-emacs-file "custom.el")) ; Customize is not used

(setq local-init-file (locate-user-emacs-file "local.el"))
(load local-init-file t nil t)

(setq inhibit-startup-message t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Don't pop up the *Warnings* buffer during native compilation
(setq native-comp-async-report-warnings-errors 'silent)

;; No file polution
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Save position in the file
(save-place-mode)

;; Don't expand symlinks when visiting file
(setq find-file-visit-truename nil)

(setq visible-bell nil)
(setq ring-bell-function #'ignore)
(setq confirm-kill-emacs #'yes-or-no-p)
(fset 'yes-or-no-p #'y-or-n-p)

;; Scroll by single lines, not half-screens
(setq scroll-conservatively most-positive-fixnum)
(setq scroll-margin 1)

;; Text
(setq-default truncate-lines nil)
(setq-default fill-column 80)

;; Don't fill with C-u - C-x f
(define-advice set-fill-column (:filter-args (arg) set-fill-column-max)
  (if (equal arg '(-))
      (list most-positive-fixnum)
    arg))

;; Region and transient mark
(setq mark-even-if-inactive nil)
(delete-selection-mode 1)

;; Disabled features
(setq disabled-command-function nil)
(setq enable-remote-dir-locals t)

;; German postfix input method: ae -> ä
(setq default-input-method 'german-postfix)

(when (fboundp #'kill-ring-deindent-mode) ;; emacs 29 compatibility
  (kill-ring-deindent-mode 1))

;; Parenthesis
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(setq show-paren-context-when-offscreen 'overlay)
(show-paren-mode 1)
(electric-pair-mode 1)

(dolist (hook '(lisp-mode-hook emacs-lisp-mode-hook))
  (add-hook hook (lambda () (electric-pair-local-mode -1))))

;; Indentation
(with-eval-after-load "cc-styles"
  ;; Use 4 spaces indentation instead of 5 for k&r
  (setf (cdr (assoc 'c-basic-offset (assoc "k&r" c-style-alist))) 4)
  (setq c-default-style "k&r"))

(with-eval-after-load "c-ts-mode"
  (setq c-ts-mode-indent-style 'k&r
        c-ts-mode-indent-offset 4))

(setq-default indent-tabs-mode nil
              tab-width 8
              css-indent-offset 2
              js-indent-level 2)

(setq-default cursor-type 'bar)


;;; --- Personal custom modes and functions ---
(defun duden (word)
  "Search for the WORD definition on duden.de.  Requires
github.com/radomirbosak/duden."
  (interactive
   (list (read-from-minibuffer "$ duden "
                               (if (region-active-p)
                                   (buffer-substring (region-beginning) (region-end))
                                 (thing-at-point 'word)))))
  (let* ((buffer "*Duden Output*")
         (buffer-name-function (lambda (_) buffer))
         (compilation-buffer-name-function buffer-name-function))
    (compile (concat "duden " word) t)
    (with-current-buffer buffer
      (turn-on-visual-line-mode))))

(global-set-key (kbd "C-c q d") #'duden)

;; Bookmarks in org files may fail to open non-interactively:
(define-advice bookmark-jump (:before (&rest _r) bookmarks-load)
  "Load bookmarks file before trying to jump non-interactively."
  (bookmark-maybe-load-default-file)
  (advice-remove 'bookmark-jump 'bookmark-jump@bookmarks-load))

;; Like mode-line-format-right-align in emacs 30, but more precise
(defun mode-line-render (left right)
  "Return LEFT and RIGHT aligned in the mode-line respectively."
  (let* ((left (format-mode-line left))
         (right (format-mode-line right))
         (box-width (if-let (((display-graphic-p))
                             (box (face-attribute 'mode-line :box))
                             (width (plist-get box :line-width)))
                        (abs (or (car-safe width) width)) 0))
         (display `(space :align-to (+ (- scroll-bar
                                          ,(string-width right)        ; chars
                                          (,box-width))                ; px
                                       (,(window-scroll-bar-width))))) ; px
         (space (propertize " " 'display display)))
    (replace-regexp-in-string "%" "%%" (concat left space right))))

(defface project-mode-line-buffer-id
  '((t (:height 0.9 :inherit shadow)))
  "Face used for project part of buffer identification in the mode line.")

(defun project-mode-line-buffer-identification ()
  "When in a project, indicate project name and path to file."
  (if-let ((dir (cond ((file-remote-p default-directory) nil)
                      ((derived-mode-p 'dired-mode)
                       (file-name-parent-directory default-directory))
                      ((derived-mode-p 'vterm-mode) default-directory)
                      ((buffer-file-name) default-directory)))
           (dir (expand-file-name dir))
           (proj (project-current))
           (proj (expand-file-name (project-root proj)))
           ((string-prefix-p proj dir)) ;; Is dir actually in the project?
           (prefix (file-relative-name dir (file-name-parent-directory proj))))
      (cons (propertize prefix 'face 'project-mode-line-buffer-id)
            mode-line-buffer-identification)
    mode-line-buffer-identification))

(setq-default mode-line-format '((:eval (mode-line-render
                                         (list " "
                                               mode-line-mule-info
                                               mode-line-modified
                                               mode-line-remote
                                               mode-line-frame-identification
                                               mode-line-position " "
                                               (project-mode-line-buffer-identification))
                                         (list mode-line-misc-info
                                               '(vc-mode vc-mode)
                                               " "
                                               mode-line-modes)))))
(setq mode-line-compact 'long)

(setq frame-title-format '("%b - " invocation-name "@" system-name))

;; Override which-func-update with version that updates all windows
(define-advice which-func-update (:override (&rest _args) which-func-update-all)
  (walk-windows 'which-func-update-1 nil 'visible))

(setq which-func-unknown "…")
(which-function-mode 1)
(column-number-mode 1)

;; Make sure the time stamps are formatted in English across the systems
(setq system-time-locale "C")

(dolist (k '("M-<down>" "M-n"))
  (global-set-key (kbd k) #'scroll-up-line))
(dolist (k '("M-<up>" "M-p"))
  (global-set-key (kbd k) #'scroll-down-line))

(global-set-key (kbd "C-M-<backspace>") #'backward-kill-sexp)
(global-set-key (kbd "ESC M-DEL")       #'backward-kill-sexp) ; for terminal

(global-set-key (kbd "M-z")   #'zap-up-to-char)
(global-set-key (kbd "C-M-z") #'zap-to-char)

(global-set-key (kbd "C-<tab>") #'completion-at-point)

(setq compilation-scroll-output t)
(global-set-key (kbd "C-c c") #'compile)

(global-set-key (kbd "M-g o") #'ff-find-other-file)
(global-set-key (kbd "M-g i") #'imenu)

(global-set-key (kbd "C-h C-m") #'man)
(setq Man-notify-method 'pushy)

(global-set-key (kbd "<menu>") 'mode-specific-command-prefix)

(defun toggle-window-no-delete (&optional window)
  "Invert the no-delete-other-windows parameter of the WINDOW."
  (interactive)
  (let ((no-delete (not (window-parameter window 'no-delete-other-windows))))
    (set-window-parameter window 'no-delete-other-windows no-delete)
    (message (if no-delete
                 "Window will not be deleted on delete-other-windows"
               "Window will be deleted on delete-other-windows"))))

(unless (fboundp #'toggle-window-dedicated) ;; emacs 29 compatibility
  (defun toggle-window-dedicated (&optional window)
    "Invert the dedicatation of the WINDOW to its buffer."
    (interactive)
    (let ((dedicated (not (window-dedicated-p window))))
      (set-window-dedicated-p window dedicated)
      (message (if dedicated
                   "Window is now dedicated to its buffer"
                 "Window is no longer dedicated to its buffer")))))

(global-set-key (kbd "C-c w d") #'toggle-window-dedicated)
(global-set-key (kbd "C-c w s") #'toggle-window-no-delete)

(global-set-key (kbd "C-x C-d") #'dired)
(global-set-key (kbd "C-x C-b") #'ibuffer)

(global-set-key (kbd "C-x C-c") #'save-buffers-kill-emacs)

(defun turn-on-indent-tabs-local-mode ()
  "Turn on `indent-tabs-mode' locally."
  (interactive)
  (setq-local indent-tabs-mode t))

(defun comment-use-cpp-syntax ()
  "Set up newcomment to uss CPP multiline comments."
  (setq-local comment-start "/* "
              comment-add 0
              comment-start-skip "\\(?://+\\|/\\*+\\)\\s *"
              comment-end-skip "[ 	]*\\(\\s>\\|\\*+/\\)"
              comment-end " */"))

(with-eval-after-load "asm-mode"
  (add-hook 'asm-mode-hook #'turn-on-indent-tabs-local-mode)
  (add-hook 'asm-mode-hook #'comment-use-cpp-syntax)
  (add-hook 'asm-mode-hook
            (lambda () ; get rid of really annoying comment behavior
              (local-unset-key (vector asm-comment-char)))))

(defun move-back-to-indentation-or-line (arg)
  "Move point to first non-whitespace character or beginning of the line.

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there."
  (interactive "^p")
  (or arg (setq arg 1))

  (let ((orig (point)))
    (if (/= arg 1) ;; Original behaviour of beginning-of-line
	(let ((line-move-visual nil))
	  (line-move (1- arg) t)))
    (back-to-indentation)
    (when (<= orig (point))
      (beginning-of-line))))

(defun move-end-of-line-comment (arg)
  "Move to the last non-comment character or end of the line.

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there."
  (interactive "^p")
  (or arg (setq arg 1))

  (let ((orig (point)))
    (if (/= arg 1) ;; Original behaviour of end-of-line
	(let ((line-move-visual nil))
	  (line-move (1- arg) t)))
    (when-let ((comment-start (comment-search-forward (line-end-position) t)))
      (goto-char comment-start)
      (skip-chars-backward " \t"))
    (when (>= orig (point))
      (end-of-line))))

(global-set-key [remap move-beginning-of-line] #'move-back-to-indentation-or-line)
(global-set-key [remap move-end-of-line]       #'move-end-of-line-comment)

;; Complementary to C-x x t (toggle-truncate-lines)
(global-set-key (kbd "C-x x v") #'visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(when (fboundp #'visual-wrap-prefix-mode) ;; emacs 29 compatibility
  (global-set-key (kbd "C-x x w") #'visual-wrap-prefix-mode)
  (global-visual-wrap-prefix-mode))

(setq my-frame-scale-factor
      (if (fboundp #'frame-scale-factor)
          (frame-scale-factor) 1))

(require 'treesit)

(add-to-list 'treesit-language-source-alist
             '(typst "https://github.com/uben0/tree-sitter-typst"))

(defconst treesit-langs '(("c" . c) ("c++" . cpp) ("bash" . bash)
                          ("python" . python) ("rust" . rust) ("typst" . typst)))

(defun treesit-populate-mode-mapping ()
  "Populate `major-mode-remap-alist' according to `treesit-langs'."
  (interactive)
  (when (and (fboundp #'treesit-available-p) (treesit-available-p))
    (dolist (lang treesit-langs)
      (when-let (((treesit-ready-p (cdr lang) t))
                 (mode (intern (concat (car lang) "-mode")))
                 (ts-mode (intern (concat (car lang) "-ts-mode"))))
        (add-to-list 'major-mode-remap-alist (cons mode ts-mode))))))

(defun treesit-install-language-grammars ()
  "Install tree-sitter grammars for languages in `treesit-langs'."
  (interactive)
  (dolist (lang treesit-langs)1
    (unless (treesit-ready-p (cdr lang) t)
      (treesit-install-language-grammar (cdr lang))))
  (treesit-populate-mode-mapping))

(treesit-populate-mode-mapping)

(defun increment-dec-at-point (n)
  "Add N (or 1 if nil) to decimal at point."
  (interactive "p*")
  (save-match-data
    (if-let (((thing-at-point-looking-at "-?[0-9]+"))
             (match (match-string 0)))
        (replace-match (format (format "%%0%dd" (length match))
                               (+ (or n 1) (string-to-number match 10))))
      (message "No decimal at point"))))

(defun increment-hex-at-point (n)
  "Add N (or 1 if nil) to hexadecimal at point."
  (interactive "p*")
  (save-match-data
    (if-let (((thing-at-point-looking-at "[0-9a-fA-F]+"))
             (match (match-string 0)))
        (replace-match (format (format "%%0%dx" (length match))
                               (+ (or n 1) (string-to-number match 16))))
      (message "No hexadecimal at point"))))

(global-set-key (kbd "C-c   +") #'increment-dec-at-point)
(global-set-key (kbd "C-c 6 +") #'increment-hex-at-point)

(defun alist-merge (old new)
  "Update OLD alist with values from NEW alist, with NEW taking precedence."
  (dolist (pair new)
    (setq old (cons pair (assq-delete-all (car pair) old))))
  old)


;;; --- Packages ---
(eval-when-compile
  (require 'package)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                           ("melpa" . "https://melpa.org/packages/")))
  (setq package-archive-priorities '(("gnu" . 20)
                                     ("nongnu" . 10)
                                     ("melpa" . 0)))
  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (require 'bind-key))
(setq use-package-always-defer t)

(use-package dash :ensure t)

(use-package pp
  :bind* (([remap eval-last-sexp]  . pp-eval-last-sexp)
          ([remap eval-expression] . pp-eval-expression)))

(use-package display-fill-column-indicator
  :demand t
  :config
  (setq global-display-fill-column-indicator-modes
        '(prog-mode message-mode (not special-mode)))
  (global-display-fill-column-indicator-mode))

(use-package frame
  :config
  (when (fboundp #'undelete-frame-mode)
    (undelete-frame-mode))
  (setq window-divider-default-right-width 1)
  (window-divider-mode))

(use-package mouse
  :config
  (when (fboundp #'context-menu-mode)
    (context-menu-mode)))

(use-package window
  :bind* (("C-," . previous-buffer)
          ("C-." . next-buffer)
          ("C-;" . other-window)))

(use-package windmove
  :demand t
  :config
  (windmove-default-keybindings))

(use-package tab-bar
  :bind* (("M-[" . tab-previous)
          ("M-]" . tab-next))
  :config
  ;; Like tab-bar-select-tab-modifiers but won't be overriden.
  (cl-loop for i from 1 to 9
           do (bind-key* (format "M-%d" i) #'tab-bar-select-tab))
  (bind-key* "M-0" #'tab-recent)

  (setq tab-bar-tab-hints t)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-format '(tab-bar-format-tabs-groups
                         tab-bar-format-history
                         tab-bar-separator)))

(use-package bufferlo
  :ensure t
  :demand t
  :bind* (("C-c b q"        . bufferlo-bury)
          ("C-c b 1"        . bufferlo-clear)
          ("C-c b <delete>" . bufferlo-kill-buffers)
          ("C-x t <delete>" . bufferlo-tab-close-kill-buffers)
          ("C-x 5 <delete>" . bufferlo-delete-frame-kill-buffers))
  :config
  (setq tab-bar-new-tab-choice "*scratch*")
  (defun frame-set-scratch-buffer (frame)
    (with-selected-frame frame
      (switch-to-buffer "*scratch*")))
  (add-hook 'after-make-frame-functions #'frame-set-scratch-buffer)
  (bufferlo-mode 1))

(use-package wgrep
  :ensure t)

(use-package minions
  :ensure t
  :demand t
  :config
  (add-to-list 'minions-prominent-modes 'lsp-mode)
  (add-to-list 'minions-prominent-modes 'envrc-mode)
  (add-to-list 'minions-prominent-modes 'vterm-copy-mode)
  (minions-mode 1))

(use-package project
  :bind-keymap* ("C-x p" . project-prefix-map)
  :bind (([remap compile] . compile-maybe-project)
         :map project-prefix-map ("g" . project-magit)) ;; TODO: Add project-vterm?
  :config
  (setq project-switch-commands '((project-find-file "Find file")
                                  (project-dired "Dired")
                                  (project-compile "Compile")
                                  (project-magit "Magit")))
  ;; Credit to karthink for this .project detection snippet below
  (setq project-local-identifier ".project")
  (cl-defmethod project-root ((project (head local)))
    (cdr project))
  (defun project-try-local (dir)
    "Determine if DIR is a non-VC project.
DIR must include a .project file to be considered a project."
    (if-let ((root (locate-dominating-file dir project-local-identifier)))
        (cons 'local root)))
  (add-hook 'project-find-functions #'project-try-local)

  (defun compile-maybe-project ()
    "Call `project-compile' if buffer belongs to a project or `compile' otherwise."
    (interactive)
    (if (and (fboundp #'project-current)
             (project-current nil))
        (call-interactively #'project-compile)
      (call-interactively #'compile)))
  (defun project-magit ()
    "Show the status of the project repository in a magit buffer."
    (interactive)
    (magit-status (project-root (project-current t)))))

(use-package compile
  :bind (("C-c x" . pop-to-compilation-buffer)
         (:map compilation-mode-map
               ("x" . quit-window)
               ("c" . recompile)))
  :config
  (defun pop-to-compilation-buffer ()
    "Display the compilation buffer."
    (interactive)
    (when-let ((buffer (get-buffer "*compilation*")))
      (pop-to-buffer buffer))))

(use-package recentf
  :demand t
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (recentf-mode 1)
  (with-eval-after-load "bookmark"
    (add-to-list 'recentf-exclude (regexp-quote (expand-file-name bookmark-default-file))))
  (setq recentf-max-saved-items nil
        recentf-auto-cleanup 'never
        recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"
                          "\\.newsrc\\(\\|\\.eld\\)\\'")))

(use-package calc
  :config
  (setq-default calc-multiplication-has-precedence nil
                calc-kill-line-numbering nil))

(use-package epg
  :config
  (setq epg-pinentry-mode 'loopback)
  (fset 'epg-wait-for-status 'ignore))


;;; --- Org Mode and Calendar ---
(use-package calendar
  :config
  (setq calendar-latitude 52.38
        calendar-longitude 9.69
        calendar-week-start-day 1))

(use-package password-cache
  :config
  (setq password-cache-expiry 600))

(use-package auth-source
  :config
  ;; Don't even dare to write plain-text .authinfo
  (setq auth-sources (list (locate-user-emacs-file "authinfo.gpg")))
  (setq auth-source-save-behavior nil)
  (auth-source-pass-enable)

  (define-advice auth-source-pass-search (:before-while (&rest _) open-coffin)
    (let ((coffin-path (concat
                        (file-name-as-directory auth-source-pass-filename)
                        ".coffin/coffin.tar.gpg")))
      (or (not (file-exists-p coffin-path))
          (equal (call-process "pass" nil nil nil "open" "-t" "3min") 0)))))

(use-package gnus
  :config
  (setq gnus-select-method '(nnnil "")
        gnus-init-file (locate-user-emacs-file "gnus.el")
        gnus-inhibit-mime-unbuttonizing t
        gnus-always-read-dribble-file t
        gnus-refer-thread-limit 5000
        gnus-large-newsgroup 1000)

  (setq gnus-summary-line-format "%1{%U%R%O %4k %&user-date;%*%} │ %2{%f%}%-42= │ %I%S\n")
  (setq gnus-thread-indent-level 2)

  (setq gnus-user-date-format-alist
        '(((gnus-seconds-today) . "  %k:%M")
          (31536000 . " %e %b")
          (t . "%Y-%m")))

  (defun gnus-easyimap-add (name address server &rest keyword-args)
    "Add secondary nnimap method to Gnus with sane defaults.

Add an nnimap method called NAME (defaults to ADDRESS if nil)
with user ADDRESS on SERVER.  KEYWORD-ARGS might include :method,
:parameters, and :posting-style to allow customizing
`gnus-secondary-select-methods', `gnus-parameters', and
`gnus-posting-styles', respectively."
    (let* ((name (or name address))
           (method (alist-merge
                    `((nnimap-user ,address)
                      (nnimap-address ,server)
                      (nnimap-server-port "imaps"))
                    (plist-get keyword-args :method)))
           (parameters (alist-merge
                        `((gcc-self ,(concat "nnimap+" name ":Sent")))
                        (plist-get keyword-args :parameters)))
           (posting-style (alist-merge
                           `((address ,address)
                             (X-Message-SMTP-Method ,(concat "smtp " server " 587")))
                           (plist-get keyword-args :posting-style))))
      (add-to-list 'gnus-secondary-select-methods
                   (append `(nnimap ,name) method))
      (add-to-list 'gnus-parameters
                   (cons (concat "^nnimap\\+" (regexp-quote name) ":.*")
                         parameters))
      (add-to-list 'gnus-posting-styles
                   (cons (concat "^nnimap\\+" (regexp-quote name) ":.*")
                         posting-style))))

  (defun gnus-easyimap-organize-topics (parent)
    "Organize nnimap groups under topics for each method."
    (interactive (list (read-string "Parent topic: " "Mail")))
    (unless (gnus-topic-find-topology parent)
      (gnus-topic-create-topic parent nil))
    (dolist (method gnus-secondary-select-methods)
      (when-let (((equal (car method) 'nnimap))
                 (name (cadr method))
                 (topic name))
        (if (gnus-topic-find-topology topic)
            (gnus-topic-move topic parent)
          (gnus-topic-create-topic topic parent))
        (gnus-topic-move-matching (concat "^nnimap\\+" (regexp-quote name) ":.*") topic))))

  ;; Useful default servers
  ;; (add-to-list 'gnus-secondary-select-methods
  ;;              '(nntp "gwene" (nntp-address "news.gwene.org")))
  (add-to-list 'gnus-secondary-select-methods
               '(nntp "gmane" (nntp-address "news.gmane.io"))))

(use-package smtpmail
  :config
  (setq smtpmail-servers-requiring-authorization ".*"
        smtpmail-debug-info t
        smtpmail-smtp-service 587)

  ;; Be explicit about server/port
  (define-advice smtpmail-send-it (:before () reset-server)
    (setq smtpmail-smtp-server
          (read-string "Outgoing SMTP mail server: " smtpmail-smtp-server)

          smtpmail-smtp-service
          (read-number (format "Port number to use when contacting %s: "
			       smtpmail-smtp-server)
                       smtpmail-smtp-service)

          smtpmail-smtp-user
          (cadr (mail-extract-address-components (message-fetch-field "From"))))))

(use-package mml
  :config
  (when (version< emacs-version "30")
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=67931
    (define-advice mml-smime-openssl-sign (:override (cont) fix-openssl-sign)
      (smime-sign-buffer (cdr (assq 'keyfile cont)))
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t)
        (replace-match "\n" t t))
      (goto-char (point-max))))

  (setq mml-smime-use 'openssl))

(use-package message
  :config
  ;; Extract Message-ID domain from the From field, if present
  (define-advice message-make-fqdn (:before-until (&rest _args) from-fqdn)
    (save-restriction
      (message-narrow-to-headers)
      (when-let ((from-mail (message-fetch-field "from" t))
                 ((string-match "@\\([.[:alnum:]-]*\\)" from-mail)))
        (match-string 1 from-mail))))

  (defun message-recipients-contain (addr)
    (save-restriction
      (message-narrow-to-headers)
      (let ((recipients (message-all-recipients))
            (addr (mail-extract-address-components addr)))
        (seq-contains-p recipients addr
                        (lambda (x y) (string-equal (cadr x) (cadr y)))))))

  (defun message-add-self-to-bcc ()
    (save-excursion
      (save-restriction
        (message-narrow-to-headers)
        (let ((bcc (message-fetch-field "Bcc"))
              (from (message-fetch-field "From")))
          (unless (message-recipients-contain from)
            (widen)
            (message-goto-bcc)
            (when bcc
              (insert ", "))
            (insert from))))))

  ;; (add-hook 'message-send-hook #'message-add-self-to-bcc)
  (setq message-send-mail-function #'smtpmail-send-it))

(use-package tramp
  :config
  (setq tramp-completion-use-auth-sources nil
        tramp-show-ad-hoc-proxies t)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package org
  :preface
  (defun org-set-directory (path)
    (interactive "D")
    (setq org-directory (file-name-as-directory path))
    (setq org-agenda-files (list org-directory)
          org-archive-location (concat org-directory "archive/%s::datetree/")))
  (defun org-visit-directory (arg)
    (interactive "P")
    (when arg
      (call-interactively #'org-set-directory))
    (push-mark)
    (find-file org-directory))
  :bind*
  (("C-c a" . org-agenda)
   ("C-c i" . org-visit-directory))
  :bind
  (:map org-mode-map
        ("C-c C-." . org-time-stamp-inactive))
  :config
  (org-set-directory org-directory)
  ;; (setq-default org-display-custom-times t)
  ;; (setq org-time-stamp-custom-formats '("<%a %d %b %Y>" . "<%a %d %b %Y %H:%M>"))

  (setq org-agenda-prefix-format '((agenda . " %i %-12:c%-12t% s")
                                   (todo   . " %i %-12:c")
                                   (tags   . " %i %-12:c")
                                   (search . " %i %-12:c")))

  (setq org-agenda-start-on-weekday 1)

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

  (setq org-agenda-custom-commands '(("a" "Agenda and all TODOs"
                                      ((agenda "")
                                       (alltodo ""))
                                      nil)))

  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-return-follows-link t)
  (setq org-startup-indented t))

(use-package python
  :bind (:map python-mode-map
              ("C-c C-c" . (lambda () (interactive) (python-shell-send-buffer t)))))

(use-package eww
  :bind (("C-h 7" . eww-man7-index))
  :config
  (setq eww-search-prefix "https://lite.duckduckgo.com/html/?q=")
  (setq eww-auto-rename-buffer t)

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

  ;; Show links in imenu
  (defun eww-mode--imenu ()
  (when (derived-mode-p 'eww-mode)
    (let ((links))
      (save-excursion
        (goto-char (point-max))
        (while (text-property-search-backward 'shr-url nil nil t)
          (when (and (get-text-property (point) 'shr-url)
                     (not (get-text-property (point) 'eww-form)))
            (push (cons (format "%s @ %s"
                                (button-label (point))
                                (propertize (get-text-property (point) 'shr-url) 'face 'link))
                        (point))
                  links))))
      links)))

  (add-hook 'eww-mode-hook (lambda ()
                             (setq imenu-create-index-function #'eww-mode--imenu))))

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

(use-package xref
  :config
  ;; Also used by dired and project
  (setq xref-search-program 'ripgrep))

(use-package dired
  :custom
  ;; (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  (dired-recursive-copies 'top)
  (dired-recursive-deletes 'top)
  (dired-listing-switches "-lah")
  (dired-auto-revert-buffer t)
  (dired-maybe-use-globstar t)
  (dired-isearch-filenames 'dwim)
  :bind (:map dired-mode-map
              ("C-x M-o" . my/dired-omit-switch))
  :config
  (require 'dired-x)
  (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

  ;; Remember omit state across buffers
  (defvar my/dired-omit nil "Whether `dired-omit-mode' should be enabled.")
  (defun my/dired-omit-apply ()
    (dired-omit-mode (if my/dired-omit nil -1)))
  (defun my/dired-omit-switch ()
    "Switch `dired-omit-mode' state.  See `my/dired-omit'."
    (interactive)
    (setq my/dired-omit (not my/dired-omit))
    (my/dired-omit-apply))

  (add-hook 'dired-mode-hook 'my/dired-omit-apply))

(use-package diredfl
  :after dired
  :ensure t
  :hook (dired-mode . diredfl-mode))

(use-package dired-du
  :after dired
  :ensure t
  :bind (:map dired-mode-map
              ("C-x M-r" . dired-du-mode)
              ("* N"     . dired-du-count-sizes))
  :config
  (setq dired-du-size-format t))

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

(use-package flymake
  :bind* (("C-c ! !" . flymake-mode)
          (:map flymake-mode-map
                ("C-c ! l" . flymake-show-buffer-diagnostics)
                ("C-c ! n" . flymake-goto-next-error)
                ("C-c ! p" . flymake-goto-prev-error)
                ("C-c ! b" . flymake-running-backends)
                ("C-c ! s" . flymake-start)))
  :hook ((prog-mode LaTeX-mode) . flymake-mode))

(use-package flymake-diagnostic-at-point
  :ensure t
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode)
  :config
  (setq flymake-diagnostic-at-point-error-prefix ""
        flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer
        ;; Take priority over eldoc with its 0.5s delay
        flymake-diagnostic-at-point-timer-delay 0.51))

;; Make key-bindings work in other keyboard layouts
(use-package reverse-im
  :ensure t
  :demand t
  :custom
  (reverse-im-input-methods '("ukrainian-computer" "german"))
  :config
  (reverse-im-mode t))

;; (use-package undo-tree
;;   :ensure t
;;   :demand t
;;   :config
;;   (setq undo-tree-auto-save-history t
;;         undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))
;;         undo-tree-visualizer-diff t
;;         undo-tree-visualizer-timestamps t
;;         undo-tree-enable-undo-in-region t)
;;   (global-undo-tree-mode))

(use-package vundo
  :ensure t
  :bind* ("C-x u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-fu-session
  :ensure t
  :demand t
  :config
  (undo-fu-session-global-mode))

(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))


;;; --- Completion ---
(setq enable-recursive-minibuffers t)
(setq read-minibuffer-restore-windows nil)
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
  :bind (:map vertico-map
              ("<prior>" . vertico-scroll-down)
              ("<next>"  . vertico-scroll-up))
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :demand t
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))
                                        (lsp-capf (styles orderless))))
  (add-to-list 'orderless-matching-styles 'orderless-initialism))

(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("M-s l"                                 . consult-line)
         ("M-s g"                                 . consult-git-grep)
         ("M-s r"                                 . consult-ripgrep)
         ("M-g m"                                 . consult-mark)
         ([remap switch-to-buffer]                . consult-buffer)
         ([remap switch-to-buffer-other-window]   . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]    . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-tab]      . consult-buffer-other-tab)
         ([remap recentf-open-files]              . consult-recent-file)
         ([remap bookmark-jump]                   . consult-bookmark)
         ;; ([remap load-theme]                   . consult-theme)
         ([remap goto-line]                       . consult-goto-line)
         ;; ([remap man]                          . consult-man)
         ([remap yank-pop]                        . consult-yank-pop)
         ([remap imenu]                           . consult-imenu)
         ([remap flymake-show-buffer-diagnostics] . consult-flymake)
         ([remap vterm]                           . consult-vterm)
         ([remap eshell]                          . consult-eshell))
  :bind* ("<insert>" . consult-buffer)
  :init
  (setq completion-in-region-function #'consult-completion-in-region)
  (add-hook 'gud-mode-hook
            (lambda ()
              (setq-local completion-in-region-function #'completion--in-region)
              (setq-local completion-styles '(basic partial-completion))))
  :config
  ;; xref
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Less preview by default
  (consult-customize
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "M-.")

  ;; Useful buffer sources
  (defvar consult--source-eshell-buffer
    (list :name     "Eshell Buffer"
          :hidden   t
          :narrow   ?s
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create (concat "*eshell*<" name ">"))
              (unless (derived-mode-p 'eshell-mode)
                (eshell-mode))
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (consult--buffer-query :mode 'eshell-mode :as #'buffer-name))))
  (defvar consult--source-vterm-buffer
    (list :name     "Vterm Buffer"
          :hidden   t
          :narrow   ?z
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create (concat "*vterm*<" name ">"))
              (unless (derived-mode-p 'vterm-mode)
                (vterm-mode))
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (consult--buffer-query :mode 'vterm-mode :as #'buffer-name))))
  (defvar consult--source-dired-buffer
    (list :name     "Dired Buffer"
          :hidden   t
          :category 'buffer
          :narrow   ?d
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new      #'ignore
          :items
          (lambda ()
            (consult--buffer-query :mode 'dired-mode :as #'buffer-name))))

  ;; Bufferlo integration
  (defvar consult--source-buffer-hidden
    (append '(:hidden t) consult--source-buffer))
  (defvar consult--source-local-buffer
    (list :name     "Local Buffer"
          :category 'buffer
          :narrow   ?l
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :default  t
          :items
          (lambda ()
            (consult--buffer-query :predicate #'bufferlo-local-buffer-p
                                   :sort 'visibility
                                   :as #'buffer-name))))

  (setq consult--source-buffer-hidden (plist-put consult--source-buffer-hidden :narrow ? )
        consult--source-hidden-buffer (plist-put consult--source-hidden-buffer :narrow ?.))

  (setq consult-buffer-sources `(consult--source-local-buffer
                                 consult--source-buffer-hidden
                                 consult--source-hidden-buffer
                                 consult--source-project-buffer-hidden
                                 consult--source-modified-buffer
                                 consult--source-dired-buffer
                                 consult--source-vterm-buffer
                                 consult--source-eshell-buffer))

  ;; Vterm
  (defvar consult--source-vterm-local-buffer
    (list :name     "Local Vterm Buffer"
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create (concat "*vterm*<" name ">"))
              (unless (derived-mode-p 'vterm-mode)
                (vterm-mode))
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (consult--buffer-query :predicate #'bufferlo-local-buffer-p :mode 'vterm-mode :as #'buffer-name :sort 'visibility))))
  (defun consult-vterm (&optional arg)
    (interactive "P")
    (if arg
        (vterm arg)
      (consult--multi `(consult--source-vterm-local-buffer
                        ,(plist-put consult--source-vterm-buffer :narrow ? )))))

  ;; Eshell
  (defvar consult--source-eshell-local-buffer
    (list :name     "Local Eshell Buffer"
          :category 'buffer
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create (concat "*eshell*<" name ">"))
              (unless (derived-mode-p 'eshell-mode)
                (eshell-mode))
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (consult--buffer-query :predicate #'bufferlo-local-buffer-p :mode 'eshell-mode :as #'buffer-name :sort 'visibility))))

  (defun consult-eshell (&optional arg)
    (interactive "P")
    (if arg
        (eshell arg)
      (consult--multi `(consult--source-eshell-local-buffer
                        ,(plist-put consult--source-eshell-buffer :narrow ? ))))))


(use-package embark
  :ensure t
  :bind (("C-c e" . embark-act)
         ("C-c E" . embark-act-all)))

(use-package embark-consult
  :ensure t
  :demand t
  :after (consult embark))

(use-package expand-region
  :ensure t
  :bind* (("C-=" . er/expand-region)))

(use-package avy
  :ensure t
  :bind* (("C-j" . avy-goto-word-1)))

(use-package winner
  :ensure t
  :demand t
  :config
  (winner-mode))

;; Get environment variables
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; --- Color theme ---
(use-package custom-css
  :if (eq x-toolkit-scroll-bars 'gtk)
  :demand t
  :load-path "lisp/custom-css"
  :config
  (custom-css-scroll-bar-mode))

(use-package on-demand-scroll-bar
  :demand t
  :load-path "lisp/on-demand-scroll-bar"
  :config
  (on-demand-scroll-bar-mode))

(setq custom-safe-themes t)

(defvar after-load-theme-hook nil "Functions called after loading a theme.")
(define-advice load-theme (:after (&rest _args) run-after-load-theme-hook)
  (run-hooks 'after-load-theme-hook))

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

(defun theme-fixup-fill-column-indicator ()
  "Match fill column indicator's background with default one."
  ;; NOTE: On some systems, the indicator is one pixel thin if :height is 1.
  (set-face-attribute 'fill-column-indicator nil
                      :background 'unspecified
                      :height     'unspecified
                      :inherit    'default))

(defun theme-add-mode-line-padding ()
  "Prettify the mode-line by adding padding to it."
  (let* ((mode-line-border (cons 1 (truncate 6 my-frame-scale-factor)))
         (bg-active   (face-attribute 'mode-line :background))
         (bg-inactive (face-attribute 'mode-line-inactive :background))
         (box-active   (list :line-width mode-line-border :color bg-active))
         (box-inactive (list :line-width mode-line-border :color bg-inactive)))
    (set-face-attribute 'mode-line          nil :box box-active)
    (set-face-attribute 'mode-line-inactive nil :box box-inactive)))

(defun theme-set-scroll-bar-background ()
  "Set scroll-bar background to default background."
  (set-face-attribute 'scroll-bar nil :background
                      (face-attribute 'default :background)))

(add-hook 'after-load-theme-hook #'theme-fixup-fill-column-indicator)
(add-hook 'after-load-theme-hook #'theme-add-mode-line-padding)
(add-hook 'after-load-theme-hook #'theme-set-scroll-bar-background)

(use-package modus-themes
  :ensure t
  :bind ("C-c t m" . modus-themes-toggle)
  :config
  (setq modus-themes-common-palette-overrides '((bg-region bg-sage)
                                                (fg-region unspecified)
                                                (fringe unspecified))
        modus-themes-italic-constructs t
        modus-themes-org-blocks 'tinted-background
        modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))

  (defun modus-theme-diff-hl-fringes ()
    (modus-themes-with-colors
      (set-face-attribute 'diff-hl-change nil :background bg-main :foreground bg-changed-fringe)
      (set-face-attribute 'diff-hl-insert nil :background bg-main :foreground bg-added-fringe)
      (set-face-attribute 'diff-hl-delete nil :background bg-main :foreground bg-removed-fringe)))
  (with-eval-after-load "diff-hl"
    (modus-theme-diff-hl-fringes)
    (add-hook 'after-load-theme-hook #'modus-theme-diff-hl-fringes)))

(global-set-key (kbd "C-c t t") #'load-theme)
(load-theme 'modus-vivendi-tinted t)

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode
          lisp-mode) . rainbow-delimiters-mode))

(use-package eshell
  :bind (("C-z" . eshell))
  :config
  (defun eshell/last-remote ()
    (when-let ((r (if-let ((base (file-remote-p default-directory)))
                      base
                    (seq-some 'file-remote-p (ring-elements eshell-last-dir-ring)))))
      ;; remove the trailing semicolon, helps to multi-hop
      ;; i.e. cd $r|sudo::
      (replace-regexp-in-string ":\\'" "" r)))
  (require 'esh-var)
  (add-to-list 'eshell-variable-aliases-list '("r" eshell/last-remote nil t))

  (setq eshell-destroy-buffer-when-process-dies t)
  (defalias 'eshell/v #'eshell-exec-visual)
  (defalias 'eshell/ff #'find-file)
  (defalias 'eshell/clear #'eshell/clear-scrollback))

(use-package vterm
  :load-path "lisp/emacs-libvterm"
  :commands (term vterm-mode)
  :bind (("C-x C-z" . vterm)
         (:map vterm-mode-map
               ("C-c TAB"   . vterm-cd-saved-directory)
               ("C-c C-x"   . vterm-send-C-x)
               ("C-<up>"    . vterm-copy-mode)
               ("M-p"       . vterm-send-C-p)
               ("M-n"       . vterm-send-C-n))
         (:map vterm-copy-mode-map
               ("C-<up>"    . vterm-previous-prompt)
               ("C-<down>"  . vterm-next-prompt-or-copy)))
  :config
  (define-advice vterm
      (:before (&optional _arg) save-directory)
    (setq vterm-saved-directory default-directory))
  (define-advice consult-vterm
      (:before (&optional _arg) save-directory)
    (setq vterm-saved-directory default-directory))

  (add-hook 'vterm-mode-hook
            (lambda () (setq cursor-type 'box)))

  (defun vterm-next-prompt-or-copy (n)
    (interactive "p")
    (let ((old-point (point)))
      (vterm-next-prompt n)
      (when (= (point) old-point)
        (vterm-copy-mode -1))))

  (defun vterm-cd-saved-directory ()
    (interactive)
    (when (bound-and-true-p vterm-saved-directory)
      (vterm-insert vterm-saved-directory)))

  (setq vterm-max-scrollback 50000)
  (setq vterm-timer-delay 0.01)
  (setq vterm-kill-buffer-on-exit t))

(use-package eshell-vterm
  :load-path "lisp/eshell-vterm"
  :demand t
  :after eshell
  :config
  (require 'vterm)
  (eshell-vterm-mode))

(use-package magit-todos
  :ensure t
  :preface
  ;; quite buggy -- trigger manually
  (with-eval-after-load "magit-status"
    (transient-append-suffix #'magit-status-jump
      '(0 -1) '[("t" "List todos" magit-todos-list)])))

(use-package hl-todo
  :ensure t
  :demand t
  :config
  (global-hl-todo-mode))

;; Smart trailing whitespace trimming
(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.njk\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package dumb-jump
  :demand t :after xref
  :ensure t
  :config
  ;; Fix dumb-jump not using project.el:
  (define-advice dumb-jump-get-project-root (:override (filepath) dumb-jump-project)
    (s-chop-suffix
     "/"
     (expand-file-name
      (project-root (project-current t (file-name-directory filepath))))))

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 50))

(use-package copilot
  :load-path "lisp/copilot.el"
  :bind ("C-c p" . copilot-call)
  :config
  (defun copilot-call ()
    (interactive)
    (copilot-mode 1)
    (copilot-complete)
    (copilot-transient))

  ;; Todo make on-demand
  (transient-define-prefix copilot-transient ()
    ["Select"
     ("l" "List" copilot-panel-complete :transient t)
     ("p" "Previous" copilot-previous-completion :transient t)
     ("n" "Next" copilot-next-completion :transient t)]
    ["Actions"
     ("a" "Accept" copilot-accept-completion)]))

;; How does this get along with dtrt-mode?
(use-package editorconfig
  :ensure t
  :demand t
  :config
  (editorconfig-mode 1))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions))
  :config
  ;; project.el does not resolve symlinks:
  ;; If project root path includes a symlink, jump to definition fails to fire up eglot
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'highlight)
  ;; Keep inlay hints off
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd" "--header-insertion=never")))
  (setq-default eglot-workspace-configuration
                '((haskell (plugin (stan (globalOn . :json-false)))))))

;; This is used by LSP/Eglot for function arguments
(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode))

(use-package dtrt-indent
  :ensure t
  :demand t
  :config
  (dtrt-indent-global-mode))

(use-package cmake-mode :ensure t)

(use-package with-editor
  :ensure t
  :demand t
  :config
  (shell-command-with-editor-mode))

(use-package magit
  :ensure t
  :config
  (defun set-commit-fill-column ()
    (set-fill-column 72))
  (add-hook 'git-commit-mode-hook #'set-commit-fill-column))

(use-package fringe-helper
  :ensure t)

(use-package diff-hl
  :ensure t
  :demand t
  :config
  (require 'fringe-helper)
  (defun my-diff-hl-fringe-bmp (_type _pos)
    (fringe-helper-define 'my-diff-hl-fringe '(top periodic)
      "..XXX..."))
  (setq diff-hl-fringe-bmp-function 'my-diff-hl-fringe-bmp)

  (setq diff-hl-draw-borders nil
        diff-hl-side 'left)
  (global-diff-hl-mode 1)
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package markdown-mode
  :ensure t)

(use-package mensa-mode
  :commands mensa
  :load-path "lisp/mensa-mode")

(use-package tex
  :ensure auctex
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

(use-package vhdl-mode
  :config
  (setq vhdl-intelligent-tab nil)
  (add-hook 'vhdl-mode-hook #'vhdl-activate-customizations))

(use-package haskell-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package rust-mode
  :ensure t
  :config
  (define-key rust-mode-map (kbd "C-c C-c") #'rust-run-clippy)
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

(use-package beancount
  :load-path "lisp/beancount-mode"
  :commands (beancount-mode)
  :mode ("\\.beancount\\'" . beancount-mode)
  :config
  (add-hook 'beancount-mode-hook #'outline-minor-mode)
  (require 'flymake-bean-check)
  (add-hook 'beancount-mode-hook #'flymake-bean-check-enable))

(use-package typst-ts-mode
  :load-path "lisp/typst-ts-mode"
  :mode ("\\.typ\\'"))

(use-package rmsbolt
  :config
  ;; Use tool defaults
  (setq rmsbolt-asm-format nil))

(use-package rainbow-mode
  :ensure t)

(use-package pdf-tools
  :ensure t
  :pin "melpa"
  :preface
  (pdf-loader-install)
  :config
  (setq-default pdf-view-display-size 'fit-page)

  ;; Fix blurriness on retina
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)

  ;; Invert RET behavior
  (require 'pdf-outline)
  (define-key pdf-outline-buffer-mode-map (kbd "RET") #'pdf-outline-follow-link-and-quit)
  (define-key pdf-outline-buffer-mode-map (kbd "M-RET") #'pdf-outline-follow-link))

(use-package saveplace-pdf-view
  :ensure t
  :demand t)

(use-package sprunge
  :ensure t
  :bind (("C-c q p" . sprunge-dwim))
  :config
  (defun sprunge-dwim ()
    (interactive)
    (if (region-active-p)
        (sprunge-region)
      (sprunge-buffer))))

;; envrc-mode should be enabled late since it prepends itself to var. hooks
(use-package envrc
  :ensure t
  :demand t
  :config
  (setq envrc-none-lighter nil)
  (envrc-global-mode)

  (define-advice vterm--set-directory (:after (&rest _) update-envrc)
    (when envrc-mode (envrc--update))))


;;; --- Some final nuances ---
;; Start the server if not running
(require 'server)
(unless (or (daemonp) (server-running-p))
  (server-start))

;;; Show startup time:
(add-hook 'emacs-startup-hook
          (lambda ()
            (message (format "Emacs loaded in %s seconds." (emacs-init-time "%.2f")))))

(provide 'init)
;;; init.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
