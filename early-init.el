;;; early-init.el --- Emacs early init              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Defer garbage collection until the end of the startup process
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(setq read-process-output-max (* 4 1024 1024))

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 64 1024 1024) ; 64mb
          gc-cons-percentage 0.1)))

;; Emacs consults file-name-handler-alist every time a file is read
;; so we disable it during the startup (temporarily!)
(defvar tmp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist tmp--file-name-handler-alist)))

;; Prefer newer versions of elisp files
(setq load-prefer-newer t)

(provide 'early-init)
;;; early-init.el ends here
