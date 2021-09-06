;;; early-init.el --- Emacs early init              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Defer garbage collection until the end of the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 64 1024 1024)
          gc-cons-percentage 0.1)))

(setq read-process-output-max (* 4 1024 1024))
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
