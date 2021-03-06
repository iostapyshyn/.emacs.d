;;; early-init.el --- Emacs early init
;;; Commentary:
;;; Code:

;; Defer garbage collection until the end of the startup process
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold (* 16 1024 1024) ; 16mb
          gc-cons-percentage 0.1)))

;; Emacs consults file-name-handler-alist every time a file is read
;; so we disable it during the startup (temporarily!)
(defvar tmp--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist tmp--file-name-handler-alist)))

;; Appearance
(push '(menu-bar-lines . (if (or (eq window-system 'ns) (eq window-system 'mac)) 1 0)) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . fullscreen) default-frame-alist)

;; Disable pixel-by-pixel scrolling, since it's extremely choppy.
(setq mac-mouse-wheel-smooth-scroll nil)

;; Prefer newer versions of elisp files
(setq load-prefer-newer t)

(provide 'early-init)
;;; early-init.el ends here
