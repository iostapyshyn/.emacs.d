(set-face-attribute 'default        nil :family "Iosevka Custom" :height 120)
(set-face-attribute 'fixed-pitch    nil :family "Iosevka Custom" :height 120)
(set-face-attribute 'variable-pitch nil :height 1.1)

(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

(setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
;; Apply settings:
(mouse-wheel-mode 1)

(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-option-modifier nil
      mac-frame-tabbing nil)
