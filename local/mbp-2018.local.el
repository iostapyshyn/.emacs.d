(setenv "LIBRARY_PATH"
  (mapconcat 'identity
    '("/usr/local/opt/gcc/lib/gcc/11"
      "/usr/local/opt/libgccjit/lib/gcc/11"
      "/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin21/11.2.0") ":"))

(set-face-attribute 'default        nil :family "Iosevka Custom" :height 120)
(set-face-attribute 'fixed-pitch    nil :family "Iosevka Custom" :height 120)
(set-face-attribute 'variable-pitch nil :height 1.1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
;; Apply settings:
(mouse-wheel-mode 1)

(setq mac-command-modifier 'super
      mac-option-modifier 'meta
      mac-right-option-modifier nil
      mac-frame-tabbing nil)
