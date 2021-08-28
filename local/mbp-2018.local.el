(setq font-monospaced   (list :family "Iosevka Fixed"
                              :width 'expanded
                              :height 120)
      font-proportional (list :height 1.1))

(when window-system
  (set-frame-parameter nil 'fullscreen 'fullscreen)

  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse t)
  ;; Apply settings:
  (mouse-wheel-mode 1))
