(setq font-monospaced   (list :family "Input"
                              :height 120)
      font-proportional (list :height 1.1))

(when window-system
  (set-frame-size nil 160 50)

  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse t)
  ;; Apply settings:
  (mouse-wheel-mode 1))
