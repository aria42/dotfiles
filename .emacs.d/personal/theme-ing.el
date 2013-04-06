(defun theme-dark ()
  (interactive)
  (load-theme 'zenburn t)
  (set-face-background 'default "#222")
  (set-face-background 'region "#374186")
  (set-face-background 'fringe "#191919")
  (set-face-background 'hl-line "#191919")
  (set-face-background 'linum nil)
  (set-face-foreground 'linum "#3f5f3f")
  (setq ansi-term-color-vector ['unspecified
                                "#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf"
                                "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"]))
(theme-dark)
