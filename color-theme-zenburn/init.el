(setq color-theme-is-cumulative nil)
(setq color-theme-is-global nil)

(defun my-color-theme-frame-init (frame)
  (setq color-theme-is-cumulative nil)
  (set-variable 'color-theme-is-global nil)
  (select-frame frame)
  (if window-system
      (progn (load "zenburn")
             (color-theme-zenburn))
    (progn
           (color-theme-arjen))))

(add-hook 'after-make-frame-functions 'my-color-theme-frame-init)

;; Must manually call `my-color-theme-frame-init' for the initial frame.
(cond ((selected-frame)
       (my-color-theme-frame-init (selected-frame))))


