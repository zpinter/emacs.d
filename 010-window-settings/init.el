(add-module-path "maxframe")
                                        ;disable gui crap
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq inhibit-splash-screen t)

(defcustom mf-display-padding-width 50
  "*Any extra display padding that you want to account for while
determining the maximize number of columns to fit on a display"
  :type 'integer
  :group 'maxframe)

(when (and (eq system-type "darwin") window-system)
  (require 'maxframe)
  (setq mf-max-width 1600)
  (add-hook 'window-setup-hook 'maximize-frame t)

  (require 'carbon-font)
  (fixed-width-set-default-fontset
   "-apple-inconsolata-medium-r-normal--14-*-*-*-*-*-iso10646-1"))
