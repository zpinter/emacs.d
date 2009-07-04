(require 'twit)

;; (load "~/my-elisp/twit.el")
;;(add-module-path "twittering")
;;(require 'twittering-mode)
;;(setq twittering-icon-mode t)
(defface twit-title-face
    '((((background light))
       (:background "PowderBlue"
        :underline "DeepSkyBlue"
        :box (:line-width 2 :color "PowderBlue" :style 0)))
      (((background dark))
       (:background "PowderBlue"
        :underline "DeepSkyBlue"
        :box (:line-width 2 :color "PowderBlue" :style 0)))
      (t (:underline "black")))
  "Title Area of the recent tweets buffer."
  :group 'twit)