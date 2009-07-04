(autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
(autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
(autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
(autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
(autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
(autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
(autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
(autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t)

;; (Global-set-key [(shift space)]         'bc-set) ;; Shift-SPACE for set bookmark
;; (global-set-key [(meta j)]              'bc-previous) ;; M-j for jump to previous
;; (global-set-key [(shift meta j)]        'bc-next) ;; Shift-M-j for jump to next
;; (global-set-key [(meta up)]             'bc-local-previous) ;; M-up-arrow for local previous
;; (global-set-key [(meta down)]           'bc-local-next) ;; M-down-arrow for local next
;; (global-set-key [(control c)(j)]        'bc-goto-current) ;; C-c j for jump to current bookmark
;; (global-set-key [(control x)(meta j)]   'bc-list) ;; C-x M-j for the bookmark menu list