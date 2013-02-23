;;redo keybindings

(setq undo-tree-map (make-sparse-keymap))
;; remap `undo' and `undo-only' to `undo-tree-undo'
(define-key undo-tree-map [remap undo] 'undo-tree-undo)
(define-key undo-tree-map [remap undo-only] 'undo-tree-undo)
;; bind standard undo bindings (since these match redo counterparts)
(define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)
(define-key undo-tree-map "\C-_" 'undo-tree-undo)

;;don't want this:
;; ;; redo doesn't exist normally, so define our own keybindings
;; (define-key undo-tree-map (kbd "C-?") 'undo-tree-redo)

(define-key undo-tree-map (kbd "M-_") 'undo-tree-redo)
;; just in case something has defined `redo'...
(define-key undo-tree-map [remap redo] 'undo-tree-redo)
;; we use "C-x u" for the undo-tree visualizer
(define-key undo-tree-map (kbd "\C-x u") 'undo-tree-visualize)
;; bind register commands
(define-key undo-tree-map (kbd "C-x r u")
  'undo-tree-save-state-to-register)
(define-key undo-tree-map (kbd "C-x r U")
  'undo-tree-restore-state-from-register)

(require 'undo-tree)
(global-undo-tree-mode)
