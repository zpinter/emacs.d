(require 'epa-setup)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

(setq zconfig-private-loaded nil)
(defun zconfig-private-eval (p)
  (unless zconfig-private-loaded
	 (load "~/private.gpg")
	 (setq zconfig-private-loaded t))
  (eval p))


