(require 'ert)
(require 'helm-git)

(ert-deftest helm-c-git-files-returns-nil-for-empty-repo ()
  (with-temp-git-repo repo
   (should (equal (helm-c-git-files) nil))))

(ert-deftest helm-c-git-files-returns-all-files-in-current-repo ()
  (with-temp-git-repo repo
   (create-file-in-repo repo "file1")
   (create-file-in-repo repo "file2")
   (should (member "file1" (helm-c-git-files)))
   (should (member "file2" (helm-c-git-files)))))
