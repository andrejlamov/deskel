(load-file "desk.el")

(defun desk-test-mode ()
  "A tdd mode for developing deskel."
  (interactive)
  (let ((current-buffer (current-buffer)))
    (tdd-mode)
    (compile "make")
    (delete-window)
    (find-file "*ert*")
    (auto-revert-mode)
    (split-window)
    (switch-to-buffer current-buffer)))


(ert-deftest desk-remove-buffer-test ()
  "Test to remove a buffer."
  (let ((prev-buffer-list (buffer-list)))
    (get-buffer-create "some-buffer")
    (desk-remove-buffer "some-buffer")
    (should (equal
             (buffer-list)
             prev-buffer-list))))

(ert-deftest save-as-and-load-desktop-test ()
  "Test to save and then load a desktop."
  (desk-delete-dir "test/a")
  (desk-remove-all-files)
  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 "ab")
  (desk-remove-all-files)
  (desk-load-0 "test/ab")
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("b.txt" "a.txt"))))

(ert-deftest desk-clear-test ()
  "Test if all files are removed when cleared."
  (desk-remove-all-files)
  (should (equal (desk-files-in-buffer-list) nil)))



