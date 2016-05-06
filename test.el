(load-file "desk.el")

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
  (desk-delete-dir "ab1")
  (desk-remove-all-files)
  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 "ab1")
  (desk-remove-all-files)
  (desk-load-0 "test/ab1")
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("b.txt" "a.txt"))))

(ert-deftest desk-clear-test ()
  "Test if all files are removed when cleared."
  (desk-remove-all-files)
  (should (equal (desk-files-in-buffer-list) nil)))

    
(ert-deftest desk-delete-dir-test ()
  "Test to delete a desktop dir."
  (desk-delete-dir "test/ab2")
  (desk-remove-all-files)
  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 "ab2")
  (should (file-exists-p "ab2"))
  (desk-delete-dir "ab2")
  (should (not (file-exists-p "test/ab2"))))


(ert-deftest desk-load-and-unload ()
  "Test to load and unload."
  (desk-delete-dir "test/ab1")
  (desk-remove-all-files)
  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 "ab1")

  (desk-remove-all-files)

  (desk-load-0 "test/ab1")
  
  (should (equal desk-current "test/ab1"))

  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("b.txt" "a.txt")))
  
  (desk-unload)
  
  (should (equal desk-current nil))
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           nil)))


