(load-file "desk.el")


(ert-deftest desk-close-buffer-test ()
  "Test to close a buffer."
  (let ((prev-buffer-list (buffer-list)))
    (get-buffer-create "some-buffer")
    (desk-close-buffer "some-buffer")
    (should (equal
             (buffer-list)
             prev-buffer-list))))

(ert-deftest save-as-and-load-desktop-test ()
  "Test to save and then load a desktop."
  (desk-delete-dir "test/ab1")
  (desk-close-all-files)
  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 (expand-file-name "ab1"))
  (desk-unload)
  (desk-load-0 (expand-file-name "test/ab1"))
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("b.txt" "a.txt"))))

(ert-deftest desk-clear-test ()
  "Test if all files are closed when cleared."
  (desk-close-all-files)
  (should (equal (desk-files-in-buffer-list) nil)))

    
(ert-deftest desk-delete-dir-test ()
  "Test to delete a desktop dir."
  
  (desk-delete-dir "test/ab2")
  (desk-close-all-files)
  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 "ab2")
  (should (file-exists-p "ab2"))
  (desk-delete-dir "ab2")
  (should (not (file-exists-p "test/ab2"))))


(ert-deftest desk-load-and-unload-desk ()
  "Test to load and unload."
  (desk-delete-dir "test/ab1")
  (desk-close-all-files)

  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 "ab1")

  (desk-unload)

  (desk-load-0 "test/ab1")
  
  (should (equal desk-current (expand-file-name "test/ab1")))

  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("b.txt" "a.txt")))
  
  (desk-unload)
  
  (should (equal desk-current nil))
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           nil))
  (desk-close-all-files))


(ert-deftest desk-delete-desk-test ()
  (desk-delete-dir "test/ab1")
  (desk-close-all-files)
  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 "ab1")
  (desk-unload)
  (desk-load-0 "test/ab1")
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("b.txt" "a.txt")))

  (desk-delete-desk)

  (desk-unload)
  
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           nil))

  (should (not (file-exists-p "test/ab1"))))

(ert-deftest desk-swap-two-desks-and-add-file-tests ()
  "Swap between two desks."
  (desk-unload)
  (desk-delete-dir "test/ab")
  (desk-delete-dir "test/cd")


  (find-file "test/a.txt")
  (find-file "b.txt")
  (desk-save-as-0 "ab")
  (desk-unload)
  
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           nil))
  
  (find-file "test/c.txt")
  (find-file "d.txt")
  (desk-save-as-0 "cd")
  (desk-unload)

  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           nil))

  (desk-load-0 "test/ab")
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("b.txt" "a.txt")))

  (desk-load-0 "test/cd")
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("d.txt" "c.txt")))

  (desk-load-0 "test/ab")
  (find-file "test/e.txt")

  (desk-load-0 "cd")
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("d.txt" "c.txt")))

  (desk-load-0 "test/ab")
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("e.txt" "b.txt" "a.txt")))
  
  (desk-load-0 "test/cd")
  (should (equal
           (mapcar 'buffer-name (desk-files-in-buffer-list))
           '("d.txt" "c.txt"))))
