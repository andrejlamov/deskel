(defun desk-init () 
  (setq desk-home-dir "~/.deskel")
  (desk-make-dir desk-home-dir))

(defun desk-delete-dir (path)
  "Delete directory."
  (if (file-exists-p path)
      (delete-directory path t)))

(defun desk-make-dir (path)
  "Create directory."
  (if (not (file-exists-p path))
      (make-directory path)))


(defun desk-remove-buffer (buffer-name)
  "Remove a buffer without prompting."
  (switch-to-buffer buffer-name)
  (set-buffer-modified-p nil) ; skip prompt
  (kill-buffer buffer-name))


(defun desk-files-in-buffer-list ()
  "Return files in buffer-list."
  (interactive)
  (cl-remove-if
   (lambda (b)
     (let ((n (buffer-name b)))
       (or (equal (substring n 0 1) "*")
           (equal (substring n 0 2) " *")
           (equal (substring n 0 1) "#"))))
   (buffer-list)))


(defun desk-env (body)
  "Execute in the deskel enviroment. Should be a macro?"
  (cl-letf (((symbol-function 'y-or-n-p)    (lambda (a) t))
            ((symbol-function 'yes-or-no-p) (lambda (a) t)))
    (funcall body)))

(defun desk-remove-all-files ()
  "Clear all dekstops."
  (desk-env (lambda ()
              (desktop-clear))))


(defun desk-save-as-0 (path)
  "Save a desktop in the deskel enviroment."
  (desk-env (lambda () 
              (desk-make-dir path)
              (desktop-save path))))

(defun desk-save-as-1 (path)
  "Interactive wrapper for `desk-save-as-0'."
  (interactive "f")
  (desk-save-as-0 path))

(defun desk-save-as ()
  "Save desktop in desk-home-dir."
  (interactive)
  (let ((default-directory desk-home-dir))
    (call-interactively 'desk-save-as-1)))

(defun desk-load-0 (path)
  "Load a desktop in the deskel enviroment."
  (desk-env (lambda () 
    (desktop-read path))))


(defun desk-load-1 (path)
  "Interactive wrapper for `desk-load'."
  (interactive "f")
  (desk-load-0 path))

(defun desk-load ()
  "Load desktop from desk-home-dir."
  (interactive)
  (let ((default-directory desk-home-dir))
    (call-interactively 'desk-load-1)))

(desk-init)

