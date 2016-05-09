(defun desk-init () 
  (setq desk-home-dir "~/.deskel/")
  (desk-make-dir desk-home-dir)
  (if (not (boundp 'desk-current))
      (setq desk-current nil))
  (add-to-list 'after-save-hook 'desk-save)
  (add-to-list 'window-configuration-change-hook 'desk-save)
  (desk-map-keys)
  (advice-add 'helm-ff-update-when-only-one-matched
            :after
            #'desk-load-auto-select))


(defun desk-map-keys ()
  (define-prefix-command 'desk-map)
  (global-set-key (kbd "C-t") 'desk-map)
  (define-key desk-map (kbd "n") 'desk-new)
  (define-key desk-map (kbd "s") 'desk-save)
  (define-key desk-map (kbd "a") 'desk-save-as)
  (define-key desk-map (kbd "l") 'desk-load)
  (define-key desk-map (kbd "u") 'desk-unload)
  (define-key desk-map (kbd "r") 'desk-remove-file)
  (define-key desk-map (kbd "c") 'desk-close-all-files)
  (define-key desk-map (kbd "v") 'desk-show-desk)
  (define-key desk-map (kbd "d") 'desk-delete-desk))


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


(defun desk-delete-dir (p)
  "Delete directory."
  (let ((path (expand-file-name p)))
    (if (file-exists-p path)
        (delete-directory path t))))

(defun desk-make-dir (p)
  "Create directory."
  (let ((path (expand-file-name p)))
    (if (not (file-exists-p p))
        (make-directory p))))


(defun desk-close-buffer (buffer-name)
  "Close a buffer without prompting."
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
            ((symbol-function 'yes-or-no-p) (lambda (a) t))
            ((symbol-function 'desktop-save-buffer-p)
             ;; Try to save as much as possible
             (lambda (filename bufname mode &rest _dummy) t)))
    (funcall body)))

(defun desk-close-all-files ()
  "Clear all dekstops."
  (interactive)
  (desk-env (lambda ()
              (desktop-clear))))


(defun desk-save-as-0 (p)
  "Save a desktop in the deskel enviroment."
  (let ((path (expand-file-name p)))
    (desk-env (lambda () 
                (desk-make-dir path)
                (desktop-save path)
                (setq desk-current path)))))

(defun desk-save-as-1 (path)
  "Interactive wrapper for `desk-save-as-0'."
  (interactive "f")
  (desk-save-as-0 path))

(defun desk-save-as ()
  "Save desktop in desk-home-dir."
  (interactive)
  (let ((default-directory desk-home-dir))
    (call-interactively 'desk-save-as-1)))

(defun desk-load-0 (p)
  "Load a desktop in the deskel enviroment."
  (let ((path (expand-file-name p)))
    (desk-env (lambda () 
                (desk-unload)
                (desktop-read path)))
    (setq desk-current path)))

(defun desk-load-1 (path)
  "Interactive wrapper for `desk-load'."
  (interactive "f")
  (desk-load-0 path))


(defun desk-load-auto-select ()
  (if (boundp 'desk-in-desk-load)
      (if (and
           (not (equal helm-pattern (expand-file-name desk-home-dir)))
           (file-exists-p (helm-get-selection)))
          (progn
            (let ((helm--reading-passwd-or-string t)) 
              (helm-confirm-and-exit-minibuffer))))))


(defun desk-load ()
  "Load desktop from desk-home-dir."
  (interactive)
  (setq desk-current nil)
  (let ((default-directory desk-home-dir)
        (desk-in-desk-load t))
    (call-interactively 'desk-load-1))
  (with-temp-message (format "Desk loaded. Current desk is %s" desk-current)))


(defun desk-unload ()
  "Unload desktop and restart clean."
  (interactive)
  (setq desk-current nil)
  (desk-close-all-files)
  (with-temp-message (format "Desk unloaded. Current desk is %s" desk-current)))


(defun desk-delete-desk ()
  "Delete current desk."
  (interactive)
  (if (not (equal desk-current nil))
      (let ((desk desk-current))
        (desk-unload)
        (desk-delete-dir desk))))



(defun desk-remove-file ()
  "Remove a file from current desk."
  (interactive)
  (desk-close-buffer (current-buffer))
  (desk-save))

(defun desk-save ()
  "Save current desk."
  (interactive)
  (if (not (equal desk-current nil))
      (progn
        (desk-save-as-0 desk-current)
        (with-temp-message (format "Saved desk %s" desk-current)))))


(defun desk-new ()
  "Create a new empty desk."
  (interactive)
  (desk-close-all-files)
  (call-interactively 'desk-save-as))


(defun desk-show-desk ()
  "Show current desk."
  (interactive)
  (message "Current desktop: %s" desk-current))

(desk-init)

