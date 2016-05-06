(setq desktop-clear-preserve-buffers
      (cons "\\*ert\\*" desktop-clear-preserve-buffers)) 
(setq backup-inhibited t)
(load-file "test.el")

(setq failed-tests
      (ert-stats-completed-unexpected (ert-run-tests-interactively t)))
 
(switch-to-buffer "*ert*")
(set-visited-file-name "*ert*")
(save-buffer)

(if (equal failed-tests 0)
    (kill-emacs 0)
  (kill-emacs 1))
