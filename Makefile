all:
	emacs --no-init-file --quick -l ert -l desktop -l run_test.el
	emacs --batch --eval '(byte-compile-file "desk.el")'
