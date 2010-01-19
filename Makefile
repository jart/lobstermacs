# expiremental

all:
	emacs --batch --eval "(add-to-list 'load-path (expand-file-name \".\"))" \
		      --eval "(add-to-list 'load-path (expand-file-name \"elpa-to-submit\"))" \
		      --eval "(add-to-list 'load-path (expand-file-name \"elpa-to-submit/jabber\"))" \
		      --eval "(add-to-list 'load-path (expand-file-name \"elpa-to-submit/company\"))" \
		      --eval "(add-to-list 'load-path (expand-file-name \"elpa-to-submit/nxhtml\"))" \
		      --eval "(add-to-list 'load-path (expand-file-name \"elpa-to-submit/nxhtml/util\"))" \
		      --eval "(batch-byte-compile-if-not-done)" *.el
