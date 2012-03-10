;; Another Emacs Initialization Set

(defconst init-path (concat user-emacs-directory "init-d")
	"Directory for init-scripts.")

(defconst init-cache-file (concat user-emacs-directory ".cache/init-d.el")
	"Common cache file for all init-scripts.")

(defconst init-version "0.2")

(defun init-list-disabled ()
  "List all disabled scripts in init-d."
	(directory-files init-path t "^-[0-9]+.*\.el-$"))

(defun init-list-active ()
	"List all active scripts from init-d."
	(directory-files init-path t "^[0-9]+.*\.el$"))

(defun init-reload-all ()
	(interactive)
	(if (delete-file init-cache-file)
	(init-load)))

;;;###autoload
(defun init-d ()
	"Another Emacs Initialization Set.

Run init scripts for specific features and modes."
	(if (file-readable-p init-cache-file)
			(load init-cache-file nil "Load init-scripts...")
		(progn (make-directory (concat user-emacs-directory ".cache") t)
					 (with-temp-file init-cache-file
						 (dolist (script (init-list-active))
							 (if (byte-compile-file script)
									 (insert-file-contents script))))
					 (byte-compile-file init-cache-file t))))

(provide 'init-d)
