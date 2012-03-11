;; Another Emacs Initialization Set

(defconst init-path (concat user-emacs-directory "init-d")
	"Directory for init-scripts.")

(defconst init-cache-file (concat user-emacs-directory ".cache/init-d.el")
	"Common cache file for all init-scripts.")

(defun init-version ()
	"Inverse compatible version data."
	(interactive)
	(message "0.2"))

(defun init-list-disabled ()
  "List all disabled scripts in init-d."
	(directory-files init-path t "^-[0-9]+.*\.el-$"))

(defun init-list-active ()
	"List all active scripts from init-d."
	(directory-files init-path t "^[0-9]+.*\.el$"))

(defun init-reload-all ()
	"Repeat loading of all active scripts. Results may be unpredictable."
	(interactive)
	(delete-file init-cache-file)
	(init-d))

;;;###autoload
(defun init-d ()
	"Another Emacs Initialization Set.

Run init scripts for specific features and modes."
	(if (file-readable-p init-cache-file)
			(load init-cache-file nil (message "Load init-scripts... %s" (init-version)))
		(progn (make-directory (concat user-emacs-directory ".cache") t)
					 (with-temp-file init-cache-file
						 (dolist (script (init-list-active))
							 (if (byte-compile-file script)
									 (insert-file-contents script))))
					 (byte-compile-file init-cache-file t))
		(delete-other-windows))) ;; hide *Compiler log* output

(defun init-print-active () ; TODO
	"Print all active scripts from init-d ordered by their weight."
	(interactive)
	(dolist (script (init-list-active))
		(message "%s\n" script))

(defun init-make-from-buffer () ; TODO
	"Make new init-script from current buffer."
	(interactive)) 

(defun init-new (buf) ; TODO
	"Create new buffer with simple template for new init-script."
	(interactive "Mscript name: ")
	(set-buffer (generate-new-buffer buf)))


(provide 'init-d)
