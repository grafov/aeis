;; Another Emacs Initialization Set

(defconst init-path (concat user-emacs-directory "init-d/")
	"Directory for init-scripts.")

(defconst init-normal-weight 50
	"Default weight (normal priority) for init-scripts.")

(defconst init-cache-file (concat user-emacs-directory ".cache/init-d.el")
	"Common cache file for all init-scripts.")

(defconst init-list-buffer-name "* Init list *"
	"Name of the buffer with init-scripts list.")

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

;;;###autoload
(defun init-d ()
	"Another Emacs Initialization Set.
Run init scripts for specific features and modes."
	(if (file-readable-p init-cache-file)
			(load init-cache-file nil (message "Loading init-scripts ver. %s" (init-version)))
		(progn (make-directory (concat user-emacs-directory ".cache") t)
					 (with-temp-file init-cache-file
						 (dolist (script (init-list-active))
							 (if (byte-compile-file script)
									 (insert-file-contents script))))
					 (byte-compile-file init-cache-file t))
		(delete-other-windows))) ;; hide *Compiler log* output

(defun init-reload-all ()
	"Repeat loading of all active scripts. Results may be unpredictable."
	(interactive)
	(delete-file init-cache-file)
	(init-d))


(defun init-print-active () ; TODO
	"Print all active scripts from init-d ordered by their weight."
	(interactive)
	(save-excursion
		(set-buffer (generate-new-buffer init-list-buffer-name))
		(setq buffer-read-only nil)
		(erase-buffer)
		(dolist (script (init-list-active))
			(insert script) (insert))
		(setq buffer-read-only t)
		(switch-to-buffer init-list-buffer-name)))

(defun init-make-from-buffer () ; TODO
	"Make new init-script from current buffer."
	(interactive))

(defun init-get-script-filename (weight) 
	"Get fullpath and name of the file with init-script. For internal use."
	(let ((weight (format "%02d" (if weight weight init-normal-weight))))
		(convert-standard-filename (read-file-name "script name: " (concat init-path weight "-")
																							 (concat weight "-myscript.el") 
																							 'confirm-after-completion) 
																)))

(defun init-edit-script (weight)
	"Make new or edit existing init-script. Prompts for script name and use prefix number for setting weight."
	(interactive "P")
	(let ((script) (buf))
		(setq buf
					(set-buffer
					 (create-file-buffer (setq script (init-get-script-filename weight)))))
		(if (file-exists-p script)
				(find-file script)
			(progn 
				(insert (format ";; %s —" (substring (buffer-name) 3 -3)))
				(insert "; mode: lisp; coding: utf-8")
				(insert)
				(insert)
				(switch-to-buffer buf)))))

(provide 'init-d)