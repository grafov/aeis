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
	(message "0.4-dev-exp"))

(defun init-get-disabled ()
	"List all disabled scripts in init-d."
	(directory-files init-path t "^[0-9]+-.*\.el-$"))

(defun init-get-active ()
	"List all active scripts from init-d."
	(directory-files init-path t "^[0-9]+-.*\.el$"))

;;;###autoload
(defun init-d ()
	"Another Emacs Initialization Set.
Run init scripts for specific features and modes."
	(if (file-readable-p init-cache-file)
			(load init-cache-file nil (message "Loading init-scripts ver. %s" (init-version)))
		(progn (make-directory (concat user-emacs-directory ".cache") t)
					 (with-temp-file init-cache-file
						 (dolist (script (init-get-active))
							 (if (byte-compile-file script)
									 (insert-file-contents script))))
					 (byte-compile-file init-cache-file t))
		(delete-other-windows))) ;; hide *Compiler log* output

(defun init-reload-all ()
	"Repeat loading of all active scripts. Results may be unpredictable."
	(interactive)
	(init-reset-cache)
	(init-d))

(defun init-reset-cache ()
	"Drop cache file for init-scripts."
	(if (file-exists-p init-cache-file)
			(delete-file init-cache-file)))

(defun init-list-active () ; TODO
	"Print all active scripts from init-d ordered by their weight."
	(interactive)
	(save-excursion
		(set-buffer (generate-new-buffer init-list-buffer-name))
		(setq buffer-read-only nil)
		(erase-buffer)
		(dolist (script (init-get-active))
			(insert script) (insert))
		(setq buffer-read-only t)
		(switch-to-buffer init-list-buffer-name)))

(defun init-make-from-buffer () ; TODO
	"Make new init-script from current buffer."
	(interactive))

(defun init-get-script-filename (weight &rest disabled)
	"Get fullpath and name of the file with init-script. For internal use."
	(let ((fname) (weight (format "%02d" (if weight weight init-normal-weight))))
		(setq fname (read-file-name "script name: " (concat init-path weight "-")
																(concat weight "-myscript")
																'confirm-after-completion
																""
																(if disabled 'init-check-disabled 'init-check-enabled)))
		(convert-standard-filename (if (string-match-p (if disabled ".el-$" ".el$") fname)
																	 fname (concat fname ".el")))))

(defun init-check-enabled (name)
	"Check that script are enabled.
Disabled scripts ends with dash (example: 50-name.el-)."
	(string= (substring name -3) ".el"))

(defun init-check-disabled (name)
	"Check that script are disabled.
Disabled scirpts ends with dash (example: 50-name.el-)."
	(string= (substring name -4) ".el-"))

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

(defun init-disable-script (weight)
	"Disable init-script. Disabled scirpts ends with dash. Example: 50-name.el-."
	(interactive "P")
	(let ((script (init-get-script-filename weight)) (cscript))
		(if (file-readable-p script)
				(rename-file script (replace-endswith script ".el" ".el-") :ok-if-already-exists))
		(setq cscript (replace-endswith script ".el" ".elc"))
		(if (file-readable-p cscript)
				(rename-file cscript (replace-endswith script ".elc" ".elc-") :ok-if-already-exists))
	(init-reset-cache)))

(defun init-enable-script (weight)
	"Enable init-script."
	(interactive "P")
	(let ((script (init-get-script-filename weight t)) (cscript))
		(if (file-readable-p script)
				(rename-file script (replace-endswith script ".el-" ".el") :ok-if-already-exists))
		(setq cscript (replace-endswith script ".el-" ".elc-"))
		(if (file-readable-p cscript)
				(rename-file cscript (replace-endswith script ".elc-" ".elc") :ok-if-already-exists))
	(init-reset-cache)))

;; helpers
;;

(defun replace-endswith (from-string endswith to-string)
	"If FROM-STRING ends with ENDSWITH then replace ENDSWITH with TO-STRING."
	(let ((endswith-regexp (concat endswith "$")))
	(if (string-match-p endswith-regexp from-string)
			(replace-regexp-in-string endswith-regexp to-string from-string)
		from-string)))

(provide 'init-d)