(defun init-list-disabled ()
  "List all disabled scripts in init-d."
	(directory-files init-path t "^-[0-9]+.*\.el-$"))


(defun init-list-active ()
	"List all active scripts from init-d."
	(directory-files init-path t "^[0-9]+.*\.el$"))

(provide 'init-helpers)
