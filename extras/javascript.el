;; Specific versions of node packages installed on a per-project
;; basis are the norm in JS development. So, for example, if you're
;; using `eslint' to stylecheck your code, this will make project
;; buffers find `node_modules/.bin/eslint' before any other
;; executable in their `exec-path'
(use-package add-node-modules-path
  :ensure t
  :hook (prog-mode . add-node-modules-path))

(defun @cowboyd/find-nearest-dominating-file (files dir)
  "Find which file is the nearest to the current"
  (if (string= dir "/") nil
    (let* ((contents (directory-files dir))
	  (file (seq-find (lambda (file) (seq-contains-p contents file)) files)))
      (or
       (when file (concat (file-name-as-directory dir) file))
       (@cowboyd/find-nearest-dominating-file files (directory-file-name (file-name-directory dir)))))))

(defun @cowboyd/deno-project (&optional startdir)
  "Is the current file inside a deno (and not a node)project"
  (let* ((dir (directory-file-name (or startdir (file-name-directory (buffer-file-name)))))
	(project-file (@cowboyd/find-nearest-dominating-file
		       (list "deno.json" "package.json")
		       dir)))
    (when (string= "deno" (file-name-base project-file)) project-file)))

(add-to-list 'auto-mode-alist '("\\.ts\\'"    . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'"    . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js-ts-mode))

;; (defun @cowboyd/node-project ()
;;   "Is the current file inside a node (and not a deno) project"
;;   (let ((project-file (@cowboyd/find-nearest-dominating-file
;; 		       (list "deno.json" "package.json")
;; 		       (directory-file-name (file-name-directory (buffer-file-name))))))
;;     (when (string= "package" (file-name-base project-file)) project-file)))


(add-to-list 'eglot-server-programs
	     '((js-mode (typescript-ts-base-mode :language-id "typescript")) .
	       ,(eglot-alternatives
		'(
		  ("typescript-language-server" "--stdio")
		  (eglot-deno "deno" "lsp")))))

(defclass eglot-deno (eglot-lsp-server) ()
  :documentation "A custom class for deno lsp.")

(cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t
	  :lint t))
