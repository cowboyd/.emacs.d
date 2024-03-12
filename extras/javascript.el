;; Specific versions of node packages installed on a per-project
;; basis are the norm in JS development. So, for example, if you're
;; using `eslint' to stylecheck your code, this will make project
;; buffers find `node_modules/.bin/eslint' before any other
;; executable in their `exec-path'
(use-package add-node-modules-path
  :ensure t
  :hook (prog-mode . add-node-modules-path)
  :custom
  (add-node-modules-path-command '("pnpm bin")))

(add-to-list 'auto-mode-alist '("\\.ts\\'"    . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'"    . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"    . js-ts-mode))

(defun @cowboyd/deno-lsp-or-typescript-language-server (&optional interactive project)	 
  (let ((marker (and (eq (car project) 'eglot--project)
                     (plist-get (cddr project) :marker))))
	   (cond ((string= marker "package.json")
		  '("typescript-language-server" "--stdio"))
		 ((string= marker "deno.json")
		  '("deno" "lsp" :initializationOptions '(:enable t :lint t)))
		 (interactive
		  (message "Can't guess server invocation for '%s'" (project-root project))
		  nil))))

(defun @cowboyd/js-project-find-function (dir)
  (when eglot-lsp-context
    (let* ((marker nil)
           (root (catch 'done
		   (locate-dominating-file
                    default-directory
                    (lambda (d)
                      (dolist (f '("package.json" "deno.json"))
			(when (file-exists-p (expand-file-name f d))
			  (setq marker f)
			  (throw 'done d))))))))

      (when root
	;; Use experimental eglot--project project type which has some
	;; space for user-defined props, `:marker' in this case
	`(eglot--project ,root :marker ,marker)))))

(add-hook 'project-find-functions '@cowboyd/js-project-find-function)
(add-to-list 'eglot-server-programs
             `((js-mode (typescript-ts-base-mode :language-id "typescript"))
	       . @cowboyd/deno-lsp-or-typescript-language-server))
