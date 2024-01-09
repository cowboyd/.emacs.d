;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Scrolling
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Smooth Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Compilation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Compilation from Emacs, taken from prelude
(defun @cowboyd/colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))



(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)

(add-hook 'compilation-filter-hook #'@cowboyd/colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Diminish keeps the modeline un-cluttered.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package diminish
  :ensure t
  :config
  (diminish 'which-key-mode)
  (diminish 'eldoc-mode)
  :custom
  (flymake-mode-line-lighter ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Save whenever a buffer loses focus
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package super-save
  :ensure t
  :diminish
  :config
  (super-save-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Move lines and regions up and down
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Kill line if now region specified
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package crux
  :ensure t
  :config (crux-with-region-or-line kill-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Chords for executing commands (e.g. "uu" for undo-tree)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define-global "xx" 'execute-extended-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undo as a tree of alternate realities.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-tree
  :ensure t
  :diminish
  :chords (("uu" . undo-tree-visualize))
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `((".*" . ,temporary-file-directory)))
  :config (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Browse and edit the kill ring for very smart selection of cut text.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package browse-kill-ring
  :ensure t
  :chords (("yy" . browse-kill-ring)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Project switching
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun @cowboyd/project-magit-status ()
  "Run magit-status in current project"
  (interactive)
  (magit-status (project-root (project-current))))

(use-package project
  :config
  (add-to-list 'project-switch-commands `(@cowboyd/project-magit-status "Magit" "m") t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Markdown
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Git
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package git-timemachine
  :ensure t)

(use-package git-link
  :ensure t
  :after git-timemachine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Windowing helpers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; restore the previous windowing configuration
(winner-mode)
(define-key global-map (kbd "C-c C-q") 'winner-undo)

;; This makes the split super simple and thereby consisent by always
;; splitting horizontally by half.

(setq split-height-threshold nil)
(add-hook 'window-configuration-change-hook
          #'@cowboyd/window-configuration-change-hook)

(defun @cowboyd/window-configuration-change-hook ()
  "Set the `split-width-threshold' so that the screen only splits once.
For example, if the frame is 360 columns wide, then we want the
`split-width-threshold' to be 181. That way, when you split
horizontally, the two new windows will each be 180 columns wide, and
sit just below the threshold."
  (setq split-width-threshold (+ 1 (/ (frame-width) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Eglot LSP key bindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c .") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c f") 'eglot-format)
(define-key eglot-mode-map (kbd "C-c !") 'flymake-show-buffer-diagnostics)
(define-key eglot-mode-map (kbd "C-c C-g !") 'flymake-show-project-diagnostics)
(define-key eglot-mode-map (kbd "C-c C-g C-n") 'flymake-goto-next-error)
(define-key eglot-mode-map (kbd "C-c C-g C-p") 'flymake-goto-prev-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GraphQL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package graphql-ts-mode
  :ensure t
  :mode ("\\.graphql\\'" "\\.gql\\'")
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))
