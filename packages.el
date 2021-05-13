;;; packages.el -- selection of packages to use
;;; Code:
;;; Commentary:
(require 'use-package)

(use-package zenburn-theme :config (load-theme 'zenburn t))
(use-package frontside-javascript :config (frontside-javascript))

(use-package exec-path-from-shell :config (exec-path-from-shell-initialize))

(use-package undo-tree
  :config (global-undo-tree-mode)
  :chords (("uu" . undo-tree-visualize)))

;; frontside-completion.el
(use-package company
  :custom
  (company-idle-delay 0.5)
  (company-tooltip-limit 10)
  (company-minimum-prefix-length)
  (company-tooltip-flip-when-above t)
  :config (global-company-mode))
(use-package flx)
(use-package smex)
(use-package swiper)
(use-package ivy
  :custom
  (ivy-re-builders-alist '((swiper . ivy--regex-plus)
                           (counsel-ag . ivy--regex-plus)
                           (t . ivy--regex-fuzzy)))
  :config (ivy-mode 1))

(use-package counsel
  :chords (("xx" . counsel-M-x))
  :bind (("M-X" . counsel-M-x)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h b" . counsel-descbinds)
         ("C-x C-f" . counsel-find-file)
         :map counsel-find-file-map
         ("C-j" . ivy-done)
         ("RET" . ivy-alt-done)))


(use-package projectile
  :custom
  (projectile-switch-project-action (quote projectile-dired))
  (projectile-completion-system 'ivy)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-global-mode t))

(use-package counsel-projectile :config (counsel-projectile-mode))

; frontside-git
(use-package magit
  :bind ("C-x g" . magit-status)
  :custom (git-link-open-in-browser t))

;; (use-package diff-hl
;;   :hook (magit-post-refresh . diff-hl-magit-post-refresh)
;;   :config (global-diff-hl-mode t) (diff-hl-flydiff-mode)
;;   :custom (diff-hl-draw-borders nil))

(use-package git-link
  :custom (git-link-open-in-browser t)
  :bind (("C-x v b" . git-link)))

(use-package github-clone
  :bind (("C-x v c" . github-clone)))

(use-package git-timemachine
  :bind (("C-x v t" . git-timemachine)))



;; frontside-web.el
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-quoting nil)
  (css-indent-offset 2))

(use-package emmet-mode
  :hook sgml-mode
  :hook web-mode)

;; frontside-ruby.el

(use-package ruby-tools
  :hook (ruby-mode . ruby-tools-mode)
  :hook (ruby-mode . subword-mode)
  :config
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.cap\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Berksfile\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Appraisals\\'" . ruby-mode))

  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc"))

(use-package rspec-mode
  :config (rspec-install-snippets)
  :custom (rspec-use-rake-when-possible nil))

;; frontside-yaml.el
(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; frontside-markdown.el
(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  :hook (markdown-mode . turn-on-auto-fill))

;; frontside-lsp.el
(use-package lsp-mode
  :custom
  (lsp-auto-guess-root t)
  (lsp-file-watch-threshold 5000))

;; frontside-rust.el
(use-package lsp-mode)
(use-package rust-mode
  :hook (rust-mode . lsp))

;; frontside-dart.el
(use-package lsp-mode)
(use-package dart-mode
  :hook (dart-mode . lsp))

;; frontside-modeline.el
(use-package diminish
  :config
  (diminish 'ivy-mode)
  (diminish 'company-mode)
  (diminish 'smartparens-mode)
  (diminish 'volatile-highlights-mode)
  (diminish 'undo-tree-mode)
  (diminish 'flycheck-mode)
  (diminish 'drag-stuff-mode)
  (diminish 'which-key-mode)
  (diminish 'yas-minor-mode)
  (diminish 'projectile-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'editorconfig-mode)
  (diminish 'eldoc-mode))



;;; TODO

;; Counsel provides some nice enhancements to core emacs functions.
;; See https://github.com/abo-abo/swiper#counsel for details

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h b") 'counsel-descbinds)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; Flip bindings for ivy-done and ivy-alt-done in counsel. This allows you to
;; hit RET to complete a directory instead of opening dired.
(define-key counsel-find-file-map (kbd "C-j") 'ivy-done)
(define-key counsel-find-file-map (kbd "RET") 'ivy-alt-done)

(provide 'packages)
;;; packages.el ends here
