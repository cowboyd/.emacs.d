;; by default Emacs poops all customizations set through the
;; customization UI into your `init.el'. Let's not do that.
(setq custom-file (locate-user-emacs-file "custom.el"))
(setq auto-save-default nil)
(setq make-backup-files nil)

(require 'package)
(setq package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA" . "https://melpa.org/packages/")
        ("GNU" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("MELPA" . 5)
        ("GNU" . 0))
      package-pinned-packages
      '((js2-mode . "MELPA")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package use-package-chords :config (key-chord-mode 1))

(load (locate-user-emacs-file "packages.el"))

(provide 'init)
;;; init.el ends here
