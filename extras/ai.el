;;; ai.el --- AI tools configuration -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; agent-shell — AI coding agent inside Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package agent-shell
  :ensure t
  :bind-keymap ("C-c a" . agent-shell-prefix-map)
  :bind (:map agent-shell-prefix-map
              ("a" . agent-shell)
              ("t" . agent-shell-toggle)
              ("n" . agent-shell-new-shell)
              ("f" . agent-shell-send-file)
              ("r" . agent-shell-send-region)
              ("d" . agent-shell-send-dwim)
              ("c" . agent-shell-prompt-compose)
              ("?" . agent-shell-help-menu)
              ("i" . minuet-show-suggestion)
              ("F" . gptel-fn-complete))
  :init
  (defvar agent-shell-prefix-map (make-sparse-keymap)
    "Keymap for agent-shell commands.")
  :custom
  (agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; agent-shell-attention — mode-line indicator for pending agent buffers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package agent-shell-attention
  :vc (:url "https://github.com/ultronozm/agent-shell-attention.el" :branch "main")
  :after agent-shell
  :bind (:map agent-shell-prefix-map
              ("." . agent-shell-attention-jump))
  :custom
  (agent-shell-attention-lighter " 🤖:%d")
  (agent-shell-attention-show-zeros t)
  :config
  (agent-shell-attention-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; gptel — LLM client for one-off prompts and rewrites
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gptel
  :ensure t
  :custom
  (gptel-model 'claude-opus-4-6)
  (gptel-expert-commands t)
  (gptel-rewrite-default-action 'accept)
  :config
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key (let ((key (auth-source-pick-first-password
                           :host "api.anthropic.com"
                           :user "apikey")))
                 (lambda () key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; gptel-fn-complete — complete function at point using an LLM
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gptel-fn-complete
  :ensure t
  :after gptel)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; minuet — inline code completion as-you-type
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun @cowboyd/minuet-block-suggestions ()
  "Return non-nil to block suggestions in unhelpful contexts.
Blocks when buffer is read-only, cursor is at beginning of line,
or cursor is not at end of line (ignoring trailing whitespace)."
  (not (and (not buffer-read-only)
            (not (bolp))
            (looking-at-p "\\s-*$"))))

(use-package minuet
  :ensure t
  :diminish minuet-auto-suggestion-mode
  :hook (prog-mode . minuet-auto-suggestion-mode)
  :custom
  (minuet-provider 'claude)
  (minuet-n-completions 1)
  (minuet-add-single-line-entry nil)
  (minuet-auto-suggestion-debounce-delay 0.3)
  :bind (:map minuet-active-mode-map
              ("M-A" . minuet-accept-suggestion)
              ("M-a" . minuet-accept-suggestion-line)
              ("M-n" . minuet-next-suggestion)
              ("M-p" . minuet-previous-suggestion)
              ("M-e" . minuet-dismiss-suggestion))
  :config
  (plist-put minuet-claude-options :model "claude-opus-4-6")
  (plist-put minuet-claude-options :api-key
             (let ((key (auth-source-pick-first-password
                         :host "api.anthropic.com"
                         :user "apikey")))
               (lambda () key)))
  (add-hook 'minuet-auto-suggestion-block-predicates
            #'@cowboyd/minuet-block-suggestions -100))
