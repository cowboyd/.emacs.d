;;; ai.el --- AI tools configuration -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; agent-shell — AI coding agent inside Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun @cowboyd/agent-shell-dot-subdir (subdir)
  (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
         (name (file-name-nondirectory cwd))
         (parent (file-name-nondirectory (directory-file-name (file-name-directory cwd)))))
    (expand-file-name
     subdir
     (expand-file-name name
                       (expand-file-name parent "~/agent-transcripts/")))))

(defun @cowboyd/agent-shell-buffer-name (agent project)
  "Return a compact shell buffer name like \"Claude @ project\"."
  (format "%s @ %s" agent project))

;; Emoji with VS-16 (variation selectors) cause off-by-one cursor positioning
;; in TTY Emacs across terminals (Ghostty, iTerm2, Windows Terminal): Emacs
;; computes width as 2 cells but terminals inconsistently advance by 1, leaving
;; ghost modelines in the buffer area during streaming output. ASCII in TTY.
(defconst @cowboyd/agent-shell-viewport-idle-glyph
  (if (display-graphic-p) "⚪️" "o")
  "Glyph shown when the viewport's shell is idle.")

(defconst @cowboyd/agent-shell-viewport-busy-glyph
  (if (display-graphic-p) "🟢" "*")
  "Glyph shown when the viewport's shell is working.")

(defun @cowboyd/agent-shell-viewport-buffer-id ()
  "Render buffer id as \"Claude @ project glyph\" with a busy-aware glyph."
  (let* ((name (buffer-name))
         (base (if (string-suffix-p agent-shell-viewport--suffix name)
                   (substring name 0 (- (length name)
                                        (length agent-shell-viewport--suffix)))
                 name))
         (glyph (if (agent-shell-viewport--busy-p)
                    @cowboyd/agent-shell-viewport-busy-glyph
                  @cowboyd/agent-shell-viewport-idle-glyph)))
    (format "%s %s" base glyph)))

(defun @cowboyd/agent-shell-viewport-tidy ()
  "Shorten the viewport major-mode lighter and show a busy-aware buffer id."
  (setq mode-name "AS")
  (setq-local mode-line-buffer-identification
              '(:eval (@cowboyd/agent-shell-viewport-buffer-id))))

(use-package agent-shell
  :ensure t
  :diminish (agent-shell-completion-mode agent-shell-ui-mode)
  :bind-keymap ("C-c a" . agent-shell-prefix-map)
  :bind (:map agent-shell-prefix-map
              ("a" . agent-shell)
              ("t" . agent-shell-toggle)
              ("n" . agent-shell-new-shell)
              ("f" . agent-shell-send-file)
              ("r" . agent-shell-send-region)
              ("R" . agent-shell-send-region-to)
              ("d" . agent-shell-send-dwim)
              ("c" . agent-shell-prompt-compose)
              ("?" . agent-shell-help-menu)
              ("." . agent-shell-attention-jump)
              ("i" . minuet-show-suggestion)
              ("F" . gptel-fn-complete))
  :init
  (defvar agent-shell-prefix-map (make-sparse-keymap)
    "Keymap for agent-shell commands.")
  :hook ((agent-shell-viewport-edit-mode agent-shell-viewport-view-mode)
         . @cowboyd/agent-shell-viewport-tidy)
  :custom
  (agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))
  (agent-shell-session-strategy 'prompt)
  (agent-shell-prefer-viewport-interaction t)
  (agent-shell-buffer-name-format #'@cowboyd/agent-shell-buffer-name)
  (agent-shell-dot-subdir-function #'@cowboyd/agent-shell-dot-subdir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; agent-shell-macext — macOS extensions for agent-shell
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package agent-shell-macext
  :if (eq system-type 'darwin)
  :ensure t
  :vc (:url "https://github.com/cxa/agent-shell-macext")
  :hook (agent-shell-mode . agent-shell-macext-setup)
  :custom
  (agent-shell-macext-file-copy-policy 'auto)
  (agent-shell-macext-notifications nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; knockknock — in-frame notification popups
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package knockknock
  :ensure t
  :vc (:url "https://github.com/konrad1977/knockknock"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; agent-shell-attention — notification management for agent-shell
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package agent-shell-attention
  :ensure t
  :after (agent-shell knockknock)
  :custom
  (agent-shell-attention-show-zeros t)
  :config
  (setq agent-shell-attention-notify-function
        (lambda (buffer title body)
          (let ((name (string-replace "Agent " "" (string-remove-suffix " agent" title)))
                (icon (expand-file-name "icons/chat.svg" user-emacs-directory)))
            (knockknock-notify :title name :message body
                               :icon-file icon :duration 5)
            (call-process "terminal-notifier" nil 0 nil
                          "-title" name "-message" body
                          "-sender" "org.gnu.Emacs"))))
  (setq agent-shell-attention-render-function
        (lambda (pending active)
          (let* ((busy (- (or active 0) pending))
                 (total (length (agent-shell-attention--live-agent-shell-buffers)))
                 (fmt (if (display-graphic-p)
                          " %d🤖[⚡%d 🔔%d]"
                        " %d agents [%d busy %d pending]")))
            (if (and (zerop total)
                     (not agent-shell-attention-show-zeros))
                ""
              (propertize (format fmt total busy pending)
                          'mouse-face 'mode-line-highlight
                          'help-echo (format "%d agents, %d working, %d ready" total busy pending)
                          'local-map agent-shell-attention--mode-line-map)))))
  ;; Fix: upstream uses :request but agent-shell renamed it to :acp-request,
  ;; and the permission dialog selects the shell buffer before the notification
  ;; check runs, so we check the viewport too.
  (advice-add 'agent-shell--on-request :before
    (cl-function
     (lambda (&key state acp-request &allow-other-keys)
       (when (and state acp-request)
         (let* ((buffer (map-elt state :buffer))
                (label (agent-shell-attention--request-label acp-request)))
           (when (and buffer label)
             (let* ((win (selected-window))
                    (sel (window-buffer win))
                    (vp (and (fboundp 'agent-shell-viewport--buffer)
                             (agent-shell-viewport--buffer
                              :shell-buffer buffer :existing-only t)))
                    (visible (or (eq sel buffer) (eq sel vp))))
               (unless visible
                 (let ((title (format "%s agent" (buffer-name buffer))))
                   (message "%s %s: %s"
                            agent-shell-attention-message-prefix
                            (buffer-name buffer) label)
                   (agent-shell-attention--maybe-notify buffer title label)))
               (agent-shell-attention--mark-buffer buffer label
                                                   :force (not visible))))))))
    '((name . fix-request-keyword)))
  (advice-add 'agent-shell-attention--jump-to-buffer :around
    (lambda (fn buffer)
      (let ((viewport (and agent-shell-prefer-viewport-interaction
                           (agent-shell-viewport--buffer
                            :shell-buffer buffer :existing-only t))))
        (if viewport
            (progn
              (when (bound-and-true-p winner-mode)
                (winner-save-unconditionally))
              (agent-shell-attention--clear-buffer buffer)
              (pop-to-buffer viewport agent-shell-attention-display-buffer-action)
              (if (agent-shell-attention--permission-pending-p buffer)
                  (agent-shell-jump-to-latest-permission-button-row)
                (goto-char (point-max))))
          (funcall fn buffer))))
    '((name . prefer-viewport)))
  (advice-add 'agent-shell-attention--buffer-selected-p :after-until
    (lambda (buffer)
      (and (fboundp 'agent-shell-viewport--buffer)
           (let ((viewport (agent-shell-viewport--buffer
                            :shell-buffer buffer :existing-only t)))
             (and viewport
                  (eq viewport (window-buffer (selected-window)))))))
    '((name . viewport-aware)))
  (advice-add 'agent-shell-attention--maybe-clear-current :after
    (lambda ()
      (unless (derived-mode-p 'agent-shell-mode)
        (dolist (buffer (agent-shell-attention--live-agent-shell-buffers))
          (let ((vp (agent-shell-viewport--buffer
                     :shell-buffer buffer :existing-only t)))
            (when (and (eq vp (current-buffer))
                       (not (agent-shell-attention--permission-pending-p buffer)))
              (agent-shell-attention--clear-buffer buffer))))))
    '((name . viewport-aware)))
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
