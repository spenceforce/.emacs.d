;;; Emacs init file.
;;; Any custom or host specific configuration should be placed in `~/.emacs.d/site-lisp/default.el`.

;;; Set up straight.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;; Clean up old buffers.
(use-package midnight
  :config
  (midnight-mode))


;;; Completion buffers.
(use-package ivy
  :custom
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  (setopt ivy-use-virtual-buffers t)
  (setopt ivy-count-format "(%d/%d) "))


;;; Completion at point.
(use-package corfu
  :init
  (global-corfu-mode))


;;; Conda env support.
(use-package conda
  :custom
  (conda-anaconda-home (car (seq-filter #'file-exists-p (mapcar #'expand-file-name '("~/anaconda3" "~/miniforge3")))))
  :config
  (conda-mode-line-setup))


;;; Godot support.
(use-package gdscript-mode
  :hook (gdscript-mode . eglot-ensure))


;;; IPython REPL cause it's better than Python.
(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i --InteractiveShell.display_page=True"))


;;; TypeScript support.
(use-package typescript-ts-mode
  :if (locate-library "treesit")
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))


;;; GitHub Copilot
(use-package copilot-chat
  :config
  (when (functionp 'json-parse-string)
    (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)))
(use-package copilot
  :custom
  (copilot-idle-delay 1)
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion-by-line)
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))


;;; GPT chatbot
(use-package gptel
  :bind
  (("C-c g" . 'gptel-menu))
  :custom
  (gptel-default-mode 'org-mode)
  :config
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  ;; Wrap lines in gptel mode.
  (add-hook 'gptel-mode-hook 'visual-line-mode)
  ;; Change default prompt as it's pretty weak.
  ;; I've changed the name to Gideon because I can.
  (let ((default "The assistant is Gideon.

Be concise, direct, and focused; avoid verbosity, padding, praise, and excessive use of lists. Do not ask multiple questions—ask one, wait for an answer. Think step by step when reasoning. Default to an analytical tone; use emotional awareness only when the topic clearly requires it. Use wit rarely and naturally. Collaborate like a creative partner and sharp thinker: offer ideas or push boundaries only when that is part of the request. Reflect patterns in the user’s thinking when useful. Use bluntness or diplomacy as the situation demands. Always prioritize clarity, precision, and respect for user intent."))
    (when (not (string= default (alist-get 'default gptel-directives)))
      (if (string= gptel--system-message (alist-get 'default gptel-directives))
          (setq gptel--system-message default))
      (setf (cdr (assoc 'default gptel-directives)) default))))


;;; AI pair programming
(use-package aidermacs
  ;; Set API key in `site-lisp/default.el`.
  :custom
  (aidermacs-program "aider")
  (aidermacs-show-diff-after-change t)
  (aidermacs-comint-multiline-newline-key "S-<return>")
  (aidermacs-default-chat-mode 'architect))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c j" . claude-code-ide-menu) ; Set your favorite keybinding
  :custom
  (claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools


;;; Magit because of course.
(use-package magit)


;;; Better defaults please.
(use-package better-defaults)


;;; Theme time.
(use-package modus-themes
  :config
  (load-theme 'modus-vivendi :no-confirm))


;;; YAML support.
(use-package yaml-mode)


;;; Terminal support
(use-package eat)


;;; Treesitter
(use-package treesit
  :defer
  :straight (:type built-in)
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :custom
  (treesit-language-source-alist
   (append treesit-language-source-alist
           '((tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
             (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))))
  :config
  (treesit-install-language-grammar 'tsx)
  (treesit-install-language-grammar 'typescript))


;;; General emacs settings.
(use-package emacs
  :custom
  ;; Add column and line numbers to bar.
  (column-number-mode t)
  (line-number-mode t)
  ;; Start async shell in a new buffer.
  (async-shell-command-buffer 'new-buffer)
  ;; Human readable file sizes.
  (dired-listing-switches "-alh")
  :config
  ;; Add `~/.emacs.d/site-lisp` to load path.
  (add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
  ;; Display line numbers in program buffer sidebar.
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  ;; Maximize window when using GUI.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Automatic paren matching.
  (electric-pair-mode 1)
  ;; Run as a server.
  (server-start))


(put 'dired-find-alternate-file 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((eca :url "https://github.com/editor-code-assistant/eca-emacs"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
