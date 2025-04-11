;; Add MELPA to repo.
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))


;; Clean up old buffers.
(use-package midnight
  :ensure t)


;; Completion buffers.
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setopt ivy-use-virtual-buffers t)
  (setopt ivy-count-format "(%d/%d) "))


;; Completion at point.
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))


;; Conda env support.
(use-package conda
  :ensure t
  :custom
  (conda-anaconda-home (car (seq-filter #'file-exists-p (mapcar #'expand-file-name '("~/anaconda3" "~/miniforge3" )))))
  :config
  (conda-mode-line-setup))


;; LLM support.
(use-package copilot-chat
  :ensure t
  :config
  (when (functionp 'json-parse-string)
    (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)))


;; Magit because of course.
(use-package magit
  :ensure t)


;; Better defaults please.
(use-package better-defaults
  :ensure t)


;; Theme time.
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi :no-confirm))


;; General emacs settings.
(use-package emacs
  :custom
  ; Add column and line numbers to bar.
  (column-number-mode t)
  (line-number-mode t)
  ; Start async shell in a new buffer.
  (async-shell-command-buffer 'new-buffer)
  :config
  ; Display line numbers in program buffer sidebar.
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  ; Maximize window when using GUI.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ; Automatic paren matching.
  (electric-pair-mode 1)
  ; Run as a server.
  (server-start))


;; Any host specific config should go in `init-extra.el`.
;; No error will be thrown if it doesn't exist.
(load (locate-user-emacs-file "init-extra.el") t)


(put 'dired-find-alternate-file 'disabled nil)
