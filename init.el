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
  (global-corfu-mode)


;; Conda env support.
(use-package conda
  :ensure t
  :config
  (conda-env-autoactivate-mode t)
  (add-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
                                         (conda-env-activate-for-buffer))))
  (custom-set-variables
   ;; TODO: Search common conda env dirs for one that exists.
   ;; Replace dir with correct path if different.
   '(conda-anaconda-home (expand-file-name "~/miniforge3")))
  (conda-mode-line-setup))


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
  (server-start)
  )

(put 'dired-find-alternate-file 'disabled nil)
