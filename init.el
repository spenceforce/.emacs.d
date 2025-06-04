;;; Emacs init file.
;;; Any custom or host specific configuration should be placed in `~/.emacs.d/site-lisp/default.el`.


;;; Add MELPA to repo.
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))


;;; Clean up old buffers.
(use-package midnight
  :ensure t)


;;; Completion buffers.
(use-package ivy
  :ensure t
  :custom
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  (setopt ivy-use-virtual-buffers t)
  (setopt ivy-count-format "(%d/%d) "))


;;; Completion at point.
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))


;;; Conda env support.
(use-package conda
  :ensure t
  :custom
  (conda-anaconda-home (car (seq-filter #'file-exists-p (mapcar #'expand-file-name '("~/anaconda3" "~/miniforge3" )))))
  :config
  (conda-mode-line-setup))


;;; IPython REPL cause it's better than Python.
(use-package python
  :ensure t
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i --InteractiveShell.display_page=True"))


;;; GitHub Copilot
(use-package copilot-chat
  :ensure t
  :config
  (when (functionp 'json-parse-string)
    (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message)))
(use-package copilot
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))


;;; GPT chatbot
(use-package gptel
  :ensure t)


;;; AI pair programming
(use-package aidermacs
  ;; Set API key in `site-lisp/default.el`.
  :ensure t
  :bind
  (("C-c j" . aidermacs-transient-menu))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-show-diff-after-change t)
  (aidermacs-comint-multiline-newline-key "S-<return>"))


;;; Magit because of course.
(use-package magit
  :ensure t)


;;; Better defaults please.
(use-package better-defaults
  :ensure t)


;;; Theme time.
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi :no-confirm))


;;; Org mode.
(use-package org
  :ensure t
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :custom
  (org-log-done t)
  (org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))
  (org-agenda-files '("~/org/inbox.org" ;Capture todos
                      "~/org/tasks.org" ;One off tasks
                      "~/org/projects"  ;All project org files
                      "~/org/someday.org")) ;Someday/future ideas
  (org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (org-capture-templates
   '(("t" "Todo [inbox]" entry
         (file+headline "~/org/inbox.org" "Inbox")
         "* TODO %?\n")))
  (org-agenda-custom-commands
   '(("a" "Agenda for current week or day"
      ((agenda "")
       (todo "NEXT"
             ((org-agenda-overriding-header "\nTasks\n")))
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "\nCompleted today\n")))))
     ("n" "List of all NEXT entries" todo "NEXT")))
  :config
  ;; Automatically save all org buffers after org operations.
  (advice-add 'org-refile         :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-deadline       :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-schedule       :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-store-log-note :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-todo           :after (lambda (&rest _) (org-save-all-org-buffers))))


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
