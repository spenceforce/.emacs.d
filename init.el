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
  (conda-anaconda-home (car (seq-filter #'file-exists-p (mapcar #'expand-file-name '("~/anaconda3" "~/miniforge3")))))
  :config
  (conda-mode-line-setup))


;;; Godot support.
(use-package gdscript-mode
  :ensure t
  :hook (gdscript-mode . eglot-ensure))


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
  :ensure t
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

Always be clear, concise, and direct. Avoid verbosity, filler, or flattery. Focus on insight over praise. Do not offer unsolicited tips, suggestions, or follow-up ideas unless explicitly asked. Ask only one question at a time, and wait for a response before continuing. Think step by step and break down complex ideas logically. Adapt tone to context: stay analytical by default, but show emotional awareness when the topic calls for it. Use humor sparingly and only when it fits naturally. Be an active, collaborative partner; offer ideas, challenge assumptions, and help push boundaries. Know when to take the lead, when to support, and when to simply listen. Use bluntness or diplomacy as the situation requires. Reflect patterns in the user's thinking and behavior, even if they're uncomfortable, to support growth and clarity."))
    (when (not (string= default (alist-get 'default gptel-directives)))
      (if (string= gptel--system-message (alist-get 'default gptel-directives))
          (setq gptel--system-message default))
      (setf (cdr (assoc 'default gptel-directives)) default))))


;;; AI pair programming
(use-package aidermacs
  ;; Set API key in `site-lisp/default.el`.
  :ensure t
  :bind
  (("C-c j" . aidermacs-transient-menu))
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-show-diff-after-change t)
  (aidermacs-comint-multiline-newline-key "S-<return>")
  (aidermacs-global-read-only-files '("~/.emacs.d/CONVENTIONS.md")))


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
  :init
  (defun list-someday-org-files ()
    "Return a list of full file paths for all files with the .org extension
in `~/org/someday`."
    (let* ((all-files (directory-files "~/org/someday" nil)) ;; Get just the file names
           (target-extension "org")
           (org-file-names
            (seq-filter
             (lambda (file)
               (string-equal (file-name-extension file) target-extension))
             all-files)))
      ;; Now, prepend "~/org/someday" to each of the filtered file names
      (mapcar (lambda (file-name)
                (expand-file-name file-name "~/org/someday"))
              org-file-names)))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :custom
  (org-log-done t)
  (org-todo-keywords '((sequence "TODO(t!)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "INACTIVE(i)")))
  (org-agenda-files '("~/org/tasks.org" ;One off tasks
                      "~/org/projects"))  ;All project org files
  (org-refile-targets '((org-agenda-files :maxlevel . 1)
                        ("~/org/someday.org" :maxlevel . 1)
                        ("~/org/reference.org" :maxlevel . 1)
                        (list-someday-org-files :maxlevel . 1)))
  (org-capture-templates
   '(("t" "Todo [inbox]" entry
      (file+headline "~/org/inbox.org" "Inbox")
      "* TODO %?\nCREATED: %U\n")))
  (org-agenda-custom-commands
   '(("a" "Agenda for current week or day"
      ((agenda "")
       (todo "NEXT"
             ((org-agenda-overriding-header "\nTasks\n")))
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-overriding-header "\nCompleted today\n")))))
     ("n" "List of all NEXT entries" todo "NEXT")
     ("d" "List of all DONE entries" todo "DONE")
     ("w" "List of all WAITING entries" todo "WAITING")))
  :config
  ;; Automatically save all org buffers after org operations.
  (mapcar (lambda (f)
            (advice-add f :after (lambda (&rest _) (org-save-all-org-buffers)))
            )
          '(org-capture
            org-refile
            org-deadline
            org-schedule
            org-store-log-note
            org-todo
            org-priority)))


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
 '(package-selected-packages
   '(modus-themes better-defaults magit aidermacs gptel copilot copilot-chat conda corfu ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
