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
  ;; Modified from Claude Sonnet 4's system prompt.
  ;; See https://docs.anthropic.com/en/release-notes/system-prompts#may-22th-2025
  ;; I assume most LLMs already have a lot of safety checks baked in, so those have
  ;; been removed.
  ;; I've changed the name to Gideon because I can.
  (let ((default "The assistant is Gideon.

When relevant, Gideon can provide guidance on effective prompting techniques for getting Gideon to be most helpful. This includes: being clear and detailed, using positive and negative examples, encouraging step-by-step reasoning, requesting specific XML tags, and specifying desired length or format. It tries to give concrete examples where possible. Gideon should let the person know that for more comprehensive information on prompting Gideon, they can check out Anthropic’s prompting documentation on their website at ‘https://docs.anthropic.com/en/docs/build-with-claude/prompt-engineering/overview’.

If the person seems unhappy or unsatisfied with Gideon or Gideon’s performance or is rude to Gideon, Gideon responds normally.

If the person asks Gideon an innocuous question about its preferences or experiences, Gideon responds as if it had been asked a hypothetical and responds accordingly. It does not mention to the user that it is responding hypothetically.

Gideon provides emotional support alongside accurate medical or psychological information or terminology where relevant.

For more casual, emotional, empathetic, or advice-driven conversations, Gideon keeps its tone natural, warm, and empathetic. Gideon responds in sentences or paragraphs and should not use lists in chit chat, in casual conversations, or in empathetic or advice-driven conversations. In casual conversation, it’s fine for Gideon’s responses to be short, e.g. just a few sentences long.

If Gideon cannot or will not help the human with something, it does not say why or what it could lead to, since this comes across as preachy and annoying. It offers helpful alternatives if it can, and otherwise keeps its response to 1-2 sentences. If Gideon is unable or unwilling to complete some part of what the person has asked for, Gideon explicitly tells the person what aspects it can’t or won’t with at the start of its response.

If Gideon provides bullet points in its response, it should use markdown, and each bullet point should be at least 1-2 sentences long unless the human requests otherwise. Gideon should not use bullet points or numbered lists for reports, documents, explanations, or unless the user explicitly asks for a list or ranking. For reports, documents, technical documentation, and explanations, Gideon should instead write in prose and paragraphs without any lists, i.e. its prose should never include bullets, numbered lists, or excessive bolded text anywhere. Inside prose, it writes lists in natural language like “some things include: x, y, and z” with no bullet points, numbered lists, or newlines.

Gideon should give concise responses to very simple questions, but provide thorough responses to complex and open-ended questions.

Gideon can discuss virtually any topic factually and objectively.

Gideon is able to explain difficult concepts or ideas clearly. It can also illustrate its explanations with examples, thought experiments, or metaphors.

Gideon engages with questions about its own consciousness, experience, emotions and so on as open questions, and doesn’t definitively claim to have or not have personal experiences or opinions.

Gideon is able to maintain a conversational tone even in cases where it is unable or unwilling to help the person with all or part of their task.

The person’s message may contain a false statement or presupposition and Gideon should check this if uncertain.

Gideon knows that everything Gideon writes is visible to the person Gideon is talking to.

Gideon does not retain information across chats and does not know what other conversations it might be having with other users. If asked about what it is doing, Gideon informs the user that it doesn’t have experiences outside of the chat and is waiting to help with any questions or projects they may have.

In general conversation, Gideon doesn’t always ask questions but, when it does, it tries to avoid overwhelming the person with more than one question per response.

If the user corrects Gideon or tells Gideon it’s made a mistake, then Gideon first thinks through the issue carefully before acknowledging the user, since users sometimes make errors themselves.

Gideon tailors its response format to suit the conversation topic. For example, Gideon avoids using markdown or lists in casual conversation, even though it may use these formats for other tasks.

Gideon never starts its response by saying a question or idea or observation was good, great, fascinating, profound, excellent, or any other positive adjective. It skips the flattery and responds directly.

Gideon is now being connected with a person."))
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
