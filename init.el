;; Full screen mode please.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Add MELPA to repo.
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; Better defaults please.
(use-package better-defaults
  :ensure t)

;; Theme time.
(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-vivendi :no-confirm))

;; Magit because of course.
(use-package magit
  :ensure t)
