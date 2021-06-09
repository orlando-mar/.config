;;; package sources & initialization

(require 'package)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))

(let ((use-package 'use-package))
  (unless (package-installed-p use-package)
    (package-install use-package)))

;;; general emacs customizations

(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen t)
(electric-indent-mode -1)
(global-display-line-numbers-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-r") 'rename-buffer)

(setq inhibit-splash-screen t)
(setq custom-file null-device)
(setq backup-directory-alist '(("." . "~/.backups/")))
(setq auto-save-file-name-transforms `((".*" "~/.backups/" t)))
(setq-default tab-width 4)

;;; magit

(use-package magit
  :ensure t
  :config
  (setq magit-post-display-buffer-hook
      #'(lambda ()
          (when (derived-mode-p 'magit-status-mode)
            (delete-other-windows)))))

;;; counsel, ivy, and swiper

(use-package counsel
  :ensure t
  :init (counsel-mode 1))
(use-package ivy
  :ensure t
  :init (ivy-mode 1))
(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper-backward))

;;; multi-term

(use-package multi-term
  :ensure t)
(global-set-key (kbd "C-c C-t") 'multi-term)

; display-line-numbers-mode doesn't play nicely with term-mode
 (defun inhibit-display-line-numbers-mode ()
   "Disable display-line-numbers-mode"
   (add-hook 'after-change-major-mode-hook
	     (lambda () (display-line-numbers-mode 0))
	     :append :local))
(add-hook 'term-mode-hook 'inhibit-display-line-numbers-mode)

;;; org

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"
		org-journal-file-type 'weekly))

;;; plantuml

(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path (expand-file-name "~/.config/emacs/plantuml/plantuml.jar"))
  (setq org-plantuml-jar-path (expand-file-name "~/.config/emacs/plantuml/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;;; projectile

(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :bind 
  (:map global-map
	("C-x f" . find-file)
	("C-x C-f" . projectile-find-file))
  (:map projectile-mode-map
	("C-x p" . projectile-command-map)
	("C-x p a" . projectile-add-known-project)
	("C-x p r" . projectile-remove-known-project)))

;;; perspective

(use-package perspective
  :ensure t
  :bind
  ("C-x C-b" . persp-counsel-switch-buffer)   ; or use a nicer switcher, see below
  :init
  (persp-mode))

;;; helm

(use-package helm
  :ensure t
  :init 
  (helm-mode 1))

;;; treemacs

(use-package all-the-icons
  :ensure t)
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-width 30)
  :bind
  (:map global-map
        ("C-x t t" . treemacs)
		("C-x t a" . treemacs-add-project-to-workspace)
        ("C-x t d" . treemacs-remove-project-from-workspace)
		("C-x t h" . treemacs-narrow-to-current-file)
		("C-x t q" . treemacs-add-and-display-current-project)))
(add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t
  :bind
  (("C-x t p" . treemacs-projectile)))

;(use-package treemacs-perspective
;  :ensure t
;  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;; company

(use-package company
  :ensure t
  :init
  (global-company-mode 1))
(use-package company-fuzzy
  :ensure t
  :init
  (global-company-fuzzy-mode 1)
  :after (company))

;;; flycheck

(use-package flycheck 
  :ensure t 
  :init (global-flycheck-mode)
  :bind
  (:map flycheck-mode-map
	("C-c e l" . flycheck-list-errors)
	("C-c e n" . flycheck-next-error)
	("C-c e p" . flycheck-previous-error)))

;;; which-key

(use-package which-key 
  :ensure t 
  :init
  (which-key-mode))

;;; theme & appearance

(set-face-attribute 'default nil :height 145)
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-horizon t)
  (doom-themes-visual-bell-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode 1))

;;; modeline

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120))

;;; initial buffer setup

(persp-rename "develop")
(setq initial-buffer-choice (multi-term))
(rename-buffer "term")
(treemacs)
(switch-to-buffer "term")
(persp-switch "writing")
(org-journal-new-entry 'todo)
(delete-other-windows)
(kill-buffer "*scratch* (writing)")
(persp-switch "develop")

;;; end init.el
