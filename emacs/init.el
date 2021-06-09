;;; init.el --- initialization file for emacs

;;; Commentary:

;; startup file

;;; Code:

;;;; package sources & initialization

(require 'package)

(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))

(let ((use-package 'use-package))
  (unless (package-installed-p use-package)
    (package-install use-package)))

;;;; custom emacs defaults

(defun initial-buffer-setup ()
  "Create preferred initial buffers & startup commands."
  (persp-rename "develop")
  (multi-term)
  (rename-buffer "term")
  (treemacs)
  (treemacs-collapse-all-projects)
  (switch-to-buffer "term")
  (persp-switch "writing")
  (org-journal-new-entry 'todo)
  (delete-other-windows)
  (persp-switch "develop"))

(add-hook 'emacs-startup-hook 'initial-buffer-setup)
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen t)

(electric-indent-mode -1)
(global-display-line-numbers-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c C-r") 'rename-buffer)

(setq auto-save-file-name-transforms `((".*" "~/.backups/" t)))
(setq backup-directory-alist '(("." . "~/.backups/")))
(setq custom-file null-device)
(setq inhibit-splash-screen t)
(setq use-package-always-ensure t)
(setq-default tab-width 4)

;;;; additional package installation & configuration (in alphabetical order)

;;;;; all-the-icons

(use-package all-the-icons
  :after
  (treemacs))

;;;;; company

(use-package company
  :init
  (global-company-mode 1))

(use-package company-fuzzy
  :after
  (company)
  :init
  (global-company-fuzzy-mode 1))

;;;;; counsel

(use-package counsel
  :after
  (ivy)
  :init
  (counsel-mode 1))

;;;;; flycheck

(use-package flycheck
  :bind
  (:map flycheck-mode-map
	("C-c e l" . flycheck-list-errors)
	("C-c e n" . flycheck-next-error)
	("C-c e p" . flycheck-previous-error))
  :init
  (global-flycheck-mode))

;;;;; helm

(use-package helm
  :init
  (helm-mode 1))

;;;;; ivy

(use-package ivy
  :init
  (ivy-mode 1))

;;;;; magit

(use-package magit
  :config
  (setq magit-post-display-buffer-hook
      #'(lambda ()
          (when (derived-mode-p 'magit-status-mode)
            (delete-other-windows)))))

;;;;; markdown

(use-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :init
  (setq markdown-command "/usr/local/bin/multimarkdown")
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)))

;;; modeline

(use-package doom-modeline
  :after
  (doom-themes)
  :config
  (setq doom-modeline-height 1)
  (set-face-attribute 'mode-line nil :height 120)
  (set-face-attribute 'mode-line-inactive nil :height 120)
  :init
  (doom-modeline-mode 1))

;;;;; multi-term

(defun inhibit-display-line-numbers-mode ()
  "Disable 'display-line-numbers-mode'."
  (add-hook 'after-change-major-mode-hook
			(lambda () (display-line-numbers-mode 0))
			:append :local))

(use-package multi-term
  :bind
  (:map global-map
		("C-c C-t" . multi-term))
  :config
  (add-hook 'term-mode-hook 'inhibit-display-line-numbers-mode))

;;;;; org-journal

(use-package org-journal
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"
		org-journal-file-type 'weekly))

;;;;; perspective

(use-package perspective
  :bind
  (:map global-map
		("C-x C-b" . persp-counsel-switch-buffer))
  :commands
  (persp-rename persp-switch)
  :init
  (persp-mode))

;;;;; plantuml

(use-package plantuml-mode
  :config
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-jar-path (expand-file-name "~/.config/emacs/plantuml/plantuml.jar"))
  (setq org-plantuml-jar-path (expand-file-name "~/.config/emacs/plantuml/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;;;;; projectile

(use-package projectile
  :bind
  (:map global-map
	("C-x f" . find-file)
	("C-x C-f" . projectile-find-file))
  (:map projectile-mode-map
	("C-x p" . projectile-command-map)
	("C-x p a" . projectile-add-known-project)
	("C-x p r" . projectile-remove-known-project))
  :init
  (projectile-mode 1))

;;;;; solaire

(use-package solaire-mode
  :after
  (doom-themes)
  :init
  (solaire-global-mode 1))

;;;;; swiper

(use-package swiper
  :after
  (ivy)
  :bind
  (:map global-map
		("C-s" . swiper)
		("C-r" . swiper-backward)))

;;;;; theme

(use-package doom-themes
  :config
  (load-theme 'doom-horizon t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (setq doom-themes-treemacs-theme "doom-colors")
  (set-face-attribute 'default nil :height 145))

;;;;; treemacs

(use-package treemacs
  :bind
  (:map global-map
        ("C-x t t" . treemacs)
		("C-x t a" . treemacs-add-project-to-workspace)
        ("C-x t d" . treemacs-remove-project-from-workspace)
		("C-x t h" . treemacs-narrow-to-current-file)
		("C-x t q" . treemacs-add-and-display-current-project))
  :commands
  (treemacs-collapse-all-projects)
  :config
  (setq treemacs-width 30)
  (add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1))))

(use-package treemacs-projectile
  :after
  (treemacs projectile)
  :bind
  (:map global-map
		("C-x t p" . treemacs-projectile)))

(use-package treemacs-magit
  :after
  (treemacs magit))

;;;;; which-key

(use-package which-key
  :init
  (which-key-mode))

;;;; end package sources & initialization

(provide 'init)

;;; init.el ends here
