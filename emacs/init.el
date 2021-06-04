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

;;; magit

(use-package magit
  :ensure t)

;;; company (auto-completion)

(use-package company
  :ensure t)
(use-package company-fuzzy
  :ensure t)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'global-company-fuzzy-mode)

;;; counsel, ivy, and swiper

(use-package counsel
  :ensure t)
(use-package ivy
  :ensure t)
(use-package swiper
  :ensure t)
(ivy-mode 1)
(counsel-mode 1)
(global-set-key (kbd "C-s") 'swiper)

;;; neotree

(use-package neotree
  :ensure t
  :config
  (setq neo-theme 'arrow
        neo-smart-open t))
(global-set-key (kbd "C-c C-n") 'neotree)

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

;;; theme & appearance

(set-face-attribute 'default nil :height 165)
(use-package nord-theme
  :ensure t
  :config
  (setq nord-region-highlight "frost"))
(load-theme 'nord t)

;;; initial buffer choice

(setq initial-buffer-choice (multi-term))
(rename-buffer "term_local")

;;; end init.el
