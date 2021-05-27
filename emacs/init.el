; package sources

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; make sure use-package is installed to install other packages

(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

; general

(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(setq inhibit-splash-screen t) 
(electric-indent-mode -1)
(global-display-line-numbers-mode 1)

;; general keybindings

(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c C-r") 'rename-buffer)

; backups & autosaves
(setq backup-directory-alist '(("." . "~/.backups/")))
(setq auto-save-file-name-transforms `((".*" "~/.backups/" t)))

; counsel & related 

(use-package counsel
  :ensure t)
(ivy-mode)
(counsel-mode)
(global-set-key (kbd "C-s") 'swiper)

; file system navigation

(use-package neotree
  :ensure t
  :init
  (setq neo-theme 'arrow
        neo-smart-open t))
(global-set-key (kbd "C-c C-n") 'neotree)

; term

(global-set-key (kbd "C-c C-t") 'multi-term)
(add-hook 'term-mode-hook 'inhibit-display-line-numbers-mode)
(defun inhibit-display-line-numbers-mode ()
  "Disable display-line-numbers-mode"
  (add-hook 'after-change-major-mode-hook
            (lambda () (display-line-numbers-mode 0))
            :append :local))
;; the initial buffer is set here because it's a term window, and we need the term-mode-hook defined before opening a term buffer
(setq initial-buffer-choice (multi-term))
(rename-buffer "term_local")
(use-package multi-term
  :ensure t)

; markdown

(custom-set-variables
 '(markdown-command "/opt/homebrew/bin/multimarkdown"))
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

; theme & appearance

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-face-attribute 'default (selected-frame) :height 165)
(use-package nord-theme
  :ensure t
  :init
  (setq nord-region-highlight "frost"))
(load-theme 'nord t)

; end init.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
