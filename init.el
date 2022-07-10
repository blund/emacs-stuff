
;; Package-greier
(setq package-enable-at-startup nil)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ))
(setq indent-tabs-mode t)
(setq tab-stop-list (number-sequence 2 200 2))
(setq tab-width 2)
(setq indent-line-function 'insert-tab)

(menu-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	'(vlf auto-package-update magit project company eglot use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom theme mappe
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
 )

;; Bare oppdater pakker en gang i uken :)
 (use-package auto-package-update
	 :ensure t
   :if (not (daemonp))
   :custom
	 (auto-package-update-interval 7) ;; in days
   (auto-package-update-prompt-before-update t)
	 (auto-package-update-delete-old-versions t)
	 (auto-package-update-hide-results t)
	 :config
	 (auto-package-update-maybe)
  )


(use-package evil
  :ensure t
  :config
	(add-hook 'prog-mode-hook 'evil-mode)
  )

(use-package magit
  :ensure t
  )

(use-package eglot
  :ensure t
  :config
  (require 'eglot)

  (use-package project
    ;; Hjelp eglot å finne ut hvor prosjekter er
    :ensure t
    :after (eglot)
    )

  (use-package company
    ;; Hyggelig autocomplete
    :ensure t
    :after (eglot)
    :config
    (add-hook 'prog-mode-hook 'company-mode)
    )
  )

(use-package cc-mode
  :after eglot
  :ensure nil 
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  )

	
;bare fordi det er fint med paranteser instant
(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0) ;; Skru av delay for parentes-merking
  :config
  (show-paren-mode +1))

(use-package vlf
	:ensure t
	)
