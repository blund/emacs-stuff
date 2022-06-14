;;; package -- wtf  -*- lexical-binding: t; -*-
;;; Commentary:

;; Sources:
;; [1] - https://github.com/MatthewZMD/.emacs.d
;; [2] - http://emacs-fu.blogspot.com/2010/02/dealing-with-many-buffers-ibuffer.html
;; se på -> [3] - http://xahlee.info/
;; se på -> [4] - https://stackoverflow.com/questions/53697743/use-package-with-config-function-might-not-be-available-at-runtime
;;; Code:

;; [1]
;; Ikke utfør pakke-håndtering
(setq package-enable-at-startup nil)

;;(blink-cursor-mode 0)
(menu-bar-mode -1)
(unless (display-graphic-p)
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(tool-bar-mode -1)

;; Sane tab defaults :)
;;(setq tab-stop-list (number-sequence 0 200 4))

(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default js-switch-indent-offset 4)
(c-set-offset 'comment-intro 0)
(c-set-offset 'innamespace 0)
(c-set-offset 'case-label '+)
(c-set-offset 'access-label 0)
(c-set-offset (quote cpp-macro) 0 nil)
(defun smart-electric-indent-mode ()
  "Disable 'electric-indent-mode in certain buffers and enable otherwise."
  (cond ((and (eq electric-indent-mode t)
              (member major-mode '(erc-mode text-mode)))
         (electric-indent-mode 0))
        ((eq electric-indent-mode nil) (electric-indent-mode 1))))
(add-hook 'post-command-hook #'smart-electric-indent-mode)

;; [1]
;; Skru av eller på linjebryting.
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Endre tekststørrelse.
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Fornuftig hurtigtast for bevegelse opp/ned et paragraf.
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

(global-hl-line-mode)


;; [1]
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ))
;; [1]
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  ;;(package-initialize)                        ; @NOTE - VELDIG USIKKER PÅ DETTE
  )

;; set use-package-verbose to t for interpreted .emacs,
;; and to nil for byte-compiled .emacs.elc.
(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))))

;; [1]
;; Install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))


(use-package auto-package-update
  ;; [1]
  ;; Håndter pakke-oppdateringer hver uke, ikke hver dag..
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))


(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package highlight-numbers
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  )

(use-package magit
  :ensure t
  )

(use-package xclip
  :ensure t
  )

;; Use rm-improved on deletion
(setq delete-by-moving-to-trash t)
(defun system-move-file-to-trash (filename)
  (shell-command (concat (executable-find "rip") " " filename)))

(use-package rainbow-mode
  ;; For å vise fargen til hex strenger.
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode)
  )

(use-package paren
  :ensure nil
  :init
  (setq show-paren-delay 0) ;; Skru av delay for parentes-merking
  :config
  (show-paren-mode +1))

(use-package eglot
  ;; LSP-implementasjon of choice
  :ensure t
  :config

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

  (use-package typescript-mode
    :ensure t
    :after (eglot project)
    :config
    (setenv "NODE_PATH" "/usr/bin")
    ;; https://github.com/joaotavora/eglot/issues/624
    ;; https://github.com/mohkale/emacs/blob/master/init.org#typescript
    (define-derived-mode typescript-react-mode typescript-mode
      "Typescript JSX")

    (put 'typescript-react-mode 'eglot-language-id "typescriptreact")
    (add-to-list 'eglot-server-programs `(typescript-react-mode . ("typescript-language-server" "--stdio")))
    (add-hook 'typescript-react-mode-hook 'eglot-ensure)
    (add-hook 'typescript-react-mode-hook 'company-mode)
    ;; Finn prosjekt med project
    (cl-defmethod project-root ((project (head eglot-project)))
      (cdr project))

    (defun my-project-try-tsconfig-json (dir)
      (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
        (cons 'eglot-project found)))

    (add-hook 'project-find-functions
              'my-project-try-tsconfig-json nil nil)

    ;; Start for tsx også
    (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-react-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-react-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-react-mode))

    )

  (use-package go-mode
    :ensure t
    :after (eglot)
    :config
    (add-hook 'go-mode-hook
              (lambda ()
                  "Hook for å kjøre gofmt når du lagrer."
                  (add-hook 'before-save-hook
                            (lambda ()
                              (progn (gofmt) nil))
                            nil
                            t)))

    ;; Finn prosjekter, fra https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
    (defun project-find-go-module (dir)
      (when-let ((root (locate-dominating-file dir "go.mod")))
        (cons 'go-module root)))
    (cl-defmethod project-root ((project (head go-module)))
      (cdr project))
    (add-hook 'project-find-functions #'project-find-go-module)

    (add-to-list 'eglot-server-programs '((go-mode) "gopls"))
    (add-hook 'go-mode-hook 'eglot-ensure)
    (add-hook 'go-mode-hook 'company-mode)
    )

  ;; Sett opp clangd for c og cpp
  (use-package cc-mode
    :ensure nil ;; cc-mode er builtin, så ikke hent med package manager
    :config
    (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-ensure)
    )
  )

(use-package ibuffer
  ;; [1] Erstatning av vanlige buffere, som lar deg sortere buffere.
  :ensure t
  :bind ("C-x C-b" . ibuffer)
  :init
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))

  ;; Gjem grupper hvis de er tomme.
  (setq ibuffer-show-empty-filter-groups nil)

  ;; [2] Sett opp regler for diverse grupper
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("Org"
                  (mode . org-mode))
                 ("Emacs" ;; Emacs konfigurering
                  (filename . ".config/emacs/"))
                 ("Vipir 1"
                  (filename . "prosjekt/favn/vipir/1/"))
                 ("Vipir 2"
                  (filename . "prosjekt/favn/vipir/2/"))
                 ("Programming" ;; For div programmering som ikke faller under andre kategorier
                  (or
                   (mode . c-mode)
                   (mode . dart-mode)
                   (mode . python-mode)
                   (mode . emacs-lisp-mode)
                   (mode . go-mode)
                   (mode . typescript-mode)
                   (mode . haskell-mode)
                   ;; etc
                   ))
                 ))))

  ;; Legg til hooken som aktiverer filtrene
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")))
  :custom
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))

  )


;;(package-refresh-contents)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.config/emacs/emacs-saves/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
      `((".*" "~/.config/emacs/emacs-saves/" t)))

(add-to-list 'load-path "~/.config/emacs/custom-stuff/")
(require 'elf-mode)
(elf-setup-default)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(create-lockfiles nil)
 '(custom-enabled-themes '(naysayer))
 '(custom-safe-themes
   '("fe6d5c7f964e132b585772e43e6d6cf67aa02f4c0c86c222ecfd6269e24ccfc5" "5d45a95dfd0538c91ee91619bcbb0e8a47e2a299fa3e92ee46bb52d6475438f6" "5001f282dccd7cdf8d58f84d4a5c65853191a5956f646400d774f747f25a2f16" "8da5ccb56b1cc2753bc5578cd0e4e30c27ac542721759357f0086ebff4531a90" "1c001faab3c285cbf8ed0ea37ac4e0114b51ca39012510558265c31d9b9b5eab" "509af944490046bcffab808c4a39f1f093a358fbefb4748f00f7beb4b26bee38" "a317d947943e5925de40217b01f8762271945464fb216a9f2231be0ce7e2beaa" "ad503ecce2f5f758ebd883f951e33a428672beaa04c1ef327497f2cf1cd005b3" "51d400b018190c6dd7d2ada3109e2ac2194eddc02832c8fc0e7f402031c4ab29" default))
 '(package-selected-packages
   '(macro-expand project xclip use-package typescript-mode rainbow-mode ibuffer-vc highlight-numbers go-mode flycheck exec-path-from-shell eglot company auto-package-update)))

;; (set-frame-font "PxPlus IBM VGA8 12" nil t)
(set-frame-font "Meslo LG S 10" nil t)

;;; Generelle greier

;; (global-highlight-numbers-mode)

(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))


;;; Typescript

;; ;;; Go


(flycheck-define-checker dart
  "Dart static analyzer using dartanalyze.
https://github.com/dart-lang/sdk/tree/master/pkg/analyzer_cli#dartanalyzer"
  :command ("dart" "analyze" "--format=machine")
  :error-patterns
  ((error line-start "ERROR" "|" (= 2 (+ (any "A-Z" "a-z" "0-9" "_")) "|")
          (file-name) "|" line "|" column "|" (one-or-more (any digit)) "|"
          (message) line-end)

   (warning line-start "WARNING" "|" (= 2 (+ (any "A-Z" "a-z" "0-9" "_")) "|")
          (file-name) "|" line "|" column "|" (one-or-more (any digit)) "|"
          (message) line-end)

   (info line-start "INFO" "|" (= 2 (+ (any "A-Z" "a-z" "0-9" "_")) "|")
          (file-name) "|" line "|" column "|" (one-or-more (any digit)) "|"
          (message) line-end))
  :modes dart-mode)


(add-to-list 'flycheck-checkers 'dart)
(add-hook 'dart-mode-hook 'dart-server)
(add-hook 'dart-mode-hook 'flycheck-mode)

(require 'xclip)
(xclip-mode 1)

(defun gtags-create-or-update ()
  "Create or update the gnu global tag file."
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p")))     (let ((olddir default-directory)
          (topdir (read-directory-name
                    "gtags: top of source tree:" default-directory)))
      (cd topdir)
      (shell-command "gtags && echo 'created tagfile'")
      (cd olddir))         (shell-command "global -u && echo 'updated tagfile'")))

;;; Konfigurering av backup-filer
(setq backup-directory-alist '(("." . "~/.config/emacs/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(provide '.emacs)
;;; .emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
