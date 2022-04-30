;;; package -- wtf  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Sources:
;; [1] - https://github.com/MatthewZMD/.emacs.d

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
;; Select the folder to store packages
;; Comment / Uncomment to use desired sites
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory)
      package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("cselpa" . "https://elpa.thecybershadow.net/packages/")
        ;; ("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
        ;; ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
        ))
;; [1]
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))
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

;; [1]
;; Håndter pakke-oppdateringer hver uke, ikke hver dag..
(use-package auto-package-update
  :if (not (daemonp))
  :custom
  (auto-package-update-interval 7) ;; in days
  (auto-package-update-prompt-before-update t)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

;; [1]
;; Truncate lines
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
;; Adjust font size like web browsers
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)
;; Move up/down paragraph
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)

    
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
  (use-package ibuffer-vc
    :commands (ibuffer-vc-set-filter-groups-by-vc-root)
    :custom
    (ibuffer-vc-skip-if-remote 'nil))
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
           " " filename))))
    
;; (package-refresh-contents)
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.config/emacs/emacs-saves/"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
(setq auto-save-file-name-transforms
      `((".*" "~/.config/emacs/emacs-saves/" t)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(create-lockfiles nil)
 '(custom-enabled-themes '(misterioso))
 '(package-selected-packages
   '(auto-package-update use-package prettier evil eglot typescript-mode xclip gradle-mode dart-server magit go-mode undo-tree ##)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#172021" :foreground "#c9b99f" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :width normal :family "Inconsolata"))))
 '(font-lock-builtin-face ((t :foreground "#c9b99f")))
 '(font-lock-comment-face ((t (:foreground "#28d43c"))))
 '(font-lock-constant-face ((t (:foreground "#c9b99f"))))
 '(font-lock-doc-string-face ((t (:foreground "#28d43c"))))
 '(font-lock-function-name-face ((t (:foreground "#c9b99f" :weight normal))))
 '(font-lock-keyword-face ((t (:foreground "#f7d7d5" :weight bold))))
 '(font-lock-string-face ((t (:foreground "#fc7923"))))
 '(font-lock-type-face ((t (:foreground "#a7f9be"))))
 '(font-lock-variable-name-face ((t (:foreground "#c9b99f")))))


;;; Generelle greier
(global-flycheck-mode)

(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))


;;; Bruk IBuffer for å organisere buffre
;;(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(setq ibuffer-saved-filter-groups
  ;; (quote (("default"      
  ;;           ("Org" ;; all org-related buffers
  ;;             (mode . org-mode))  
  ;;           ("vipir 1"
  ;;             (filename . "prosjekt/favn/vipir/1/"))
  ;;           ("vipir 2"
  ;;             (filename . "prosjekt/favn/vipir/2/"))
  ;;           ("Programming" ;; prog stuff not already in MyProjectX
  ;;             (or
  ;;               (mode . c-mode)
;;                 (mode . dart-mode)
;;                 (mode . python-mode)
;;                 (mode . emacs-lisp-mode)
;;                 (mode . go-mode)
;;                 (mode . typescript-mode)
;;                 (mode . haskell-mode)
;;                 ;; etc
;;                 )) 
;;             ))))

;; (add-hook 'ibuffer-mode-hook
;;   (lambda ()
;;     (ibuffer-switch-to-saved-filter-groups "default")))


;;; Typescript
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; ;;; Go
(defun go-fmt-hook ()
  (add-hook 'before-save-hook
    (lambda ()
    (progn (gofmt) nil))
    nil
    t))


(add-hook 'go-mode-hook
          (lambda () (go-fmt-hook)))


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
  "create or update the gnu global tag file"
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
