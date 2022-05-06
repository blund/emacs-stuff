
;;; naysayer-theme.el --- The naysayer color theme

;; Author: Børge Lundsaunet <mail@blund.site>
;; Version: 1.0
;; Filename: naysayer-theme.el
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/nickav/naysayer-theme.el
;; License: GPL-3+

;;; Commentary:
;; Based on Nick Aversano's recreation of Jonathan Blow's emacs theme

;;; Code:

(unless (>= emacs-major-version 24)
  (error "The naysayer theme requires Emacs 24 or later!"))

(deftheme naysayer "The wannabe Jonathan Blow theme.")


;; @MERK - du kan se fargene med rainbow mode
(let ((background     "#172021")
      (gutters        "#062329")
      (gutter-fg      "#062329")
      (gutters-active "#062329")
      (builtin        "#ffffff")
      (selection      "#473f96")
      (text           "#d1b897")
      (comments       "#28d43c")
      (types          "#a7f9be")
      (keywords       "#f7d7d5")
      (strings        "#fc7923")
      (constants      "#7ad0c6")
      (macros         "#8cde94")
      (numbers        "#7ad0c6")
      (white          "#ffffff")
      (error          "#ff0000")
      (warning        "#ffaa00")
      (highlight-line "#0b3335") ; For hl-mode (current line highlight)
      (bot-bar-bg     "#273031") ; For unfocused bottom bar
      (line-fg        "#126367"))



  (custom-theme-set-faces
   'naysayer


   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,white                        ))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords :weight bold))))
   `(font-lock-type-face              ((t (:foreground ,types))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,text))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-doc-string-face        ((t (:foreground ,comments))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))
   `(font-lock-function-name-face     ((t (:foreground ,text))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   `(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line ((t (:foreground ,white :background ,background))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :text ,text :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background ,text
                                   :box nil))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,bot-bar-bg
                                            :box nil))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,numbers))))
  )

  (custom-theme-set-variables
    'naysayer
    '(linum-format " %4i ")
  )
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'naysayer)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'naysayer-theme)

;;; naysayer-theme.el ends here
