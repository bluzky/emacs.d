;;; doom-plain-dark-theme.el --- inspired by gko's plain theme for VSCode -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'doom-themes)

(def-doom-theme me-plain
  "Theme inspired by gko's plain dark."

  ;; name        default   256       16
  ;; ((bg         '("#222222" nil       nil ))
  ((bg         '("#282b35" nil       nil ))
   (bg-alt     (doom-lighten bg 0.15))
   (base0      '("#838083" nil nil ))
   (base1      '("#0e0c0a" nil nil ))
   (base2      '("#bbbbbb" nil nil ))
   (base3      '("#444444" nil nil ))
   (base4      '("#202020" nil nil ))
   (base5      '("#545053" nil nil ))
   (base6      '("#050505" nil nil ))
   (base7      '("#ffdddd" nil nil ))
   (base8      '("#050505" nil nil ))
   (fg         '("#d7d5d1" nil nil ))
   (fg-alt     '("#e7e5e3" nil nil ))

   (grey       fg)
   (red        fg)
   (blue       fg)
   (dark-blue  fg)
   (orange     fg)
   (green      fg)
   (teal       fg)
   (yellow     fg)
   (magenta    fg)
   (violet     fg)
   (cyan       fg)
   (dark-cyan  fg)

   ;; face categories -- required for all themes
   (highlight      base2)
   (vertical-bar   fg)
   (selection      base1)
   (builtin        base0)
   (comments       base0)
   (doc-comments   base0)
   ;; (constants      fg)
   (constants      base0)
   (functions      fg)
   (keywords       fg)
   (methods        fg)
   (operators      fg)
   (type           fg)
   (strings        "#a6c1e0")
   (variables      fg)
   (numbers        "#5b94ab")
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    (doom-darken fg 0.4))
   (vc-added       (doom-lighten fg 0.4))
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad 4)

   (modeline-bg
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0)))
   (modeline-bg-l
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0)))
   (modeline-bg-inactive   (doom-darken bg-alt 0.25))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1)))
   (modeline-fg     nil)
   (modeline-fg-alt (doom-lighten modeline-bg-inactive 0.3)))


  ;;;; Base theme face overrides
  (((font-lock-constant-face &override)      :slant 'italic)
   ((font-lock-comment-face &override)       :slant 'italic)
   ((font-lock-function-name-face &override) :bold t)
   ((font-lock-type-face &override)          :bold t)
   ((font-lock-keyword-face &override)       :bold t)
   (hl-line :background base8)
   ((line-number &override) :foreground base3)
   ((line-number-current-line &override) :foreground base2)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground highlight)

   ;;;; doom-modeline
   (doom-modeline-bar :background highlight)
   ;;;; lsp-mode
   (lsp-headerline-breadcrumb-symbols-face :foreground keywords :weight 'bold)
   ;;;; outline <built-in>
   (outline-1 :slant 'italic :foreground fg-alt)
   (outline-2 :inherit 'outline-1 :foreground base2)
   (outline-3 :inherit 'outline-2)
   (outline-4 :inherit 'outline-3)
   (outline-5 :inherit 'outline-4)
   (outline-6 :inherit 'outline-5)
   (outline-7 :inherit 'outline-6)
   (outline-8 :inherit 'outline-7)
   ;;;; org <built-in>
   (org-block-begin-line :foreground base2 :background base3)
   (org-block-end-line :foreground base2 :background base3)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))))

;;; doom-plain-dark-theme.el ends here
