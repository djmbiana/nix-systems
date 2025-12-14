;;; gruvchad-theme.el --- a port of nvchad's gruvbox theme -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Derrick Biana
;; Source: https://github.com/sainnhe/gruvbox-material
;;
;;; Commentary:
;; A warm, earthy theme inspired by Gruvbox Material and Gruvchad.
;;; Code:

(require 'doom-themes)

;;; Variables

(defgroup gruvchad-theme nil
  "Options for the `gruvchad' theme."
  :group 'doom-themes)

(defcustom gruvchad-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'gruvchad-theme
  :type 'boolean)

(defcustom gruvchad-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'gruvchad-theme
  :type 'boolean)

(defcustom gruvchad-comment-bg gruvchad-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'gruvchad-theme
  :type 'boolean)

(defcustom gruvchad-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'gruvchad-theme
  :type '(choice integer boolean))


;;; Theme definition

(def-doom-theme gruvchad
    "A warm, earthy theme inspired by Gruvbox Material"

  ;; name        default   256       16
  ((bg         '("#1e2122" nil       nil            ))  ; warm dark background
   (bg-alt     '("#282b2c" nil       nil            ))  ; slightly lighter panels
   (base0      '("#1a1d1e" "black"   "black"        ))
   (base1      '("#242728" "#242728" "brightblack"  ))
   (base2      '("#2c2f30" "#2c2f30" "brightblack"  ))
   (base3      '("#36393a" "#36393a" "brightblack"  ))
   (base4      '("#404344" "#404344" "brightblack"  ))
   (base5      '("#484b4c" "#484b4c" "brightblack"  ))
   (base6      '("#575a5b" "#575a5b" "brightblack"  ))
   (base7      '("#c0b196" "#c0b196" "brightblack"  ))
   (base8      '("#c7b89d" "#c7b89d" "white"        ))
   (fg         '("#c7b89d" "#c7b89d" "brightwhite"  ))  ; warm parchment
   (fg-alt     '("#c0b196" "#c0b196" "white"        ))  ; slightly muted

   (grey       base5)
   (red        '("#ec6b64" "#ec6b64" "red"          ))  ; warm red
   (orange     '("#e78a4e" "#e78a4e" "brightred"    ))  ; vibrant orange
   (green      '("#89b482" "#89b482" "green"        ))  ; soft green
   (blue       '("#6d8dad" "#6d8dad" "brightblue"   ))  ; muted blue
   (yellow     '("#d6b676" "#d6b676" "yellow"       ))  ; warm yellow
   (violet     '("#9385b4" "#9385b4" "magenta"      ))  ; soft purple
   (teal       '("#749689" "#749689" "brightgreen"  ))  ; muted teal
   (dark-blue  '("#6f8faf" "#6f8faf" "blue"         ))  ; nord blue
   (magenta    '("#d3869b" "#d3869b" "brightmagenta"))  ; soft magenta
   (cyan       '("#82b3a8" "#82b3a8" "brightcyan"   ))  ; aqua cyan
   (dark-cyan  '("#7daea3" "#7daea3" "cyan"         ))  ; darker cyan

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken bg 0.25))
   (selection      base3)
   (builtin        cyan)
   (comments       base5)
   (doc-comments   base6)
   (constants      orange)
   (functions      dark-cyan)
   (keywords       red)
   (methods        blue)
   (operators      fg-alt)
   (type           yellow)
   (strings        green)
   (variables      fg)
   (numbers        violet)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg-alt) "black" "black"))
   (-modeline-bright gruvchad-brighter-modeline)
   (-modeline-pad
    (when gruvchad-padded-modeline
      (if (integerp gruvchad-padded-modeline) gruvchad-padded-modeline 4)))

   (modeline-fg     'unspecified)
   (modeline-fg-alt (doom-blend violet base4 (if -modeline-bright 0.5 0.2)))
   (modeline-bg
    (if -modeline-bright
        (doom-darken base3 0.1)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken base3 0.05)
      base1))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1)))
   (modeline-bg-inactive-l (doom-darken bg 0.1)))


  ;;;; Base theme face overrides
  (((font-lock-comment-face &override)
    :background (if gruvchad-comment-bg (doom-lighten bg 0.05)))
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if -modeline-bright base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground fg)
   (css-selector             :foreground red)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background base3 :foreground base1)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-darken bg 0.1))
   ;; outline <built-in>
   ((outline-1 &override) :foreground red :weight 'ultra-bold)
   ((outline-2 &override) :foreground orange)
   ((outline-3 &override) :foreground green)
   ((outline-4 &override) :foreground blue)
   ((outline-5 &override) :foreground violet)
   ((outline-6 &override) :foreground cyan)
   ((outline-7 &override) :foreground yellow)
   ((outline-8 &override) :foreground fg)
   ;;;; org <built-in>
   (org-block            :background (doom-darken bg-alt 0.04))
   (org-block-begin-line :foreground base4 :slant 'italic :background (doom-darken bg 0.04))
   (org-ellipsis         :underline nil :background bg    :foreground red)
   ((org-quote &override) :background base1)
   (org-hide :foreground bg)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l))))

  ;;;; Base theme variable overrides
  ;; ()
  )

;;; gruvchad-theme.el ends here
