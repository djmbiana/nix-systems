;;; vague-theme.el --- A dark, muted theme inspired by Vague -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: 
;; Maintainer:
;; Source: https://github.com/vague-theme/vague-alacritty
;;
;;; Commentary:
;; A dark theme with muted, carefully balanced colors.
;;; Code:

(require 'doom-themes)

;;; Variables

(defgroup vague-theme nil
  "Options for the `vague' theme."
  :group 'doom-themes)

(defcustom vague-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'vague-theme
  :type 'boolean)

(defcustom vague-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'vague-theme
  :type 'boolean)

(defcustom vague-comment-bg vague-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'vague-theme
  :type 'boolean)

(defcustom vague-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'vague-theme
  :type '(choice integer boolean))


;;; Theme definition

(def-doom-theme vague
    "A dark, muted theme inspired by Vague"

  ;; name        default   256       16
  ((bg         '("#141415" nil       nil            ))  ; deep dark background
   (bg-alt     '("#1e1e1f" nil       nil            ))  ; slightly lighter panels
   (base0      '("#0f0f10" "black"   "black"        ))
   (base1      '("#1a1a1b" "#1a1a1b" "brightblack"  ))
   (base2      '("#252530" "#252530" "brightblack"  ))
   (base3      '("#2d2d38" "#2d2d38" "brightblack"  ))
   (base4      '("#3d3d48" "#3d3d48" "brightblack"  ))
   (base5      '("#606079" "#606079" "brightblack"  ))
   (base6      '("#9d9daf" "#9d9daf" "brightblack"  ))
   (base7      '("#c0c0c8" "#c0c0c8" "brightblack"  ))
   (base8      '("#d7d7d7" "#d7d7d7" "white"        ))
   (fg         '("#cdcdcd" "#cdcdcd" "brightwhite"  ))  ; soft foreground
   (fg-alt     '("#9d9daf" "#9d9daf" "white"        ))  ; muted secondary

   (grey       base4)
   (red        '("#d8647e" "#d8647e" "red"          ))  ; soft red
   (orange     '("#f3be7c" "#f3be7c" "brightred"    ))  ; warm orange/yellow
   (green      '("#7fa563" "#7fa563" "green"        ))  ; muted green
   (blue       '("#6e94b2" "#6e94b2" "brightblue"   ))  ; muted blue
   (yellow     '("#f5cb96" "#f5cb96" "yellow"       ))  ; bright yellow
   (violet     '("#c9b1ca" "#c9b1ca" "magenta"      ))  ; soft violet
   (teal       '("#aeaed1" "#aeaed1" "brightgreen"  ))  ; muted teal
   (dark-blue  '("#5a7a92" "#5a7a92" "blue"         ))  ; darker blue
   (magenta    '("#bb9dbd" "#bb9dbd" "brightmagenta"))  ; muted magenta
   (cyan       '("#bebeda" "#bebeda" "brightcyan"   ))  ; bright cyan
   (dark-cyan  '("#8ba9c1" "#8ba9c1" "cyan"         ))  ; darker cyan

   ;; face categories -- required for all themes
   (highlight      yellow)
   (vertical-bar   (doom-darken bg 0.25))
   (selection      base3)
   (builtin        magenta)
   (comments       base5)
   (doc-comments   base6)
   (constants      orange)
   (functions      blue)
   (keywords       magenta)
   (methods        cyan)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg-alt) "black" "black"))
   (-modeline-bright vague-brighter-modeline)
   (-modeline-pad
    (when vague-padded-modeline
      (if (integerp vague-padded-modeline) vague-padded-modeline 4)))

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
    :background (if vague-comment-bg (doom-lighten bg 0.05)))
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
   ((outline-1 &override) :foreground fg :weight 'ultra-bold)
   ((outline-2 &override) :foreground (doom-blend fg blue 0.35))
   ((outline-3 &override) :foreground (doom-blend fg blue 0.7))
   ((outline-4 &override) :foreground blue)
   ((outline-5 &override) :foreground (doom-blend magenta blue 0.2))
   ((outline-6 &override) :foreground (doom-blend magenta blue 0.4))
   ((outline-7 &override) :foreground (doom-blend magenta blue 0.6))
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

;;; vague-theme.el ends here
