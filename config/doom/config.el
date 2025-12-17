(setq doom-font (font-spec :family "Ubuntu Mono Nerd Font" :size 16))
(setq display-line-numbers-type 'relative)

;;dashboard settings
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(setq fancy-splash-image "~/.config/doom/img/xemacs_color.png")

;; custom dashboard footer with dharmic wheel
(defun +my/dashboard-footer ()
  (insert "\n\n")
  (let ((start (point)))
    (insert (+doom-dashboard--center
             +doom-dashboard--width
             (or (nerd-icons-mdicon "nf-md-dharmachakra"
                                   :face `(:foreground "#ffffff"
                                          :height 1.3
                                          :v-adjust -0.15))
                 (propertize "☸"
                            'face '(:height 1.3
                                   :v-adjust -0.15
                                   :inherit doom-dashboard-footer-icon)))))
    (make-text-button start (point)
                      'action (lambda (_) (browse-url "https://github.com/djmbiana"))
                      'follow-link t
                      'help-echo "my github."
                      'face 'doom-dashboard-footer-icon
                      'mouse-face 'highlight))
  (insert "\n"))

;; Hook everything in order
(add-hook! '+doom-dashboard-functions :append
  #'+my/dashboard-footer
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Seek for enlightenment, Marie.")))

;; font settings
(setq doom-font (font-spec :family "UbuntuMono Nerd Font" :size 16)
      doom-variable-pitch-font (font-spec :family "Ubuntu Nerd Font" :size 18)
      doom-big-font (font-spec :family "UbuntutMono Nerd Font" :size 22))

;; set theme
(add-to-list 'custom-theme-load-path "~/.config/doom/themes/")
(load-theme 'doom-gruvbox t)

;; remove top frame bar in emacs
(add-to-list 'default-frame-alist '(undecorated . t))

;; go to nix config binding
(map! :leader
      (:prefix ("f" . "file")
       :desc "Open custom directory"
       "c" (λ! (find-file "~/nix-systems"))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
(setq org-roam-directory "~/Documents/org/roam")
(setq org-modern-table-vertical 1)
(setq org-modern-table t)
(add-hook 'org-mode-hook #'hl-todo-mode)

;; sets the different headers in org mode
(custom-set-faces!
  '(org-level-8 :inherit outline-3 :height 1.0)
  '(org-level-7 :inherit outline-3 :height 1.0)
  '(org-level-6 :inherit outline-3 :height 1.1)
  '(org-level-5 :inherit outline-3 :height 1.2)
  '(org-level-4 :inherit outline-3 :height 1.3)
  '(org-level-3 :inherit outline-3 :height 1.4)
  '(org-level-2 :inherit outline-2 :height 1.5)
  '(org-level-1 :inherit outline-1 :height 1.6)
  '(org-document-title :height 1.8 :bold t :underline nil))

;; focus function for capture
(defun my/focus-and-capture ()
  "Focus main Emacs frame and open org-capture."
  (interactive)
  ;; Try to find the main emacs frame (not popup)
  (let ((main-frame (seq-find
                     (lambda (f)
                       (not (string= (frame-parameter f 'name) "emacs-popup")))
                     (frame-list))))
    (when main-frame
      (select-frame-set-input-focus main-frame))
    (org-capture)))

;; Notes popup function
(defun my/notes-popup ()
  "Open notes.org in a new popup frame."
  (interactive)
  (my/create-popup-frame (find-file-noselect "~/Documents/org/notes.org") 100 40)
  (org-mode))

(use-package! dirvish
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode)

  :config
  ;; Use nerd font icons
  (setq dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index)))

  ;; Dirvish attributes (what info to show)
  (setq dirvish-attributes
        '(nerd-icons file-size collapse subtree-state vc-state git-msg))

  ;; Enable preview for various file types
  (setq dirvish-preview-dispatchers
        '(image gif video audio epub pdf archive))

  ;; Cache directory
  (setq dirvish-cache-dir (concat doom-cache-dir "dirvish/"))

  ;; Quick access directories
  (setq dirvish-quick-access-entries
        '(("h" "~/"                 "Home")
          ("d" "~/Downloads/"       "Downloads")
          ("D" "~/Documents/"       "Documents")
          ("p" "~/projects/"        "Projects")
          ("c" "~/.config/"         "Config")
          ("n" "~/nix-systems/"     "NixOS Config")))

  ;; Keybindings within dirvish
  (map! :map dirvish-mode-map
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file
        :n "?" #'dirvish-dispatch
        :n "q" #'dirvish-quit
        :n "TAB" #'dirvish-subtree-toggle
        :n "v" #'dirvish-toggle-preview
        :n "f" #'dirvish-file-info-menu
        :n "y" #'dirvish-yank
        :n "s" #'dirvish-quicksort
        :n "a" #'dirvish-quick-access))

;; Functions
(defun my/dirvish-here ()
  "Open dirvish in the directory of the current buffer."
  (interactive)
  (let ((dir (or (when (buffer-file-name)
                   (file-name-directory (buffer-file-name)))
                 default-directory)))
    (dirvish dir)))

(defun my/focus-and-open-dirvish ()
  "Focus Emacs window and open dirvish in home directory."
  (interactive)
  (select-frame-set-input-focus (selected-frame))
  (dirvish "~/"))

;; Leader keybindings
(map! :leader
      :desc "Open dirvish here" "o f" #'my/dirvish-here
      :desc "Open dirvish home" "o F" #'dirvish)

;; Dirvish popup function for Niri
(defun my/dirvish-popup ()
  "Open dirvish in a new popup frame."
  (interactive)
  (let ((dir (or (when (buffer-file-name)
                   (file-name-directory (buffer-file-name)))
                 "~/")))
    (my/create-popup-frame "*dirvish*" 120 35)
    (dirvish dir)))

;; Update leader keybindings (replaces existing leader keybindings section)
(map! :leader
      :desc "Open dirvish here" "o f" #'my/dirvish-here
      :desc "Open dirvish home" "o F" #'dirvish
      :desc "Open dirvish popup" "o d" #'my/dirvish-popup
      :desc "Open notes popup" "o n" #'my/notes-popup)

(after! eww
  (set-popup-rule! "^\\*eww\\*" :ignore t))

(require 'elfeed)
(require 'elfeed-goodies)
(require 'elfeed-tube)
(require 'elfeed-tube-mpv)

(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)

(elfeed-tube-setup)

(setq elfeed-feeds
      (quote
        (;; News
        ("https://www.rappler.com/feed/" news philippines)
        ("https://www.philstar.com/rss/headlines" news philippines)
        ("https://www.inquirer.net/fullfeed" news philippines)
        ("https://www.theguardian.com/international/rss" news international)
        ("https://www.bbc.com/news/rss.xml" news international)

        ;; Music
        ("https://www.kerrang.com/feed/" music)
        ("https://thequietus.com/feed" music)
        ("https://daily.bandcamp.com/feed/" music)
        ("https://www.hearingthings.co/feed/" music)
        ("https://theneedledrop.com/feed/" music reviews)
        ("https://pitchfork.com/rss/reviews/albums/" music reviews)

        ;; Tech
        ("https://itsfoss.com/feed/" tech linux)
        ("https://www.linuxuprising.com/feeds/posts/default" tech linux)
        ("https://www.phoronix.com/rss.php" tech linux)
        ("https://www.wsj.com/xml/rss/3_7455.xml" tech business)
        ("https://www.wired.com/feed/rss" tech)
        ("https://www.pcworld.com/feed" tech)
        ("https://news.ycombinator.com/rss" tech hn)

        ;; Reddit
        ("https://www.reddit.com/r/Philippines/.rss" reddit philippines)
        ("https://www.reddit.com/r/Buddhism/.rss" reddit buddhism)

        ;; YouTube Channels
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCxQKHvKbmSzGMvUrVtJYnUA" jvscholz)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC_O4q0rsz5wKYBm3eKOjG0w" reysu)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" distrotube)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCl2mFZoRqjw_ELax4Yisf6w" louis_rossmann)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2wKfjlioOCLP4xQMOWNcgg" mental_outlaw)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCBZxfcALb7lqNR_bW2OVouw" breadonpenguins)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCqrKe-6YBshFzejCo3aySSQ" joshua_blais)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCXQmdfCcGHWdkKYG4b0cw8w" sylvan_franklin))))

;; Set browser to eww
(setq browse-url-browser-function 'eww-browse-url)

;; Keybindings - set after elfeed loads
(evil-define-key 'normal elfeed-show-mode-map
                  (kbd "J") 'elfeed-goodies/split-show-next
                  (kbd "K") 'elfeed-goodies/split-show-prev
                  (kbd "V") 'elfeed-tube-mpv)
(evil-define-key 'normal elfeed-search-mode-map
                  (kbd "J") 'elfeed-goodies/split-show-next
                  (kbd "K") 'elfeed-goodies/split-show-prev)

(defun thanos/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype \"%s\""
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype \"%s\" && wtype -k Return"
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(define-minor-mode thanos/type-mode
  "Minor mode for inserting text via wtype."
  :keymap `((,(kbd "C-c C-c") . ,(lambda () (interactive)
                                   (call-process-shell-command
                                    (thanos/wtype-text (buffer-string))
                                    nil 0)
                                   (delete-frame)))
            (,(kbd "C-c C-k") . ,(lambda () (interactive)
                                   (kill-buffer (current-buffer))))))

(defun thanos/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (title . "emacs-float")  ;; Explicitly set title
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (thanos/type-mode)
      (setq-local header-line-format
                  (format " %s to insert text or %s to cancel."
                          (propertize "C-c C-c" 'face 'help-key-binding)
                          (propertize "C-c C-k" 'face 'help-key-binding)))
      ;; Make the frame more temporary-like
      (set-frame-parameter frame 'delete-before-kill-buffer t)
      ;; Explicitly set title again
      (set-frame-parameter frame 'title "emacs-float")
      (set-window-dedicated-p (selected-window) t))))

(setq vterm-shell "/etc/profiles/per-user/marie/bin/zsh")
(setq vterm-environment '("TERM=xterm-256color"))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(custom-set-faces!
  '(vterm :family "UbuntuMono Nerd Font"))

(defun my/create-popup-frame (buffer-or-name &optional width height)
  "Create a popup frame for BUFFER-OR-NAME with optional WIDTH and HEIGHT."
  (let ((frame (make-frame `((name . "emacs-popup")
                             (undecorated . t)
                             (width . ,(or width 100))
                             (height . ,(or height 30))
                             (minibuffer . t)))))
    (select-frame frame)
    (switch-to-buffer buffer-or-name)
    (set-frame-parameter frame 'delete-before-kill-buffer t)
    frame))

(setq confirm-kill-processes nil)

;; Configure pyright
(after! lsp-pyright
  (setq lsp-pyright-multi-root nil))
