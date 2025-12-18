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
      doom-variable-pitch-font (font-spec :family "UbuntuMono Nerd Font" :size 18)
      doom-big-font (font-spec :family "UbuntuMono Nerd Font" :size 22))

;; set theme
(add-to-list 'custom-theme-load-path "~/.config/doom/themes/")
(load-theme 'doom-tomorrow-night t)

;; remove top frame bar in emacs
(add-to-list 'default-frame-alist '(undecorated . t))

;; go to nix config binding
(map! :leader
      (:prefix ("f" . "file")
       :desc "Open custom directory"
       "c" (λ! (find-file "~/nix-systems"))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;; ============================================================================
;;; org directory & files
;;; ============================================================================

(after! org
  ;; Set up org directory
  (setq org-directory "~/Documents/org")

  ;; Define your main org files for agenda
  (setq org-agenda-files '("~/Documents/org/inbox.org"
                           "~/Documents/org/calendar.org"
                           "~/Documents/org/projects.org"))

  ;; Archive location - completed tasks go to done.org
  (setq org-archive-location "~/Documents/org/done.org::* Archived Tasks")

  ;;; ============================================================================
  ;;; ORG BEHAVIOR & DEFAULTS
  ;;; ============================================================================

  ;; Add timestamp when task is marked DONE
  (setq org-log-done 'time)

  ;; Keep logs in a drawer to reduce clutter
  (setq org-log-into-drawer t)

  ;; Refile targets (for moving tasks between files)
  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 3)
          (nil :maxlevel . 3)))

  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;;; ============================================================================
  ;;; ORG CAPTURE TEMPLATES
  ;;; ============================================================================

  (setq org-capture-templates
        '(("i" "Inbox" entry
           (file "~/Documents/org/inbox.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)

          ("n" "Note" entry
           (file "~/Documents/org/notes.org")
           "* %? :note:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
           :empty-lines 1)

          ("c" "Calendar Event" entry
           (file "~/Documents/org/calendar.org")
           "* %?\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)

          ("d" "Deadline" entry
           (file "~/Documents/org/calendar.org")
           "* TODO %?\nDEADLINE: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)

          ("p" "Project Note" entry
           (file "~/Documents/org/projects.org")
           "* %? :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
           :empty-lines 1)

          ("l" "Link/Website" entry
           (file "~/Documents/org/notes.org")
           "* %? :link:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{URL}\n:END:\n%i"
           :empty-lines 1)

          ("v" "Video" entry
           (file "~/Documents/org/notes.org")
           "* %? :video:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{Video URL}\n:END:\n%i"
           :empty-lines 1)))

  ;;; ============================================================================
  ;;; ORG AGENDA CUSTOM VIEWS
  ;;; ============================================================================

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Inbox Tasks")
                   (org-agenda-files '("~/Documents/org/inbox.org"))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files '("~/Documents/org/projects.org"))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Calendar & Deadlines")
                   (org-agenda-files '("~/Documents/org/calendar.org"))))))

          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "TODO")
            (todo "WAITING")
            (todo "SOMEDAY")))

          ("i" "Inbox Review"
           ((todo "TODO"
                  ((org-agenda-files '("~/Documents/org/inbox.org"))
                   (org-agenda-overriding-header "Process Inbox")))))))

  ;;; ============================================================================
  ;;; CUSTOM FUNCTIONS
  ;;; ============================================================================

  ;; Function to archive all DONE tasks to done.org
  (defun my/archive-done-tasks ()
    "Archive all DONE tasks in current buffer to done.org"
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'file))

  ;; Optional: Auto-archive when marking task DONE (commented out by default)
  ;; Uncomment if you want automatic archiving
  ;; (defun my/auto-archive-done-task ()
  ;;   "Automatically archive task when marked DONE"
  ;;   (when (org-entry-is-done-p)
  ;;     (org-archive-subtree)))
  ;; (add-hook 'org-after-todo-state-change-hook 'my/auto-archive-done-task)

  ;;; ============================================================================
  ;;; KEYBINDINGS
  ;;; ============================================================================

  ;; Archive done tasks
  (map! :map org-mode-map
        :localleader
        :desc "Archive done tasks" "x d" #'my/archive-done-tasks)

  ;; Quick file access (using Doom's SPC o prefix for "open")
  (map! :leader
        :desc "Open inbox" "o i" (lambda () (interactive) (find-file "~/Documents/org/inbox.org"))
        :desc "Open calendar" "o c" (lambda () (interactive) (find-file "~/Documents/org/calendar.org"))
        :desc "Open notes" "o n" (lambda () (interactive) (find-file "~/Documents/org/notes.org"))
        :desc "Open projects" "o p" (lambda () (interactive) (find-file "~/Documents/org/projects.org"))
        :desc "Open done/archive" "o d" (lambda () (interactive) (find-file "~/Documents/org/done.org"))))

;;; ============================================================================
;;; ORG-ROAM CONFIGURATION
;;; ============================================================================

(after! org-roam
  ;; Set org-roam directory
  (setq org-roam-directory "~/Documents/org/roam")

  ;; Add org-roam to refile targets
  (setq org-refile-targets
        (append org-refile-targets
                '((org-roam-directory :maxlevel . 2))))

  ;; Function to convert a note from notes.org to org-roam
  (defun my/note-to-roam ()
    "Convert current org heading to an org-roam note"
    (interactive)
    (let* ((title (org-get-heading t t t t))
           (content (save-excursion
                      (org-end-of-meta-data t)
                      (buffer-substring-no-properties
                       (point)
                       (save-excursion
                         (org-end-of-subtree t)
                         (point))))))
      ;; Cut the current subtree
      (org-cut-subtree)
      ;; Create new roam note
      (org-roam-node-find nil title)
      ;; Insert the content
      (goto-char (point-max))
      (insert "\n" content)
      (save-buffer)
      (message "Converted '%s' to org-roam note" title)))

  ;; Keybinding for converting note to roam
  (map! :map org-mode-map
        :localleader
        :desc "Convert to org-roam note" "n r" #'my/note-to-roam))


;;; ============================================================================
;;; ORG DIRECTORY & FILES
;;; ============================================================================

(after! org
  ;; Set up org directory
  (setq org-directory "~/Documents/org")

  ;; Define your main org files for agenda
  (setq org-agenda-files '("~/Documents/org/inbox.org"
                           "~/Documents/org/calendar.org"
                           "~/Documents/org/projects.org"))

  ;; Archive location - completed tasks go to done.org
  (setq org-archive-location "~/Documents/org/done.org::* Archived Tasks")

  ;;; ============================================================================
  ;;; ORG BEHAVIOR & DEFAULTS
  ;;; ============================================================================

  ;; Add timestamp when task is marked DONE
  (setq org-log-done 'time)

  ;; Keep logs in a drawer to reduce clutter
  (setq org-log-into-drawer t)

  ;; Refile targets (for moving tasks between files)
  (setq org-refile-targets
        '((org-agenda-files :maxlevel . 3)
          (nil :maxlevel . 3)))

  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;;; ============================================================================
  ;;; ORG CAPTURE TEMPLATES
  ;;; ============================================================================

  (setq org-capture-templates
        '(("i" "Inbox" entry
           (file "~/Documents/org/inbox.org")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)

          ("n" "Note" entry
           (file "~/Documents/org/notes.org")
           "* %? :note:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
           :empty-lines 1)

          ("c" "Calendar Event" entry
           (file "~/Documents/org/calendar.org")
           "* %?\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)

          ("d" "Deadline" entry
           (file "~/Documents/org/calendar.org")
           "* TODO %?\nDEADLINE: %^T\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i"
           :empty-lines 1)

          ("p" "Project Note" entry
           (file "~/Documents/org/projects.org")
           "* %? :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
           :empty-lines 1)

          ("l" "Link/Website" entry
           (file "~/Documents/org/notes.org")
           "* %? :link:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{URL}\n:END:\n%i"
           :empty-lines 1)

          ("v" "Video" entry
           (file "~/Documents/org/notes.org")
           "* %? :video:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{Video URL}\n:END:\n%i"
           :empty-lines 1)))

  ;;; ============================================================================
  ;;; ORG AGENDA CUSTOM VIEWS
  ;;; ============================================================================

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-agenda-span 'day)
                        (org-deadline-warning-days 7)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Inbox Tasks")
                   (org-agenda-files '("~/Documents/org/inbox.org"))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-files '("~/Documents/org/projects.org"))))
            (todo "TODO"
                  ((org-agenda-overriding-header "Calendar & Deadlines")
                   (org-agenda-files '("~/Documents/org/calendar.org"))))))

          ("w" "Weekly Review"
           ((agenda "" ((org-agenda-span 'week)))
            (todo "TODO")
            (todo "WAITING")
            (todo "SOMEDAY")))

          ("i" "Inbox Review"
           ((todo "TODO"
                  ((org-agenda-files '("~/Documents/org/inbox.org"))
                   (org-agenda-overriding-header "Process Inbox")))))))

  ;;; ============================================================================
  ;;; CUSTOM FUNCTIONS
  ;;; ============================================================================

  ;; Function to archive all DONE tasks to done.org
  (defun my/archive-done-tasks ()
    "Archive all DONE tasks in current buffer to done.org"
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'file))

  ;; Optional: Auto-archive when marking task DONE (commented out by default)
  ;; Uncomment if you want automatic archiving
  ;; (defun my/auto-archive-done-task ()
  ;;   "Automatically archive task when marked DONE"
  ;;   (when (org-entry-is-done-p)
  ;;     (org-archive-subtree)))
  ;; (add-hook 'org-after-todo-state-change-hook 'my/auto-archive-done-task)

  ;;; ============================================================================
  ;;; KEYBINDINGS
  ;;; ============================================================================

  ;; Archive done tasks
  (map! :map org-mode-map
        :localleader
        :desc "Archive done tasks" "x d" #'my/archive-done-tasks)

  ;; Quick file access (using Doom's SPC o prefix for "open")
  (map! :leader
        :desc "Open inbox" "o i" (lambda () (interactive) (find-file "~/Documents/org/inbox.org"))
        :desc "Open calendar" "o c" (lambda () (interactive) (find-file "~/Documents/org/calendar.org"))
        :desc "Open notes" "o n" (lambda () (interactive) (find-file "~/Documents/org/notes.org"))
        :desc "Open projects" "o p" (lambda () (interactive) (find-file "~/Documents/org/projects.org"))
        :desc "Open done/archive" "o d" (lambda () (interactive) (find-file "~/Documents/org/done.org"))))

;;; ============================================================================
;;; ORG-ROAM CONFIGURATION
;;; ============================================================================

(after! org-roam
  ;; Set org-roam directory
  (setq org-roam-directory "~/Documents/org/roam")

  ;; Add org-roam to refile targets
  (setq org-refile-targets
        (append org-refile-targets
                '((org-roam-directory :maxlevel . 2))))

  ;; Function to convert a note from notes.org to org-roam
  (defun my/note-to-roam ()
    "Convert current org heading to an org-roam note"
    (interactive)
    (let* ((title (org-get-heading t t t t))
           (content (save-excursion
                      (org-end-of-meta-data t)
                      (buffer-substring-no-properties
                       (point)
                       (save-excursion
                         (org-end-of-subtree t)
                         (point))))))
      ;; Cut the current subtree
      (org-cut-subtree)
      ;; Create new roam note
      (org-roam-node-find nil title)
      ;; Insert the content
      (goto-char (point-max))
      (insert "\n" content)
      (save-buffer)
      (message "Converted '%s' to org-roam note" title)))

  ;; Keybinding for converting note to roam
  (map! :map org-mode-map
        :localleader
        :desc "Convert to org-roam note" "n r" #'my/note-to-roam))

;;; ============================================================================
;;; STYLE SETTINGS
;;; ============================================================================
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

;;; ============================================================================
;;; KEYBINDING REFERENCE
;;; ============================================================================

;; Doom Emacs already provides these by default:
;; SPC o a     - Open org-agenda
;; SPC o A     - Open org-agenda in other window
;; SPC X       - org-capture (or SPC n c in some Doom versions)
;; SPC m l l   - org-insert-link (in org-mode)
;; SPC m t     - org-todo (cycle TODO states)
;; SPC m s     - org-schedule
;; SPC m d     - org-deadline

;; Custom keybindings added above:
;; SPC o i     - Open inbox.org
;; SPC o c     - Open calendar.org
;; SPC o n     - Open notes.org
;; SPC o p     - Open projects.org
;; SPC o d     - Open done.org (archive)
;; SPC m x d   - Archive all DONE tasks (in org-mode buffer)
;; SPC m n r   - Convert current heading to org-roam note

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

;; Simple function that opens dirvish - script handles workspace switching
(defun my/focus-and-dirvish ()
  "Open dirvish in current Emacs session."
  (interactive)
  (dirvish))

(defun my/dirvish-here ()
  "Open dirvish in the directory of the current buffer."
  (interactive)
  (let ((dir (or (when (buffer-file-name)
                   (file-name-directory (buffer-file-name)))
                 default-directory)))
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

;; Simple function that opens elfeed - script handles workspace switching
(defun my/focus-and-elfeed ()
  "Open elfeed in current Emacs session."
  (interactive)
  (elfeed))

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
                        (format "wtype -s 350 \"%s\""
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype -s 350 \"%s\" && wtype -k Return"
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(defun thanos/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (erase-buffer)
    (org-mode)
    (setq-local header-line-format
                (format " %s to insert text or %s to cancel."
                        (propertize "C-c C-c" 'face 'help-key-binding)
			(propertize "C-c C-k" 'face 'help-key-binding)))
    (local-set-key (kbd "C-c C-k")
		   (lambda () (interactive)
		     (kill-new (buffer-string))
		     (delete-frame)))
    (local-set-key (kbd "C-c C-c")
		   (lambda () (interactive)
		     (start-process-shell-command
		      "wtype" nil
		      (thanos/wtype-text (buffer-string)))
		     (delete-frame)))))

(setq vterm-shell "/etc/profiles/per-user/marie/bin/zsh")
(setq vterm-environment '("TERM=xterm-256color"))
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(custom-set-faces!
  '(vterm :family "UbuntuMono Nerd Font"))

(setq confirm-kill-processes t)

;; Configure pyright
(after! lsp-pyright
  (setq lsp-pyright-multi-root nil))

(require 'multiple-cursors)

;; Basic multi-cursor commands
(global-set-key (kbd "C-c m l") 'mc/edit-lines)           ; cursor on each line
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)  ; mark next occurrence
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this) ; mark previous
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)   ; mark all occurrences

;; Rectangular region mode (super useful!)
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)

;; Skip current and mark next
(global-set-key (kbd "C-c m s") 'mc/skip-to-next-like-this)

;; Insert numbers at cursors (0, 1, 2...)
(global-set-key (kbd "C-c m i") 'mc/insert-numbers)

;; Sort regions by multi-cursor
(global-set-key (kbd "C-c m o") 'mc/sort-regions)
