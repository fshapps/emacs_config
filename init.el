;; STARTUP

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c4a784404a2a732ef86ee969ab94ec8b8033aee674cd20240b8addeba93e1612" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "1a53efc62256480d5632c057d9e726b2e64714d871e23e43816735e1b85c144c" "6df30cfb75df80e5808ac1557d5cc728746c8dbc9bc726de35b15180fa6e0ad9" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(display-time-mode t)
 '(font-use-system-font t)
 '(newsticker-url-list
   (quote
    (("Reddit" "https://www.reddit.com/.rss?feed=fde5995c8058d111d32b8c07f8c115acc9ab1445&user=LazarGrier" nil nil nil)
     ("Reddit Best Of" "https://www.reddit.com/r/bestof.rss" nil nil nil)
     ("VentureBeat" "http://venturebeat.com/feed/" nil nil nil))))
 '(org-tags-column -90)
 '(tool-bar-mode nil))

;; Load MELPA & ELPA package managers
(require 'package)
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Set theme
(load-theme 'monokai t)

;; Set default directory
(setq default-directory "/home/john/Dropbox/Notes")

;; Screw scratch
(setq initial-scratch-message nil)

;; Screw the welcome screen, too
(setq inhibit-startup-message t)

;; Fuck you ctrl-z
;; http://superuser.com/questions/349943/how-to-awake-emacs-gui-after-pressing-ctrlz
(global-unset-key (kbd "C-z"))

;; Highlight parens
;; https://www.emacswiki.org/emacs/ShowParenMode?_utm_source=1-2-2
(show-paren-mode 1)

;; auto close bracket insertion. New in emacs 24
;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(electric-pair-mode 1)

;; Disable scroll bars
(scroll-bar-mode -1)

;; Enable window numbering mode
;; https://github.com/luismbo/wn-mode
(wn-mode)

;; Set author name and email
(setq user-full-name "John C. Haprian"
user-mail-address "john@hcmllc.co")

;; Prevent emacs from clobbering orig file date
;; http://ergoemacs.org/emacs/emacs_make_modern.html
(setq backup-by-copying t)

;; Enable flyspell mode by default
;; http://stackoverflow.com/questions/6860750/how-to-enable-flyspell-mode-in-emacs-for-all-files-and-all-major-modes
;; This does the squiggly line thingy
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Make ispell use the more capable Hunspell
;; https://joelkuiper.eu/spellcheck_emacs
(when (executable-find "hunspell")
  (setq-default ispell-program-name "hunspell")
  (setq ispell-really-hunspell t))

;; Bind ispell (aka hunspell) buffer checker to key-bindings
(global-set-key (kbd "<f10>") 'ispell-word)
(global-set-key (kbd "<f11>") 'ispell-region)
(global-set-key (kbd "<f12>") 'ispell-buffer)

;; Bind insert inactive timestamp to C-c t
(global-set-key (kbd "C-c t") 'org-time-stamp-inactive)

;; Create line kill key binding because stupid Putty doesn't recognize ctrl-shift-del
;; http://stackoverflow.com/questions/3958528/how-can-i-delete-the-current-line-in-emacs
(global-set-key "\C-cd" 'kill-whole-line)

;; Enable company mode for all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; Turn on that sweet, sweet multiple cursor goodness
;; http://emacsrocks.com/e13.html
;; http://emacs.stackexchange.com/questions/212/is-there-a-way-to-use-query-replace-from-grep-ack-ag-output-modes/243#243
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; It can be difficult to remember the full names of Emacs commands, so I use icomplete-mode for minibuffer completion. This also makes it easier to discover commands.
(icomplete-mode 1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; Use smart mode line to clean up the modeline display a little.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(sml/setup)
(sml/apply-theme 'dark)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)

; Show time and date
(setq display-time-and-date t)

; I like to see the current time.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(setq display-time-12hr-format t)
(display-time-mode +1)

; Change cursor shape from box to bar
; hBttp://stackoverflow.com/questions/4191408/making-the-emacs-cursor-into-a-line
(setq-default cursor-type 'bar)

;; In every buffer, the line which contains the cursor will be fully
;; highlighted
(global-hl-line-mode 1)

;; Get me some of that sweet, sweet undo-tree action
(require 'undo-tree)
(global-undo-tree-mode)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Save all the histories
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Disable the toolbar
(tool-bar-mode -1)

;; Wrap lines intelligently
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Use smex to complete interactive commands
(require 'smex)
(smex-initialize)

;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Automatically update buffers when files change
(global-auto-revert-mode t)

; Make text mode the default for new files
(setq initial-major-mode 'text-mode)

; Load abbreviations aka TextExpander for Emacs
; http://www.gnu.org/software/emacs/manual/html_node/emacs/Abbrevs.html
(setq-default abbrev-mode t)
(read-abbrev-file "~/.emacs.d/.abbrev_defs")
(setq save-abbrevs t)

; anzu.el is an Emacs port of anzu.vim. anzu.el provides a minor mode which displays current match and total matches information in the mode-line in various search modes.
; https://github.com/syohex/emacs-anzu
(global-anzu-mode +1)

;;
;; ORG MODE
;;

; Org-Agenda needs to be loaded before calling org-agenda works.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(require 'org-agenda)

; Configure Exporter options
(require 'ox-pandoc nil t)

;; Make clean view for lists the default
;; http://orgmode.org/manual/Clean-view.html
(setq org-startup-indented t)

;; Set org indent mode. Make things look better? I guess?
;; I belive this is what hides leading stars from view/
(setq org-indent-mode t)

;; Dunno. Make things look nicer.
(setq auto-fill-mode -1)

; Configure agenda view to search all org files
(setq org-agenda-files '("/home/john/Dropbox/Notes"))

; Short key bindings for capturing notes/links and switching to agenda.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; Set Org Mode to syntax highlight code blocks
; http://www.star.bris.ac.uk/bjm/org-basics.html
(setq org-src-fontify-natively t)

;; Add workflow change tracking to the drawer
(setq org-log-into-drawer t)

; Add task workflows
; http://orgmode.org/worg/org-tutorials/org4beginners.html
;; Also added TODO state change tracking
;; http://orgmode.org/manual/Tracking-TODO-state-changes.html
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-todo-keywords
  '((sequence "TODO(t!)" "IN PROGRESS(i!)" "WAITING(w!)" "SOMEDAY(s!)" "|" "DONE(d!)" "CANCELLED(c!)")))

; Pretty colors
; http://doc.norang.ca/org-mode.html
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("IN PROGRESS" :foreground "yellow" :weight bold)
              ("WAITING" :foreground "lightskyblue" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
	      ("CANCELLED" :foreground "orange" :weight bold))))

;; Enforce task dependencies
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)

;; Make time tracking persistent
;; http://orgmode.org/manual/Clocking-work-time.html
(setq org-clock-persist 'history)
     (org-clock-persistence-insinuate)

;; Use hours instead of days for clocktime
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Don't show blocked tasks in agenda view
(setq org-agenda-dim-blocked-tasks 'invisible)

;; Don't show completed scheduled or deadline tasks if done
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

;; Don't show scheduled items in agenda view
(setq org-agenda-todo-ignore-scheduled 'all)
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Show deadlines only on the day they are due in week view
;; http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
(setq org-deadline-warning-days 0)

;; Hide scheduled todos in the agenda if they also have a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Show only current instance of repeating
;; http://orgmode.org/worg/org-faq.html
(setq org-agenda-repeating-timestamp-show-all nil)

;; Hide leading stars; only draw the right-most star in a heading.
;; http://doc.rix.si/cce/cce-org.html
(setq org-hide-leading-stars nil)

;; Record task completion time
(setq org-log-done 'time)

;; Causes Org to insert annotation when task deadline changed
;; http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-log-redeadline (quote time))

;; Causes Org to insert annotation when scheduled task date changes
;; http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-log-reschedule (quote time))

;; Hide formatting characters for bold, italics, etc.
;; https://www.reddit.com/r/emacs/comments/4bzj6a/how_to_get_orgmode_emphasis_markup_et_al_to/
(setq org-hide-emphasis-markers t)

;; Sorting order for tasks on the agenda
;; http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/ 
(setq org-agenda-sorting-strategy
(quote
((agenda priority-down deadline-down timestamp-down scheduled-down category-keep tag-up))))

;; Start the weekly agenda on Monday
;; ;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-start-on-weekday 1)

; Configure capture mode
;; (setq org-default-notes-file (concat org-directory "/home/john/Dropbox/Notes/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Configure capture mode templates
(setq org-capture-templates
       '(("t" "Todo" entry (file+headline "/home/john/Dropbox/Notes/inbox.org" "Tasks")
             "* TODO %?\n  %i\n)"
        ("i" "Idea" entry (file+headline "/home/john/Dropbox/Notes/inbox.org" "Ideas")
         "* %?\nAdded: %U\n")
        ("j" "Journal Entry" item (file+datetree "/home/john/Dropbox/Notes/journal.org")
             "* %?\nEntered on %U\n  %i\n")))

;; Enable IDO
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)

;; Use IDO for everything
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Enable IDO vertical mode
(require 'ido-vertical-mode)
(ido-mode (ido-vertical-mode 1))

;; Configure org-mode re-file
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 4))))

;; Make org-mode refiling work properly w/ IDO
(setq org-completion-use-ido t)
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)


;; Automatically change list bullets
;; Makes it easier to read deeply nested lists
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "-")
                                            ("A)" . "-")
                                            ("B)" . "-")
                                            ("a)" . "-")
                                            ("b)" . "-")
                                            ("A." . "-")
                                            ("B." . "-")
                                            ("a." . "-")
                                            ("b." . "-"))))

;; Allow alphabetical list entries
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-alphabetical-lists t)

;; Include archive files in org-mode search
;; http://doc.norang.ca/org-mode.html#SearchesIncludeArchiveFiles
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;; For the love of all that is holy use current window for agenda
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-window-setup 'current-window)

;; Display tags farther right
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-tags-column -98)

;; Remove indentation on agenda tags view
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-tags-match-list-sublevels t)

;; Enable persistent agenda filters
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-persistent-filter t)

;; Make bullets purty
;; https://github.com/sabof/org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Set UTF-8 as default encoding
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
