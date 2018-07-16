; Load MELPA & ELPA package managers
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(require 'package)
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Load theme
;; (load-theme 'adwaita)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBE" :slant normal :weight normal :height 109 :width normal))))
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
 '(elfeed-search-title-face ((t (:foreground "black"))))
 '(hl-line ((t (:inherit nil :background "gainsboro"))))
 '(mode-line-buffer-id-inactive ((t (:inherit mode-line-buffer-id)))))

;; Set author name and email
(setq user-full-name "John C. Haprian"
user-mail-address "john@hcmllc.co")

;; Set default directory
(setq default-directory "/home/john/Dropbox/Notes")

;; Set default frame size
;; https://www.emacswiki.org/emacs/FrameSize
(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 170))

;; Tuck backups away
;; https://www.emacswiki.org/emacs/BackupDirectory
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Screw scratch
(setq initial-scratch-message nil)

;; Screw the welcome screen, too
(setq inhibit-startup-message t)

;; Margins look better
(setq left-margin-width 2)

;; Disable scroll bars
(scroll-bar-mode -1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

; Show time and date
(setq display-time-and-date t)

;; Disable the toolbar
(tool-bar-mode -1)

;; Automatically update buffers when files change
(global-auto-revert-mode t)

;; Show matching parenthesis
(show-paren-mode 1)

;; Wrap all the lines!
(global-visual-line-mode t)

;; Global undo tree goodness
;; https://www.emacswiki.org/emacs/UndoTree
(global-undo-tree-mode)

;; Winner Mode FTW
;; https://www.emacswiki.org/emacs/WinnerMode
(winner-mode 1)

;; Twitter in Emacs? Sure. Why the hell not.
;; https://github.com/hayamiz/twittering-mode
(setq twittering-icon-mode t)
(setq twittering-use-master-password t)
;; Save desktop configuration between sessions
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
(desktop-save-mode 1)

;; Sort apropos command search results based on relevancy
;; See "Mastering Emacs" book
(setq apropos-sort-by-scores t)

;; auto close bracket insertion. New in emacs 24
;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
;; http://prodissues.com/2016/10/electric-pair-mode-in-emacs.html
(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\` . ?\`)
                            (?\( . ?\))
                            (?\{ . ?\})
                            ))


;; Enable ace-window mode for fast window navigation
;; https://github.com/abo-abo/ace-window
;;
;; (global-set-key (kbd "C-x o") 'ace-window)
;; Alternative key binding
(global-set-key (kbd "M-o") 'ace-window)

;; org-protocol
;; https://github.com/sprig/org-capture-extension
;; Can't get the damn thing to work. Tired of yak shaving for now.
;; (server-start)
;; (add-to-list 'load-path "~/path/to/org/protocol/")
;; (require 'org-protocol)

;;
;; NOV
;;
;; Read epubs in emacs? Why the fuck not?
;; https://github.com/wasamasa/nov.el
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;
;; FLYSPELL
;; http://stackoverflow.com/questions/6860750/how-to-enable-flyspell-mode-in-emacs-for-all-files-and-all-major-modes
;; This does the squiggly line thingy
;; (add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; Pimped out Flyspell
;; https://www.reddit.com/r/emacs/comments/8rxm7h/tip_how_to_better_manage_your_spelling_mistakes/
(use-package flyspell
  :defer 1
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-mode 1))

(use-package flyspell-correct-ivy
  :after flyspell
  :bind (:map flyspell-mode-map
        ("C-;" . flyspell-correct-word-generic))
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))

;; Make ispell use the more capable Hunspell
;; https://joelkuiper.eu/spellcheck_emacs
(when (executable-find "hunspell")
(setq-default ispell-program-name "hunspell")
(setq ispell-really-hunspell t))

;; Bind ispell (aka hunspell) buffer checker to key-bindings
(global-set-key (kbd "<f10>") 'ispell-word)
(global-set-key (kbd "<f11>") 'ispell-region)
(global-set-key (kbd "<f12>") 'ispell-buffer)

;; Enable company mode for all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; Enable flx fuzzy search for company mode
(with-eval-after-load 'company
      (company-flx-mode +1))

;; yasnippet templating
;; https://github.com/joaotavora/yasnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;
;; IVY + SWIPER
;;
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; Hide formatting characters for bold, italics, etc.
;; https://www.reddit.com/r/emacs/comments/4bzj6a/how_to_get_orgmode_emphasis_markup_et_al_to/
(setq org-hide-emphasis-markers t)

;; Get rid of annoying prompt when saving multiple buffers
;; https://stackoverflow.com/questions/35658509/gnu-emacs-how-to-disable-prompt-to-save-modified-buffer-on-exit
(set-buffer-modified-p nil)

;; Better Buffer Naming
;; https://github.com/Compro-Prasad/simple-emacs/blob/master/init.el
uniquify-buffer-name-style 'forward

;; Increase kill ring max capacity
;; https://github.com/Compro-Prasad/simple-emacs/blob/master/init.el
kill-ring-max 1024

;; Scroll preserving screen position
;; https://github.com/Compro-Prasad/simple-emacs/blob/master/init.el
scroll-preserve-screen-position 'always

;; Scroll one line at a time
;; https://github.com/Compro-Prasad/simple-emacs/blob/master/init.el
scroll-step            1
scroll-conservatively  10000
mouse-wheel-scroll-amount '(1 ((shift) . 1))

;; Fixes some TLS connections
;; https://github.com/Compro-Prasad/simple-emacs/blob/master/init.el
;; gnutls-min-prime-bits 4096

;; Set UTF-8 as default encoding
;; https://github.com/Compro-Prasad/simple-emacs/blob/master/init.el
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-locale-environment "en.UTF-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;;
;; Initialize Deft
;; https://jblevins.org/projects/deft/
;; 
;;
(require 'deft)
(setq deft-extensions '("txt" "tex" "org"))
(setq deft-directory "~/Dropbox/Notes/Deft")
(setq deft-recursive t)

;; PDF-Tools bitches!
;; http://pragmaticemacs.com/emacs/more-pdf-tools-tweaks/
(use-package pdf-tools
 :config
 ;; initialise
 (pdf-tools-install)
 ;; open pdfs scaled to fit page
 (setq-default pdf-view-display-size 'fit-page)
 ;; automatically annotate highlights
 (setq pdf-annot-activate-created-annotations t)
 ;; use normal isearch
 (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

;;
;; ORG MODE
;;

;; Org-Agenda needs to be loaded before calling org-agenda works.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(require 'org-agenda)

;; Make org-web-tools functions available
;; https://github.com/alphapapa/org-web-tools
(require 'org-web-tools)

;; Something about eww & capturing URLs in org. Sounded interesting.
;; https://www.reddit.com/r/emacs/comments/8yb69h/capture_link_to_web_page_opened_in_ewww/
(use-package org-eww :after eww)

; Configure agenda view to search all org files
(setq org-agenda-files '("/home/john/Dropbox/Notes/"))

;; Append all archive files to org-agenda-files
;; Bastardized version of https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
(setq org-agenda-text-search-extra-files
      (file-expand-wildcards "~/Dropbox/Notes/*.org_archive"))

;; Wrap lines
(setq org-startup-indented t)

;; Don't allow editing of folded regions
;; https://aqeelakber.com/2016/12/21/emacs-org-mode-journal-and-log/
(setq org-catch-invisible-edits 'error)

;; For the love of all that is holy use current window for agenda
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-window-setup 'current-window)

; Configure Exporter options
(require 'ox-pandoc nil t)
(require 'ox-html5slide nil t)
(require 'ox-odt nil t)
(require 'ox-gfm nil t)

;;
;; ORG-BABEL
;; https://www.reddit.com/r/emacs/comments/2oy20n/babel_and_orgmode_for_devopslike_work/
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (shell . t)
   (python . t)
   (R . t)
   (ditaa . t)
   (dot . t)
   ))
;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

; Short key bindings for capturing notes/links and switching to agenda.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Add workflow change tracking to the drawer
(setq org-log-into-drawer t)

; Add task workflows
; http://orgmode.org/worg/org-tutorials/org4beginners.html
;; Also added TODO state change tracking
;; http://orgmode.org/manual/Tracking-TODO-state-changes.html
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-todo-keywords
  '((sequence "TODO(t)" "IN PROGRESS(i)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELLED(c!)")))

; Pretty colors
; http://doc.norang.ca/org-mode.html
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("IN PROGRESS" :foreground "purple" :weight bold)
              ("WAITING" :foreground "lightskyblue" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
	      ("CANCELLED" :foreground "orange" :weight bold))))

;; Enforce task dependencies
(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)

;; For the love of all that is holy use current window for agenda
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-window-setup 'current-window)

;; Show deadlines only on the day they are due in week view
;; http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
(setq org-deadline-warning-days 0)

;; Hide scheduled todos in the agenda if they also have a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Use AM/PM in agenda time grid
(setq org-agenda-timegrid-use-ampm t)

;; Show only current instance of repeating
;; http://orgmode.org/worg/org-faq.html
(setq org-agenda-repeating-timestamp-show-all nil)

;; sort tasks in order of when they are due and then by priority
;; http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))

;; Add workflow change tracking to the drawer
(setq org-log-into-drawer t)

;; Record task completion time
(setq org-log-done 'time)

; Short key bindings for capturing notes/links and switching to agenda.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; Configure capture mode
;; (setq org-default-notes-file (concat org-directory "/home/john/Dropbox/Notes/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)

;; org-protocol config failure *sad trombone*
;; (setq org-capture-templates `(
;; 	("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;         "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
;; 	("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
;;         "* %? [[%:link][%:description]] \nCaptured On: %U")
;; ))

;; Configure capture mode templates
(setq org-capture-templates
      '(("t" "Todo" entry (file "/home/john/Dropbox/Notes/inbox.org")
	 "* TODO %?")	
	 ("n" "Note" entry (file "/home/john/Dropbox/Notes/inbox.org")
	  "* %?\n\n%U")
	 ("j" "Journal" plain (file+olp+datetree "/home/john/Dropbox/Notes/journal.org")
	  "%U\n\n%?")))


;; Configure org-mode re-file
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 8))))

;; Make org-refile-targets work better w/ Counsel?
;; https://github.com/abo-abo/swiper/issues/444
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create new parent nodes
;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Automatically change list bullets
;; Makes it easier to read deeply nested lists
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("-" . "*")
					    ("*" . "+")
					    ("a)" . "a.")
					    ("a." . "a)")
                                            ("1)" . "1.")
                                            ("1." . "1)"))))

;; Allow alphabetical list entries
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-alphabetical-lists t)


;;
;; org-chef
;; https://github.com/Chobbes/org-chef
;;
(require 'use-package)

(use-package org-chef
  :ensure t)

;;
;; EWW Web Browser
;;

;; Set EWW as default web browser in emacs
(setq browse-url-browser-function 'eww-browse-url)

;; Open each page in a new buffer
;; http://emacs.stackexchange.com/questions/24472/simple-method-for-creating-multiple-eww-buffers
(defun eww-new ()
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
    (switch-to-buffer (generate-new-buffer "eww"))
    (eww-mode)
    (eww url)))

;;
;; ELFEED
;;

;; Set keyboard shortcut
(global-set-key (kbd "C-x w") 'elfeed)

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
;;    (defun bjm/elfeed-load-db-and-open ()
;;      "Wrapper to load the elfeed db from disk before opening"
;;      (interactive)
;;      (elfeed-db-load)
;;      (elfeed)
;;      (elfeed-search-update--force))

;;    ;;write to disk when quiting
;;    (defun bjm/elfeed-save-db-and-bury ()
;;      "Wrapper to save the elfeed db to disk before burying buffer"
;;      (interactive)
;;      (elfeed-db-save)
;;      (quit-window))

(setq elfeed-sort-order 'ascending)

;; Load elfeed-org
(require 'elfeed-org)

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files (list "/home/john/Dropbox/Notes/rss.org"))

;; Disable arrow keys to force practicing key nav
;; Yeah, I know. But it's for your own good.
;; https://superuser.com/questions/437953/disable-arrow-keys-in-emacs
;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (adwaita)))
 '(elfeed-goodies/entry-pane-position (quote bottom))
 '(elfeed-goodies/feed-source-column-width 40)
 '(elfeed-goodies/tag-column-width 0)
 '(flyspell-correct-interface (quote flyspell-correct-ivy) t)
 '(flyspell-issue-message-flag nil)
 '(flyspell-mode 1 t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/Notes/#inbox.org#" "~/Dropbox/Notes/SBIR.org_archive" "~/Dropbox/Notes/alpha.org_archive" "~/Dropbox/Notes/arisdyne.org_archive" "~/Dropbox/Notes/bluetip.org" "~/Dropbox/Notes/bluetip.org_archive" "~/Dropbox/Notes/chores.org" "~/Dropbox/Notes/chores.org_archive" "~/Dropbox/Notes/fha.org_archive" "~/Dropbox/Notes/hcm.org_archive" "~/Dropbox/Notes/inbox.org" "~/Dropbox/Notes/inbox.org_archive" "~/Dropbox/Notes/job_hunt.org_archive" "~/Dropbox/Notes/journal.org" "~/Dropbox/Notes/lbry.org_archive" "~/Dropbox/Notes/mtd.org_archive" "~/Dropbox/Notes/notes.org_archive" "~/Dropbox/Notes/personal.org" "~/Dropbox/Notes/personal.org_archive" "~/Dropbox/Notes/phonebail.org" "~/Dropbox/Notes/pmp.org" "~/Dropbox/Notes/programming.org" "~/Dropbox/Notes/projects.org_archive" "~/Dropbox/Notes/reference.org" "~/Dropbox/Notes/rss.org" "~/Dropbox/Notes/tasks.org_archive" "~/Dropbox/Notes/uwgc.org_archive" "~/Dropbox/Notes/wpl.org_archive")))
 '(package-selected-packages
   (quote
    (nov ox-minutes magit org-noter org-chef org-present opener use-package org-super-agenda ace-window company-flx elfeed-org htmlize zenburn-theme yasnippet xah-find xah-elisp-mode wn-mode w3m visual-regexp-steroids undo-tree twittering-mode sml-modeline sml-mode smex smart-mode-line popup parse-csv paredit pandoc-mode ox-reveal ox-pandoc ox-html5slide ox-gfm org-web-tools org-pdfview org-if org-grep org-download org-bullets olivetti multiple-cursors monokai-theme moe-theme markdown-mode+ json-mode ido-vertical-mode ido-ubiquitous ido-sort-mtime git-commit flx-ido eww-lnum ereader epresent deft darkokai-theme csv-mode counsel company browse-kill-ring badwolf-theme avy atom-one-dark-theme atom-dark-theme anzu)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
