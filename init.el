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
(load-theme 'adwaita)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBE" :slant normal :weight normal :height 109 :width normal))))
 '(custom-enabled-themes (quote (smart-mode-line-dark)))
 '(hl-line ((t (:inherit nil :background "gainsboro")))))

;; Set author name and email
(setq user-full-name "John C. Haprian"
user-mail-address "john@hcmllc.co")

;; Set default directory
(setq default-directory "/home/john/Dropbox/Notes")

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

;; Enable company mode for all buffers
(add-hook 'after-init-hook 'global-company-mode)

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

;; Set UTF-8 as default encoding
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;
;; ORG MODE
;;

;; Org-Agenda needs to be loaded before calling org-agenda works.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(require 'org-agenda)

; Configure agenda view to search all org files
(setq org-agenda-files '("/home/john/Dropbox/Notes"))

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
(require 'ox-html5slide)
(require 'ox-odt)

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

;; Configure capture mode templates
(setq org-capture-templates
      '(("t" "Todo" entry (file "/home/john/Dropbox/Notes/inbox.org")
	 "* TODO %?")
	 ("n" "Note" entry (file "/home/john/Dropbox/Notes/inbox.org")
	  "* %?\n\n%U")
	 ("j" "Journal" plain (file+datetree "/home/john/Dropbox/Notes/journal.org")
	  "%U\n\n%?")))

;; Configure org-mode re-file
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 8))))

;; Make org-refile-targets work better w/ Counsel?
;; https://github.com/abo-abo/swiper/issues/444
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

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

;; Include archive files in org-mode search
;; http://doc.norang.ca/org-mode.html#SearchesIncludeArchiveFiles
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))

;;
;; EWW Web Browser
;;

;; Set EWW as default web browser in emacs
;; (setq browse-url-browser-function 'eww-browse-url)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (elfeed-org htmlize zenburn-theme yasnippet xah-find xah-elisp-mode wn-mode w3m visual-regexp-steroids undo-tree twittering-mode sml-modeline sml-mode smex smart-mode-line popup parse-csv paredit pandoc-mode ox-reveal ox-pandoc ox-html5slide ox-gfm org-web-tools org-pdfview org-if org-grep org-download org-bullets olivetti multiple-cursors monokai-theme moe-theme markdown-mode+ json-mode ido-vertical-mode ido-ubiquitous ido-sort-mtime git-commit flx-ido eww-lnum ereader epresent elfeed-goodies deft darkokai-theme csv-mode counsel company browse-kill-ring badwolf-theme avy atom-one-dark-theme atom-dark-theme anzu))))
