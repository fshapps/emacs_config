; STARTUP

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBE" :slant normal :weight normal :height 109 :width normal))))
 '(custom-enabled-themes (quote (smart-mode-line-dark))))

;; Load MELPA & ELPA package managers
(require 'package)
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Set default directory
(setq default-directory "/home/john/Dropbox/Notes")

;; Screw scratch
(setq initial-scratch-message nil)

;; Screw the welcome screen, too
(setq inhibit-startup-message t)

;; Fuck you ctrl-z
;; http://superuser.com/questions/349943/how-to-awake-emacs-gui-after-pressing-ctrlz
(global-unset-key (kbd "C-z"))

;; Stop whining about deleting backups
;; https://nurikabe.blog/2008/04/11/prevent-emacs-from-whining-about-excess-backup-versions/
(setq delete-old-versions t)

;; Highlight parens
;; https://www.emacswiki.org/emacs/ShowParenMode?_utm_source=1-2-2
(show-paren-mode 1)

;; auto close bracket insertion. New in emacs 24
;; http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
;; http://prodissues.com/2016/10/electric-pair-mode-in-emacs.html
(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\` . ?\`)
                            (?\( . ?\))
                            (?\{ . ?\})
                            ) )

;; Disable scroll bars
(scroll-bar-mode -1)

;; set up emacs so that each file opens in a new window
;; http://ergoemacs.org/emacs/emacs_make_modern.html
(setq pop-up-frames t)

;; Configure capture mode to open a dedicated window
;; https://stackoverflow.com/questions/15253005/in-emacs-org-mode-how-do-i-get-org-capture-to-open-in-a-full-sized-window...hmmm. Didn't work.
;; (add-hook 'org-capture-mode-hook 'sticky-window-delete-other-windows)

;; Enable window numbering mode
;; https://github.com/luismbo/wn-mode
(wn-mode)

;; Set default frame size
;; http://ergoemacs.org/emacs/emacs_customize_default_window_size.html
;; initial window
(setq initial-frame-alist
      '(
        (width . 60) ; character
        (height . 50) ; lines
        ))

;; Set author name and email
(setq user-full-name "John C. Haprian"
user-mail-address "john@hcmllc.co")

;; Restore Desktop
;; (desktop-save-mode 1)

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

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; It can be difficult to remember the full names of Emacs commands, so I use icomplete-mode for minibuffer completion. This also makes it easier to discover commands.
(icomplete-mode 1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Keep multiple backups of edited files. Poor man version control.
;; https://aqeelakber.com/2016/12/21/emacs-org-mode-journal-and-log/
(setq version-control t        ;; OpenVMS-esque
      backup-by-copying t      ;; Copy-on-write-esque
      kept-new-versions 64     ;; Indeliable-ink-esque
      kept-old-versions 0      ;; 
      delete-old-versions nil  ;; 
      )
(setq backup-directory-alist   ;; Save backups in $(pwd)/.bak
      '(("." . ".bak"))        ;;
      )

;; Don't allow editing of folded regions
;; https://aqeelakber.com/2016/12/21/emacs-org-mode-journal-and-log/
(setq org-catch-invisible-edits 'error)

;; Remove the pesky menu bar
;; (menu-bar-mode -1)

; Use smart mode line to clean up the modeline display a little.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'dark)
(setq sml/shorten-directory t)
(setq sml/shorten-modes t)

; Show time and date
(setq display-time-and-date t)

; Change cursor shape from box to bar
; hBttp://stackoverflow.com/questions/4191408/making-the-emacs-cursor-into-a-line
(setq-default cursor-type 'bar)

;; Set default cursor color
;; http://stackoverflow.com/questions/8204316/cant-change-cursor-color-in-emacsclient
(setq default-frame-alist '((cursor-color . "black")))

;; In every buffer, the line which contains the cursor will be fully
;; highlighted
;; (global-hl-line-mode 1)

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

; anzu.el is an Emacs port of anzu.vim. anzu.el provides a minor mode which displays current match and total matches information in the mode-line in various search modes.
; https://github.com/syohex/emacs-anzu
(global-anzu-mode +1)

;; yasnippet templating
;; https://github.com/joaotavora/yasnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;;
;; IFTTT
;;

;; Tells Emacs to treat any .txt file opened up in my Dropbox “org” folder in org-mode
;; http://mph.puddingbowl.org/2012/08/ifttt-dropbox-and-the-panopticon/
(add-to-list 'auto-mode-alist '("\\Dropbox/Notes/.*\.txt\\'" . org-mode))

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
;; ORG MODE
;;

;; Needed for MobileOrg?
;; https://stackoverflow.com/questions/11822353/how-to-make-org-mobile-work-in-android
(require 'org)
(setq org-directory "~/Dropbox/Notes")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/Notes/from-mobile.org")

;; Org-Agenda needs to be loaded before calling org-agenda works.
; http://zeekat.nl/articles/making-emacs-work-for-me.html#sec-10-6
(require 'org-agenda)

; Configure Exporter options
(require 'ox-pandoc nil t)
(require 'ox-html5slide)

;; Configure exporting as ODT
;; https://lists.gnu.org/archive/html/emacs-orgmode/2014-01/msg00599.html
(require 'ox-odt)

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

;; Make navigating more efficient in org-mode
;; http://emacs.stackexchange.com/questions/17502/how-to-navigate-most-efficiently-to-the-start-or-end-of-the-main-text-of-an-org/17513#17513
(setq org-special-ctrl-a/e t)

; Set Org Mode to syntax highlight code blocks
; http://www.star.bris.ac.uk/bjm/org-basics.html
(setq org-src-fontify-natively t)

;; Add workflow change tracking to the drawer
(setq org-log-into-drawer t)

;; Get rid of those damn blank lines
;; http://orgmode.org/worg/org-faq.html
(setq org-blank-before-new-entry nil)

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

; Configure capture mode
;; (setq org-default-notes-file (concat org-directory "/home/john/Dropbox/Notes/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)

;; Configure capture mode templates
(setq org-capture-templates
       '(("t" "Todo" entry (file "/home/john/Dropbox/Notes/inbox.org")
	  "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:")
        ("n" "Note" entry (file "/home/john/Dropbox/Notes/inbox.org")
             "* %?
:PROPERTIES:
:CREATED: %U
:END:")
        ("j" "Journal" plain (file+datetree "/home/john/Dropbox/Notes/journal.org")
	 "%?

%U"
:empty-lines 1)))

;; IVY Search
;; https://github.com/abo-abo/swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)

;; Enable IDO
;; (require 'ido)
;; (ido-mode 1)
;; (ido-everywhere 1)

;; Use IDO for everything
;; (require 'ido-ubiquitous)
;; (ido-ubiquitous-mode 1)

;; Enable IDO vertical mode
;; (require 'ido-vertical-mode)
;; (ido-mode (ido-vertical-mode 1))

;; Sort IDO results my modification time
;; https://github.com/pkkm/ido-sort-mtime
;; (ido-sort-mtime-mode 1)

;; Make org-mode refiling work properly w/ IDO
;; Allow refile to top level of files
(setq org-refile-use-outline-path 'file)
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)
(setq org-reverse-note-order t)

;; Configure org-mode re-file
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 8))))

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

;; For the love of all that is holy use current window for agenda
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-window-setup 'current-window)

;; Display tags farther right
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-tags-column -105)

;; Remove indentation on agenda tags view
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-tags-match-list-sublevels t)

;; Enable persistent agenda filters
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-agenda-persistent-filter t)

;; MobileOrg Setup
;; http://orgmode.org/manual/Setting-up-the-staging-area.html#Setting-up-the-staging-area
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;;
;; ELFEED
;;

;; Set keyboard shortcut
(global-set-key (kbd "C-x w") 'elfeed)

;; Set default directory for DB
(setq elfeed-db-directory "~/Dropbox/Apps/Elfeed")

;;functions to support syncing .elfeed between machines
;;makes sure elfeed reads index from disk before launching
    (defun bjm/elfeed-load-db-and-open ()
      "Wrapper to load the elfeed db from disk before opening"
      (interactive)
      (elfeed-db-load)
      (elfeed)
      (elfeed-search-update--force))

    ;;write to disk when quiting
    (defun bjm/elfeed-save-db-and-bury ()
      "Wrapper to save the elfeed db to disk before burying buffer"
      (interactive)
      (elfeed-db-save)
      (quit-window))

;; Set UTF-8 as default encoding
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (adwaita)))
 '(custom-safe-themes
   (quote
    ("a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "604648621aebec024d47c352b8e3411e63bdb384367c3dd2e8db39df81b475f5" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(display-battery-mode t)
 '(elfeed-feeds
   (quote
    ("https://scienceofsharp.wordpress.com/feed/" "http://bulletjournal.com/feed/" "http://www.threestaples.com/blog?format=RSS" "http://www.wellappointeddesk.com/feed/" "http://www.penaddict.com/blog?format=RSS" "http://wvs.topleftpixel.com/index.rdf" "http://jakubsan.tumblr.com/rss" "http://alanfriedman.tumblr.com/rss" "http://bookshelfporn.com/rss" "http://www.flickr.com/groups_feed.gne?id=35468150438@N01&format=rss_200" "http://clevelandprintroom.com/feed/" "http://www.flickr.com/recent_comments_feed.gne?id=94043197@N00&format=rss_200" "http://api.flickr.com/services/feeds/groups_pool.gne?id=46284008@N00&format=rss_200" "http://gigapan.blogspot.com/feeds/posts/default" "http://www.marcsijan.com/feed/" "http://feeds2.feedburner.com/marcofolio" "http://www.flickr.com/services/feeds/photos_friends.gne?user_id=94043197@N00&format=atom_03&friends=0&display_all=1" "http://api.flickr.com/services/feeds/photos_public.gne?id=94431609@N00&lang=en-us&format=rss_200" "http://api.flickr.com/services/feeds/photos_public.gne?id=58607835@N00&format=atom" "http://fantasygoat.livejournal.com/data/rss" "http://prostheticknowledge.tumblr.com/rss" "http://riotclitshave.livejournal.com/data/atom" "http://riotclitshave.tumblr.com/rss" "http://scifispaceships.tumblr.com/rss" "http://api.flickr.com/services/feeds/groups_pool.gne?id=88514644@N00&format=rss_200" "http://livelymorgue.tumblr.com/rss" "http://transformerstation.org/News/files/feed.xml" "http://api.flickr.com/services/feeds/photos_public.gne?id=48889065425@N01&lang=en-us&format=rss_200" "http://feeds.feedburner.com/avc" "http://feeds.feedburner.com/advancedriskology" "http://feeds.feedburner.com/InstigatorBlog" "http://startuplessonslearned.blogspot.com/feeds/posts/default" "http://sethgodin.typepad.com/seths_blog/atom.xml" "http://www.skmurphy.com/feed/" "http://stratechery.com/feed/" "http://transitionculture.org/feed/" "http://feeds.feedburner.com/ChefSteps" "http://akroncanton.craigslist.org/search/sso?catAbb=sso&excats=97-21-63-6&query=laminate%20flooring&sort=date&format=rss" "http://feeds.feedburner.com/JCCharts" "http://flowingdata.com/feed" "http://feeds.feedburner.com/FlowingData" "http://feeds.feedburner.com/InformationIsBeautiful" "https://www.rstudio.com/feed/" "http://amritrichmond.tumblr.com/rss" "http://feeds.feedburner.com/design-milk" "https://www.format.com/magazine/feed" "http://uxmag.com/rss.xml" "http://3quarksdaily.blogs.com/3quarksdaily/atom.xml" "http://feeds.feedburner.com/alansjourney" "http://www.badassoftheweek.com/rss.xml" "http://boingboing.net/rss.xml" "http://www.dailydot.com/feed/summary/latest/" "http://feeds.feedburner.com/TheThrillingWonderStory" "http://decodedpast.com/feed" "http://www.reddit.com/r/DepthHub/.rss" "http://www.farnamstreetblog.com/feed/atom/" "http://www.kungfugrippe.com/rss" "http://lesswrong.com/.rss" "http://xml.metafilter.com/atom.xml" "http://feeds.feedburner.com/OpenCulture" "http://polyrad.info/feed/" "http://www.quotationspage.com/data/qotd.rss" "http://sciencehorrors.tumblr.com/rss" "http://dilbert.com/blog/entry.feed/" "http://sthelenaonline.org/feed/" "http://stackexchangenocontext.tumblr.com/rss" "http://technabob.com/blog/feed/" "http://thechirurgeonsapprentice.com/feed/" "http://www.thisiswhyimbroke.com/feed" "http://thoughtinfection.com/feed/" "http://feeds.feedburner.com/uncrate" "http://www.waitbutwhy.com/feed" "http://zompist.wordpress.com/feed/" "http://www.jamesaltucher.com/feed/" "http://physics.ucsd.edu/do-the-math/feed/" "http://fridayinvegas.blogspot.com/feeds/posts/default?alt=rss" "http://feeds.feedburner.com/LazyManAndMoney" "http://feeds.feedburner.com/MrMoneyMustache" "http://www.nakedcapitalism.com/feed" "http://blog.modelworks.ch/?feed=rss2" "http://coolmaterial.com/feed/" "http://everyday-carry.com/rss" "http://putthison.com/rss" "http://www.chefsteps.com/feed" "http://feeds.feedburner.com/smittenkitchen" "http://feeds.feedburner.com/blogspot/summertomato" "http://thatsnerdalicious.com/feed/" "http://www.therailburger.com/index?format=RSS" "http://positech.co.uk/cliffsblog/?feed=rss2" "http://massively.joystiq.com/tag/EVE-Evolved/rss.xml" "http://www.ed.gov/feed" "http://blog.seliger.com/feed/" "http://www.adaringadventure.com/feed/" "http://feeds.feedburner.com/bakadesuyo" "http://feeds.feedburner.com/theminimalists/Hztx" "http://www.tinyhousetalk.com/feed/" "http://feeds.feedburner.com/zenhabits" "http://www.reddit.com/r/ZenHabits/.rss" "http://www.crainscleveland.com/feed/breaking_news" "http://blog.fitbit.com/?feed=rss2" "http://trekohio.com/feed/" "http://awkwardfamilyphotos.com/?feed=rss2" "http://www.craigslist.org/about/best/all/index.rss" "http://fuckinghomepage.com/rss" "http://garfieldminusgarfield.net/rss" "http://godzillahaiku.tumblr.com/rss" "http://obviousplant.tumblr.com/rss" "http://feedproxy.google.com/passiveaggressivenotes" "http://survivetheapocalypse.wordpress.com/feed/" "http://xkcd.com/rss.xml" "http://blog.erratasec.com/feeds/posts/default" "http://infosystir.blogspot.com/feeds/posts/default" "http://krebsonsecurity.com/feed/" "http://feeds.feedburner.com/BrazenCareerist" "https://www.jumpstartinc.org/feed/" "http://feed43.com/1240263800287635.xml" "http://ratracerebellion.com/feed/" "http://management.curiouscatblog.net/feed/" "http://feeds.feedburner.com/LeanBlog" "http://feeds2.feedburner.com/Command-line-fu" "http://www.playterm.org/data/cache/rss-latest.xml" "http://endlessparentheses.com/atom.xml" "http://www.linuxjournal.com/node/feed" "http://www.cyberciti.biz/feed/" "http://en.community.dell.com/techcenter/os-applications/rss" "http://clevelandmagazine.blogspot.com/feeds/posts/default" "http://www.clevescene.com/cleveland/Rss.xml" "http://chetramey.blogspot.com/feeds/posts/default" "http://rustbeltchic.com/feed/" "http://www.thepostnewspapers.com/search/?f=rss&t=article&c=wadsworth&l=50&s=start_time&sd=desc" "http://feeds.feedburner.com/FindingFreeEbooks" "http://longform.org/feed" "http://longreads.com/rss" "http://www.warisboring.com/feed/" "http://feeds.feedburner.com/TheBoyGeniusReport" "http://feeds.feedburner.com/OfficialGoogleMobileBlog" "http://feeds.phonedog.com/phonedog_cellphonereviews" "http://www.quietearth.us/rss.xml" "http://www.patternbased.com/feed/" "http://pitchfork.com/rss/thepitch/" "http://www.foxnews.com/about/rss/feedburner/foxnews/special-report" "http://townhall.com/xml/columnists/author/johnstossel/" "http://mikerowe.com/feed/" "http://www.reason.com/news/index.xml" "http://news.google.com/news?cf=all&hl=en&pz=1&ned=us&topic=h&num=3&output=rss" "http://www.inknouveau.com/feeds/posts/default" "http://feeds2.feedburner.com/advancedlifeskills/MClm" "http://feeds.feedburner.com/DumbLittleMan" "http://gandenberger.org/feed/" "http://feeds.feedburner.com/pickthebrain/LYVv" "http://feeds.feedburner.com/PracticallyEfficient" "http://feeds2.feedburner.com/raptitudecom" "http://feeds.feedburner.com/rudiusmedia/rch" "http://www.stevepavlina.com/blog/feed" "http://www.steve-olson.com/feed/" "http://calnewport.com/blog/feed/" "http://chrisguillebeau.com/3x5/feed/" "http://feeds.feedburner.com/ALifeCoachsBlog" "http://feeds.feedburner.com/TimelessInformation" "http://feeds.feedburner.com/American" "http://maplight.org/rss.xml" "http://talkabout.makelovenotporn.tv/rss" "http://proxypaige.tumblr.com/rss" "http://rockitreports.com/feed/" "http://scoptophilia.blogspot.com/feeds/posts/default?alt=rss" "http://theshapeofamother.com/feed/" "http://vintagepulchritude.blogspot.com/feeds/posts/default" "http://feeds2.feedburner.com/VioletBlueOpenSourceSex" "http://www.productbeautiful.com/feed/" "http://www.svpg.com/articles/rss" "http://feeds.feedburner.com/ItProductManagement" "http://feeds.feedburner.com/The99Percent" "http://donebeforebrekky.com/feed/" "http://drandus.wordpress.com/feed/" "http://blog.gtdnext.com/feed/" "http://emacs.stackexchange.com/feeds/tag?tagnames=org-mode&sort=votes" "http://feeds.feedburner.com/OutlinersoftwareForum" "http://productivity.stackexchange.com/feeds" "http://feeds2.feedburner.com/Smarterware" "http://feeds.feedburner.com/StudyHacks" "http://takingnotenow.blogspot.com/feeds/posts/default" "http://fourhourworkweek.com/blog/feed/" "http://feeds.feedburner.com/unclutterer" "http://edward.oconnor.cx/feed" "http://googleappsdeveloper.blogspot.com/feeds/posts/default" "http://stackoverflow.com/feeds/tag?tagnames=elisp&sort=newest" "http://feeds.feedburner.com/ConnectingTechnologyStrategyAndExecution" "http://www.betterprojects.net/feeds/posts/default" "http://feeds.feedburner.com/wordpress/Kyvt" "http://feeds.feedburner.com/typepad/HerdingCats" "http://rogueprojectleader.blogspot.com/feeds/posts/default?alt=rss" "http://www.scottberkun.com/feed/" "http://feeds.feedburner.com/pmsolutions" "http://pmstudent.com/feed/atom" "http://bps-research-digest.blogspot.com/feeds/posts/default?alt=rss" "http://www.fxckfeelings.com/feed/" "http://www.mindhacks.com/atom.xml" "http://youarenotsosmart.com/feed/" "http://quiterss.org/en/rss.xml" "http://www.marriedmansexlife.com/feeds/posts/default?alt=rss" "http://www.overcomingbias.com/feed" "http://www.quantamagazine.org/feed/" "http://slatestarcodex.com/feed/" "http://www.lastwordonnothing.com/feed/" "http://violentmetaphors.com/feed/" "http://what-if.xkcd.com/feed.atom" "http://www.lovesciencemedia.com/love-science-media/rss.xml" "http://mosex.wordpress.com/feed/" "http://pervocracy.blogspot.com/feeds/posts/default?alt=rss" "http://nextdraft.com/archives/feed/" "http://feeds.arstechnica.com/arstechnica/features/" "https://blog.getremarkable.com/feed" "http://blog.lmorchard.com/index.rss" "http://boughtitonce.com/feed/" "http://feeds.feedburner.com/BrettTerpstra" "http://sudophilosophical.com/feed/" "http://continuations.com/rss" "http://www.coolthings.com/feed/" "http://mcfunley.com/feed/atom" "http://www.devalot.com/feeds/all.rss" "http://googledocs.blogspot.com/atom.xml" "http://emacsredux.com/atom.xml" "http://gmailblog.blogspot.com/atom.xml" "http://chrome.blogspot.com/atom.xml" "http://feeds.feedburner.com/GoogleOperatingSystem" "https://hacked.com/feed/" "http://hnrss.org/newest?points=100" "https://medium.com/feed/hacker-daily" "http://feeds.feedburner.com/HighScalability" "http://ben-evans.com/benedictevans?format=rss" "http://www.howardism.org/index.xml" "http://feeds.feedburner.com/IeeeSpectrum" "http://www.joelonsoftware.com/rss.xml" "http://www.reddit.com/r/emacs/.rss" "http://www.mattcutts.com/blog/feed/" "http://www.nostarch.com/feeds/newbooks.xml" "http://feeds.feedburner.com/oreilly/radar/atom" "http://onethingwell.org/rss" "http://planet.emacsen.org/atom.xml" "http://pragmaticemacs.com/feed/" "http://prodissues.com/feed" "http://www.producthunt.com/feed" "http://sachachua.com/blog/feed" "http://shutupandtakemymoney.com/?feed=rss2" "http://rss.slashdot.org/slashdot/eqWf" "http://www.stilldrinking.org/rss/feed.xml" "http://syndication.thedailywtf.com/TheDailyWtf" "http://googleblog.blogspot.com/atom.xml" "http://feeds.feedburner.com/GoogleAppsUpdates" "http://toolsandtoys.net/feed/" "http://blog.trello.com/feed/" "http://favoriteandforget.com/rss" "http://feeds.wired.com/wired/index" "http://www.atlasobscura.com/feeds/places" "https://travelingwithcysticfibrosis.wordpress.com/feed/" "http://lj.libraryjournal.com/feed/" "http://www.librarytechnology.org/rss/" "http://www.thewhoresofyore.com/14/feed" "https://theconversation.com/us/feeds" "http://www.kurzweilai.net/news/feed/atom" "https://www.bloomberg.com/view/rss/topics/money-stuff.rss" "http://www.techmeme.com/feed.xml" "http://www.tedunangst.com/inks/rss" "https://lobste.rs/rss" "http://wavefunction.fieldofscience.com/feeds/posts/default")))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(org-clock-into-drawer "CLOCK")
 '(org-log-done (quote time))
 '(org-log-into-drawer "NOTES")
 '(org-log-redeadline (quote time))
 '(org-log-refile (quote time))
 '(org-log-reschedule (quote time))
 '(package-selected-packages
   (quote
    (pandoc-mode markdown-mode+ markdown-mode swiper ivy zenburn-theme yasnippet xah-find xah-elisp-mode wn-mode w3m visual-regexp-steroids undo-tree twittering-mode sml-modeline sml-mode smex smart-mode-line popup parse-csv paredit ox-reveal ox-pandoc ox-html5slide org-pdfview org-if org-grep org-download org-bullets olivetti multiple-cursors monokai-theme moe-theme magit json-mode ido-vertical-mode ido-ubiquitous ido-sort-mtime flx-ido eww-lnum ereader epresent elfeed-goodies darkokai-theme csv-mode company browse-kill-ring badwolf-theme avy atom-one-dark-theme atom-dark-theme anzu)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
