;; Load MELPA & ELPA package managers
(require 'package)
(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

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

;; Set UTF-8 as default encoding
;; http://doc.norang.ca/org-mode.html#AgendaViewTweaks
(setq org-export-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
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

;; Get rid of those damn blank lines
;; http://orgmode.org/worg/org-faq.html
;; (setq org-blank-before-new-entry nil)

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
;;(setq org-enforce-todo-dependencies t)
;; (setq org-track-ordered-property-with-tag t)
;; (setq org-agenda-dim-blocked-tasks t)

;; Don't show blocked tasks in agenda view
;; (setq org-agenda-dim-blocked-tasks 'invisible)

;; Show deadlines only on the day they are due in week view
;; http://pragmaticemacs.com/emacs/org-mode-basics-vii-a-todo-list-with-schedules-and-deadlines/
(setq org-deadline-warning-days 0)

;; Show only current instance of repeating
;; http://orgmode.org/worg/org-faq.html
;; (setq org-agenda-repeating-timestamp-show-all t)

;; Record task completion time
;; (setq org-log-done 'time)

;; Hide formatting characters for bold, italics, etc.
;; https://www.reddit.com/r/emacs/comments/4bzj6a/how_to_get_orgmode_emphasis_markup_et_al_to/
;; (setq org-hide-emphasis-markers t)

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

;; Set default directory for DB
;; (setq elfeed-db-directory "~/Dropbox/Apps/Elfeed")

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

(setq elfeed-sort-order 'ascending)

(setq elfeed-feeds
      '("http://3quarksdaily.blogs.com/3quarksdaily/atom.xml"
	"http://akroncanton.craigslist.org/search/sso?catAbb=sso&excats=97-21-63-6&query=laminate%20flooring&sort=date&format=rss"
	"http://alanfriedman.tumblr.com/rss"
	"http://amritrichmond.tumblr.com/rss"
	"http://awkwardfamilyphotos.com/?feed=rss2"
	"http://ben-evans.com/benedictevans?format=rss"
	"http://blog.erratasec.com/feeds/posts/default"
	"http://blog.fitbit.com/?feed=rss2"
	"http://blog.gtdnext.com/feed/"
	"http://blog.lmorchard.com/index.rss"
	"http://blog.modelworks.ch/?feed=rss2"
	"http://blog.seliger.com/feed/"
	"http://blog.trello.com/feed/"
	"http://boingboing.net/rss.xml"
	"http://bookshelfporn.com/rss"
	"http://boughtitonce.com/feed/"
	"http://bps-research-digest.blogspot.com/feeds/posts/default?alt=rss"
	"http://bulletjournal.com/feed/"
	"http://calnewport.com/blog/feed/"
	"http://chetramey.blogspot.com/feeds/posts/default"
	"http://chrisguillebeau.com/3x5/feed/"
	"http://chrome.blogspot.com/atom.xml"
	"http://clevelandmagazine.blogspot.com/feeds/posts/default"
	"http://clevelandprintroom.com/feed/"
	"http://continuations.com/rss"
	"http://coolmaterial.com/feed/"
	"http://decodedpast.com/feed"
	"http://dilbert.com/blog/entry.feed/"
	"http://donebeforebrekky.com/feed/"
	"http://drandus.wordpress.com/feed/"
	"http://edward.oconnor.cx/feed"
	"http://emacs.stackexchange.com/feeds/tag?tagnames=org-mode&sort=votes"
	"http://emacsredux.com/atom.xml"
	"http://en.community.dell.com/techcenter/os-applications/rss"
	"http://endlessparentheses.com/atom.xml"
	"http://favoriteandforget.com/rss"
	"http://feed43.com/1240263800287635.xml"
	"http://feedproxy.google.com/passiveaggressivenotes"
	"http://feeds.arstechnica.com/arstechnica/features/"
	"http://feeds.feedburner.com/ALifeCoachsBlog"
	"http://feeds.feedburner.com/American"
	"http://feeds.feedburner.com/BrazenCareerist"
	"http://feeds.feedburner.com/BrettTerpstra"
	"http://feeds.feedburner.com/ChefSteps"
	"http://feeds.feedburner.com/ConnectingTechnologyStrategyAndExecution"
	"http://feeds.feedburner.com/DumbLittleMan"
	"http://feeds.feedburner.com/FindingFreeEbooks"
	"http://feeds.feedburner.com/FlowingData"
	"http://feeds.feedburner.com/GoogleAppsUpdates"
	"http://feeds.feedburner.com/GoogleOperatingSystem"
	"http://feeds.feedburner.com/HighScalability"
	"http://feeds.feedburner.com/IeeeSpectrum"
	"http://feeds.feedburner.com/InformationIsBeautiful"
	"http://feeds.feedburner.com/InstigatorBlog"
	"http://feeds.feedburner.com/ItProductManagement"
	"http://feeds.feedburner.com/JCCharts"
	"http://feeds.feedburner.com/LazyManAndMoney"
	"http://feeds.feedburner.com/LeanBlog"
	"http://feeds.feedburner.com/MrMoneyMustache"
	"http://feeds.feedburner.com/OfficialGoogleMobileBlog"
	"http://feeds.feedburner.com/OpenCulture"
	"http://feeds.feedburner.com/PracticallyEfficient"
	"http://feeds.feedburner.com/StudyHacks"
	"http://feeds.feedburner.com/The99Percent"
	"http://feeds.feedburner.com/TheBoyGeniusReport"
	"http://feeds.feedburner.com/TheThrillingWonderStory"
	"http://feeds.feedburner.com/TimelessInformation"
	"http://feeds.feedburner.com/advancedriskology"
	"http://feeds.feedburner.com/alansjourney"
	"http://feeds.feedburner.com/avc"
	"http://feeds.feedburner.com/bakadesuyo"
	"http://feeds.feedburner.com/blogspot/summertomato"
	"http://feeds.feedburner.com/design-milk"
	"http://feeds.feedburner.com/oreilly/radar/atom"
	"http://feeds.feedburner.com/pickthebrain/LYVv"
	"http://feeds.feedburner.com/pmsolutions"
	"http://feeds.feedburner.com/rudiusmedia/rch"
	"http://feeds.feedburner.com/smittenkitchen"
	"http://feeds.feedburner.com/theminimalists/Hztx"
	"http://feeds.feedburner.com/typepad/HerdingCats"
	"http://feeds.feedburner.com/unclutterer"
	"http://feeds.feedburner.com/uncrate"
	"http://feeds.feedburner.com/wordpress/Kyvt"
	"http://feeds.feedburner.com/zenhabits"
	"http://feeds.wired.com/wired/index"
	"http://feeds2.feedburner.com/Command-line-fu"
	"http://feeds2.feedburner.com/Smarterware"
	"http://feeds2.feedburner.com/VioletBlueOpenSourceSex"
	"http://feeds2.feedburner.com/advancedlifeskills/MClm"
	"http://feeds2.feedburner.com/marcofolio"
	"http://feeds2.feedburner.com/raptitudecom"
	"http://flowingdata.com/feed"
	"http://fridayinvegas.blogspot.com/feeds/posts/default?alt=rss"
	"http://fuckinghomepage.com/rss"
	"http://gandenberger.org/feed/"
	"http://garfieldminusgarfield.net/rss"
	"http://gigapan.blogspot.com/feeds/posts/default"
	"http://gmailblog.blogspot.com/atom.xml"
	"http://godzillahaiku.tumblr.com/rss"
	"http://googleappsdeveloper.blogspot.com/feeds/posts/default"
	"http://googleblog.blogspot.com/atom.xml"
	"http://googledocs.blogspot.com/atom.xml"
	"http://heikkil.github.io/rss.xml"
	"http://hnrss.org/newest?points=100"
	"http://infosystir.blogspot.com/feeds/posts/default"
	"http://jakubsan.tumblr.com/rss"
	"http://krebsonsecurity.com/feed/"
	"http://lesswrong.com/.rss"
	"http://livelymorgue.tumblr.com/rss"
	"http://longform.org/feed"
	"http://longreads.com/rss"
	"http://management.curiouscatblog.net/feed/"
	"http://maplight.org/rss.xml"
	"http://massively.joystiq.com/tag/EVE-Evolved/rss.xml"
	"http://mcfunley.com/feed/atom"
	"http://mikerowe.com/feed/"
	"http://mosex.wordpress.com/feed/"
	"http://news.google.com/news?cf=all&hl=en&pz=1&ned=us&topic=h&num=3&output=rss"
	"http://nextdraft.com/archives/feed/"
	"http://obviousplant.tumblr.com/rss"
	"http://onethingwell.org/rss"
	"http://pervocracy.blogspot.com/feeds/posts/default?alt=rss"
	"http://physics.ucsd.edu/do-the-math/feed/"
	"http://pitchfork.com/rss/thepitch/"
	"http://planet.emacsen.org/atom.xml"
	"http://pmstudent.com/feed/atom"
	"http://polyrad.info/feed/"
	"http://positech.co.uk/cliffsblog/?feed=rss2"
	"http://pragmaticemacs.com/feed/"
	"http://prodissues.com/feed"
	"http://productivity.stackexchange.com/feeds"
	"http://prostheticknowledge.tumblr.com/rss"
	"http://proxypaige.tumblr.com/rss"
	"http://putthison.com/rss"
	"http://quiterss.org/en/rss.xml"
	"http://ratracerebellion.com/feed/"
	"http://riotclitshave.livejournal.com/data/atom"
	"http://riotclitshave.tumblr.com/rss"
	"http://rockitreports.com/feed/"
	"http://rogueprojectleader.blogspot.com/feeds/posts/default?alt=rss"
	"http://rss.slashdot.org/slashdot/eqWf"
	"http://rustbeltchic.com/feed/"
	"http://sachachua.com/blog/feed"
	"http://sciencehorrors.tumblr.com/rss"
	"http://scifispaceships.tumblr.com/rss"
	"http://scoptophilia.blogspot.com/feeds/posts/default?alt=rss"
	"http://sethgodin.typepad.com/seths_blog/atom.xml"
	"http://shutupandtakemymoney.com/?feed=rss2"
	"http://slatestarcodex.com/feed/"
	"http://stackexchangenocontext.tumblr.com/rss"
	"http://stackoverflow.com/feeds/tag?tagnames=elisp&sort=newest"
	"http://startuplessonslearned.blogspot.com/feeds/posts/default"
	"http://sthelenaonline.org/feed/"
	"http://stratechery.com/feed/"
	"http://sudophilosophical.com/feed/"
	"http://survivetheapocalypse.wordpress.com/feed/"
	"http://syndication.thedailywtf.com/TheDailyWtf"
	"http://takingnotenow.blogspot.com/feeds/posts/default"
	"http://talkabout.makelovenotporn.tv/rss"
	"http://technabob.com/blog/feed/"
	"http://thatsnerdalicious.com/feed/"
	"http://thechirurgeonsapprentice.com/feed/"
	"http://theshapeofamother.com/feed/"
	"http://thoughtinfection.com/feed/"
	"http://toolsandtoys.net/feed/"
	"http://townhall.com/xml/columnists/author/johnstossel/"
	"http://transformerstation.org/News/files/feed.xml"
	"http://transitionculture.org/feed/"
	"http://trekohio.com/feed/"
	"http://us15.campaign-archive.com/feed?u=eee7b8043119f98544067854b&id=d581eab324"
	"http://uxmag.com/rss.xml"
	"http://vintagepulchritude.blogspot.com/feeds/posts/default"
	"http://violentmetaphors.com/feed/"
	"http://wavefunction.fieldofscience.com/feeds/posts/default"
	"http://what-if.xkcd.com/feed.atom"
	"http://wvs.topleftpixel.com/index.rdf"
	"http://www.adaringadventure.com/feed/"
	"http://www.atlasobscura.com/feeds/places"
	"http://www.badassoftheweek.com/rss.xml"
	"http://www.betterprojects.net/feeds/posts/default"
	"http://www.chefsteps.com/feed"
	"http://www.clevescene.com/cleveland/Rss.xml"
	"http://www.coolthings.com/feed/"
	"http://www.craigslist.org/about/best/all/index.rss"
	"http://www.crainscleveland.com/feed/breaking_news"
	"http://www.cyberciti.biz/feed/"
	"http://www.dailydot.com/feed/summary/latest/"
	"http://www.devalot.com/feeds/all.rss"
	"http://www.ed.gov/feed"
	"http://www.farnamstreetblog.com/feed/atom/"
	"http://www.foxnews.com/about/rss/feedburner/foxnews/special-report"
	"http://www.fxckfeelings.com/feed/"
	"http://www.howardism.org/index.xml"
	"http://www.inknouveau.com/feeds/posts/default"
	"http://www.jamesaltucher.com/feed/"
	"http://www.joelonsoftware.com/rss.xml"
	"http://www.kungfugrippe.com/rss"
	"http://www.kurzweilai.net/news/feed/atom"
	"http://www.lastwordonnothing.com/feed/"
	"http://www.lovesciencemedia.com/love-science-media/rss.xml"
	"http://www.marcsijan.com/feed/"
	"http://www.marriedmansexlife.com/feeds/posts/default?alt=rss"
	"http://www.mattcutts.com/blog/feed/"
	"http://www.mindhacks.com/atom.xml"
	"http://www.nakedcapitalism.com/feed"
	"http://www.nostarch.com/feeds/newbooks.xml"
	"http://www.overcomingbias.com/feed"
	"http://www.patternbased.com/feed/"
	"http://www.penaddict.com/blog?format=RSS"
	"http://www.playterm.org/data/cache/rss-latest.xml"
	"http://www.productbeautiful.com/feed/"
	"http://www.producthunt.com/feed"
	"http://www.quantamagazine.org/feed/"
	"http://www.quietearth.us/rss.xml"
	"http://www.quotationspage.com/data/qotd.rss"
	"http://www.reason.com/news/index.xml"
	"http://www.reddit.com/r/DepthHub/.rss"
	"http://www.reddit.com/r/emacs/.rss"
	"http://www.scottberkun.com/feed/"
	"http://www.skmurphy.com/feed/"
	"http://www.steve-olson.com/feed/"
	"http://www.stilldrinking.org/rss/feed.xml"
	"http://www.svpg.com/articles/rss"
	"http://www.techmeme.com/feed.xml"
	"http://www.tedunangst.com/inks/rss"
	"http://www.thepostnewspapers.com/search/?f=rss&t=article&c=wadsworth&l=50&s=start_time&sd=desc"
	"http://www.thewhoresofyore.com/14/feed"
	"http://www.thisiswhyimbroke.com/feed"
	"http://www.threestaples.com/blog?format=RSS"
	"http://www.tinyhousetalk.com/feed/"
	"http://www.wadsworth.k12.oh.us/rss.xml"
	"http://www.waitbutwhy.com/feed"
	"http://www.warisboring.com/feed/"
	"http://www.wellappointeddesk.com/feed/"
	"http://xkcd.com/rss.xml"
	"http://xml.metafilter.com/atom.xml"
	"http://youarenotsosmart.com/feed/"
	"http://zompist.wordpress.com/feed/"
	"https://blog.getremarkable.com/feed"
	"https://hacked.com/feed/"
	"https://lobste.rs/rss"
	"https://medium.com/feed/hacker-daily"
	"https://scienceofsharp.wordpress.com/feed/"
	"https://theconversation.com/us/feeds"
	"https://travelingwithcysticfibrosis.wordpress.com/feed/"
	"https://www.bloomberg.com/view/rss/topics/money-stuff.rss"
	"https://www.format.com/magazine/feed"
	"https://www.jumpstartinc.org/feed/"
	"https://www.reddit.com/r/bestof/.rss"
	"https://www.rstudio.com/feed/"))
