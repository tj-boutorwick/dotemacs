# Emacs configuration file

This is my emacs config file. I will be updating this as I find more things to add/tweak.


<a id="org8d68ca0"></a>

# Startup performance

    (setq gc-cons-threshold (* 50 1000 1000))
    
    (defun my/display-startup-time ()
      "Display startup time and garbage collection"
      (message "Emacs loaded in %s with %d garbage collections"
    	   (format "%.2f seconds"
    		       (float-time
    			(time-subtract after-init-time before-init-time)))
    	   gcs-done))
    
    (add-hook 'emacs-startup-hook #'my/display-startup-time)


<a id="orgb02743a"></a>

# Modes


<a id="org0364daa"></a>

## Ace jump mode

    (use-package ace-jump-mode
    :bind ("C-c j" . ace-jump-char-mode))


<a id="org9178d5d"></a>

## Bind-key

    (use-package bind-key)


<a id="org0795627"></a>

## Clojure/ClojureScript

The languages I spend my software development time with the most are these two. This section includes setup for them.

    (use-package cider
      :after org
      :config 
      (setq cider-auto-select-error-buffer t
    	cider-repl-pop-to-buffer-on-connect nil
    	cider-repl-use-clojure-font-lock t
    	cider-repl-wrap-history t
    	cider-repl-history-size 1000
    	cider-show-error-buffer t
    	nrepl-hide-special-buffers t
    	nrepl-popup-stacktraces nil)
    
      (add-hook 'cider-mode-hook 'eldoc-mode)
      (add-hook 'cider-repl-mode-hook 'paredit-mode))

    (use-package clojure-mode
      :after org
      :config
      (add-hook 'clojure-mode-hook #'paredit-mode)
      (add-hook 'clojure-mode-hook #'eldoc-mode))
    
    (use-package ob-clojurescript
      :after org
      :config
      (setq org-babel-clojure-backend 'cider))


<a id="org4c7fb2a"></a>

## clj-refactor

This is such a nice library. I need to learn more about it.

    (use-package clj-refactor
      :disabled t
      :config
      (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
      (cljr-add-keybindings-with-prefix "C-c C-m")
      (setq cljr-warn-on-eval 1))


<a id="orgc97ddd4"></a>

## Dired

The code below is used for folding dired sub-directories.

    (use-package dired-subtree
      :after dired
      :config
      (setq dired-subtree-use-backgrounds nil)
      :bind (:map dired-mode-map
    	      ("<tab>" . dired-subtree-toggle)
    	      ("<C-tab>" . dired-subtree-cycle)
    	      ("<S-iso-lefttab>" . dired-subtree-remove)))


<a id="org96d99ec"></a>

## Eldoc

    (use-package eldoc
      :config
        (eldoc-mode)
        (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
        (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
        (add-hook 'ielm-mode-hook 'eldoc-mode)
        (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
      :diminish "")


<a id="org6c53eb7"></a>

## EMMS

I have been trying out EMMS for playing music. It is going well, however the last time I tried I had an issue updating my music database. Other than that, it is great to be able to access my music from Emacs. The bits in this section were taken from Uncle Dave: <https://github.com/daedreth/UncleDavesEmacs#some-more-fun-stuff>.

    (use-package emms
      :config
      (require 'emms-player-mpd)
      (setq emms-player-mpd-server-name "localhost"
    	emms-player-mpd-server-port "6601"
    	emms-seek-seconds 5
    	emms-player-list '(emms-player-mpd)
    	emms-info-functions '(emms-info-mpd)
    	mpc-host "localhost:6601"
    	emms-source-file-default-directory "~/Music/"
    	emms-source-file-directory-tree-function 'emms-source-file-directory-tree-internal)
      (add-to-list 'emms-info-functions 'emms-info-mpd)
      (emms-all)
      :bind
      ("s-m l" . emms)
      ("s-m b" . emms-smart-browse)
      ("s-m r" . emms-player-mpd-update-all-reset-cache)
      ("<XF86AudioPrev>" . emms-previous)
      ("<XF86AudioNext>" . emms-next)
      ("s-m p" . emms-pause)
      ("s-m x" . emms-stop))


<a id="orga82d8cf"></a>

### Starting the daemon from within emacs

If you have an absolutely massive music library, it might be a good idea to get rid of mpc-update and only invoke it manually when needed.

    (defun mpd/start-music-daemon ()
      "Start MPD, connect to it and sync the metadata cache."
      (interactive "p")
      (shell-command "mpd")
      (mpd/update-database)
      (emms-player-mpd-connect)
      (emms-cache-set-from-mpd-all)
      (message "MPD Started!"))
    (global-set-key (kbd "s-m c") 'mpd/start-music-daemon)


<a id="org5a25065"></a>

### Killing the daemon from within emacs

    (defun mpd/kill-music-daemon ()
      "Stops playback and kill the music daemon."
      (interactive "p")
      (emms-stop)
      (call-process "killall" nil nil nil "mpd")
      (message "MPD Killed!"))
    (global-set-key (kbd "s-m k") 'mpd/kill-music-daemon)


<a id="orgc69bd20"></a>

### Updating the database easily.

    (defun mpd/update-database ()
      "Updates the MPD database synchronously."
      (interactive "p")
      (call-process "mpc" nil nil nil "update")
      (message "MPD Database Updated!"))
    (global-set-key (kbd "s-m u") 'mpd/update-database)


<a id="orgc409ee6"></a>

## ESS

This is for R integration into Emacs. I use R for mainly data analysis and visualization.

    (use-package ess
      :defer t
      :init (setq ess-ask-for-ess-directory nil))
    
      (setq use-package-verbose t)


<a id="orgef1213c"></a>

## Helm

    (use-package helm
      :diminish helm-mode
      :after org
      :config
        (setq helm-candidate-number-limit 100
    	  helm-idle-delay 0.0
    	  helm-input-idle-delay 0.01  
    	  helm-yas-display-key-on-candidate t
    	  helm-quick-update t
    	  helm-M-x-requires-pattern nil
    	  helm-ff-skip-boring-files t)
    
        :bind (("C-c h" . helm-mini)
    	   ("C-h a" . helm-apropos)
    	   ("C-x C-b" . helm-buffers-list)
    	   ("C-x b" . helm-buffers-list)
    	   ("M-y" . helm-show-kill-ring)
    	   ("M-x" . helm-M-x)
    	   ("C-x C-f" . helm-find-files)))


<a id="org96c0cb9"></a>

## Helm config

    (use-package helm-config
      :after helm)


<a id="org0d8cbe4"></a>

# Custom functions


<a id="orge6429f9"></a>

## Clear shell

    (defun my/clear-shell ()
       (interactive "p")
       (let ((old-max comint-buffer-maximum-size))
           (setq comint-buffer-maximum-size 0)
           (comint-truncate-buffer)
           (setq comint-buffer-maximum-size old-max)))
       (bind-key "C-c x" 'my/clear-shell)


<a id="org782660c"></a>

## Switch theme

    (defun my/switch-theme (theme)
      "Disables any currently active themes and loads theme."
      ;; This interactive call is taken from `load-theme'
      (interactive
       (list
        (intern (completing-read "Load custom theme: "
    			     (mapc 'symbol-name
    				   (custom-available-themes))))))
      (let ((enabled-themes custom-enabled-themes))
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme theme t)))
    
    (bind-key "s-<f12>" 'my/switch-theme)


<a id="org116ec4c"></a>

## Disable active theme

Sometimes switching from one theme to another will leave artefacts. This function resets to the default theme to avoid these artefacts.

    (defun my/disable-active-themes ()
      "Disables any currently active themes listed in `custom-enabled-themes'."
      (interactive "p")
      (mapc #'disable-theme custom-enabled-themes))
    (bind-key "s-<f11>" 'my/disable-active-themes)


<a id="orgda65246"></a>

## Reload config file

    ; reload init file
    (global-set-key (kbd "C-c i")
    (lambda () (interactive) (org-babel-load-file (concat user-emacs-directory "config.org"))))


<a id="orgcc038f8"></a>

# Interface tweaks

This section customizes the user interface and related items.

    (when (display-graphic-p)
      (menu-bar-mode -1)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (tooltip-mode -1))
    
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq inhibit-startup-message t
          initial-scratch-message "notepad"
          line-spacing 0.01
          global-auto-revert-non-file-buffers t
          auto-revert-verbose nil
          sentence-end-double-space nil
          echo-keystrokes 0.1
          ring-bell-function 'ignore
          backup-directory-alist `(("." . "~/.emacs.d/.saves"))
          frame-title-format '((buffer-file-name "%f" 
    					     (dired-directory "%b")))
          tab-width 1
          indent-tabs-mode nil
          display-time-default-load-average nil
          battery-mode-line-format " %b%p%%"
          calendar-latitude -41.3
          calendar-location-name "Welly"
          calendar-longitude 174.8
          calendar-standard-time-zone-name "NZDT"
          column-number-mode t
          org-image-actual-width nil) ;; allow for resizing of inline images
    (add-hook 'find-file-hooks   ;; Display full pathname for files.
    	  #'(lambda ()
    	     (setq mode-line-buffer-identification 'buffer-file-truename)))
    
    (add-to-list 'default-frame-alist
    	    '(font . "DejaVu Sans Mono-10"))
    (global-auto-revert-mode t) 
    (global-visual-line-mode 1)
    
    (set-face-attribute 'region nil :background "#000000"); HL color
    (put 'narrow-to-region 'disabled nil)
    
    (setq show-paren-delay 0
          show-paren-style 'parenthesis)


<a id="org6e76684"></a>

# Ledger

I periodically use this mode to keep track of my transactions. I have bash aliases that tend to be used more often, however.

    (use-package ledger-mode
      :ensure t
      :init
      (setq ledger-clear-whole-transactions 1)
    
      :mode "\\.dat$")


<a id="org20a2007"></a>

# Magit

I manage version control through Magit, as one does when one uses Emacs.

    (use-package magit
      :defer t
      :bind 
      ("C-x g" . magit-status))


<a id="org21e318b"></a>

# Org-mode

I spend heaps of time in org mode. This section lists the configuration I use.

       (use-package org
         :defer t
         :config
         (setq org-ellipsis "▾")
         (set-face-attribute 'org-ellipsis nil :underline nil )
    
         (setq org-directory "/home/tj/Dropbox/org/")
         (setq org-agenda-files (mapcar #'(lambda (f) (interactive) (expand-file-name f org-directory)) (directory-files org-directory nil "^\\w.*org$"))
    	   org-odt-preferred-output-format "docx"
    	   org-src-fontify-natively t)
    
         ;; log the date each when a deadline for a task changes
         (setq org-log-redeadline 'time)
    
         ;; log the date each when a schedule for a task changes
         (setq org-log-reschedule 'time)
    
         ;; ensure child tasks are marked 'done' before
         ;; a parent can be marked 'done'
         (setq org-enforce-todo-dependencies t)
    
         ;; ensure checkboxes are marked 'done' before 
         ;; their parent can be marked 'done'
         (setq org-enforce-todo-checkbox-dependencies t)
    
         (defun org-summary-todo (n-done n-not-done)
           "Switch entry to DONE when all subentries are done, to TODO otherwise."
           (let (org-log-done org-log-states)   ; turn off logging
    	 (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
    
         (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
         (add-hook 'org-mode-hook 'turn-on-flyspell 'append)
    
         (setq org-agenda-custom-commands
    	   '(("h" "Daily habits"
    	      ((agenda ""))
    	      ((org-agenda-show-log t)
    	       (org-agenda-ndays 7)
    	       (org-agenda-log-mode-items '(state))
    	       (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
    	     ;; other commands here
    	     ))
    
         ;; make things a bit cleaner by assigning org files to variables
         (setq business-file "~/Dropbox/org/business.org")
         (setq ledger-file "~/Dropbox/Life/Money/ledger.dat")
         (setq work-file "~/Dropbox/org/work.org")
         (setq research-file "~/Dropbox/org/research.org")
    
         (setq org-capture-templates
    	   '(("h" "Home" entry (file "~/Dropbox/org/home.org") "* TODO %?")
    
    	     ("b" "Business")
    	     ("be" "ESL Speed Reading" entry (file+olp business-file "Inventful" "ESL Speed Reading") "* TODO %?\n %a")
    	     ("bf" "Footbag Dictionary" entry (file+olp business-file "Inventful" "Footbag Dictionary App") "* TODO %?")
    	     ("bs" "Shred Log" entry (file+olp business-file "Inventful" "Shred Log") "* TODO %?")
    	     ("bt" "Tasks" entry (file+headline business-file "Tasks") "* TODO %?")
    
    	     ("j"  "Journal" plain (file+olp+datetree "~/Dropbox/org/journal.org") "%U %?")
    
    	     ("r" "Research"  entry (file+headline research-file "Research") "* TODO %?")
    
    	     ("w" "work")
    	     ("wt" "Task" entry (file+headline work-file "Tasks") "* TODO %?")
    	     ("ww" "Workshop" entry (file+headline work-file "Workshops") "* TODO %?")))
    
         (defun my-read-date ()
           "Parse date for capturing ledger entries via org mode"
           (replace-regexp-in-string "-" "/" (org-read-date)))
    
         (setq org-file-apps '((auto-mode . emacs) ("\\.pdf\\'" . "okular %s"))
    	   org-clock-into-drawer t
    	   org-log-done t
    	   org-confirm-babel-evaluate nil
    	   org-cycle-separator-lines 0
    	   org-deadline-warning-days 14)
    
         (setq org-latex-pdf-process
    	   '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
    
         ;; put org-table captions below table, not above
         (setq org-latex-table-caption-above nil) 
    
         (org-babel-do-load-languages
          'org-babel-load-languages
          '((R . t)
    	(C . t)
    	(ditaa . t)
    	(emacs-lisp . t)
    	(latex . t)
    	(clojure . t)
    	(clojurescript . t)
    	(shell . t)))
    
         (require 'org-tempo) 
         (add-to-list 'org-structure-template-alist '("di" . "src ditaa"))
         (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
         (add-to-list 'org-structure-template-alist '("r" . "src R"))
         (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
         (add-to-list 'org-structure-template-alist '("cljs" . "src clojurescript"))
    
    
    ; change src block color
    (set-face-attribute 'org-block nil :background
    		    (color-darken-name
    		     (face-attribute 'default :background) 3)
    		     :extend t)
    
    
    
    
         ;; ditaa commands
         (setq org-babel-ditaa-java-cmd "java")
         (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")
    
         (setq auto-mode-alist ; file extentions to open in orgmode or other modes
    	   '(("\\.org"  . org-mode)  
    	     ("\\.gpg$"  . org-mode)  
    	     ("\\.md$"   . org-mode)
    	     ("\\.txt$"  . org-mode)
    	     ("\\.clj$"   . clojure-mode)
    	     ("\\.cljc$"   . clojure-mode)
    	     ("\\.cljs$"   . clojurescript-mode)
    	     ("\\.edn\\'" . clojure-mode)
    	     ("\\.tex$"  . LaTeX-mode)
    	     ("\\.php$"  . web-mode)
    	     ("\\.css$"  . web-mode)
    	     ("\\.c$" . c-mode)
    	     ("\\.cpp$" . c-mode)
    	     ("\\.html$" . web-mode)))
    
         :bind
         (("C-c l"  . org-store-link)
         ("C-c c"  . org-capture)
         ("C-c a"  . org-agenda)))


<a id="org8e8b4b0"></a>

# ox-latex

This is for LaTeX support for org's exporter.

    (use-package ox-latex
    :defer t)


<a id="org9d032e8"></a>

# Paredit

Maintaining parenthetical agreement, and more.

    (use-package paredit)


<a id="org146f0bf"></a>

# Shortcuts

I have created a couple of key chords for things like changing text scale, and for visiting files I frequent.

    (bind-key "C-+" 'text-scale-increase)
    (bind-key "C--" 'text-scale-decrease)
    
    (global-set-key (kbd "C-c t") ; visit todo file
    		(lambda () "Visit your home TODO file" (interactive) (find-file "~/Dropbox/org/home.org")))
    
    (global-set-key (kbd "C-c o") ; visit config file
    		(lambda () "Visit your config file" (interactive) (find-file "~/.emacs.d/config.org"))) 
    
    (global-set-key (kbd "C-c e") ; visit ledger file
    		(lambda () "Vist your ledger file" (interactive) (find-file ledger-file))) 


<a id="org488c5b1"></a>

# Runtime Performance

This is used to reset the garbage collection cycles to collection happens more frequently but in less time.

    (setq gc-cons-threshold (* 2 1000 1000))

