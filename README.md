# Emacs configuration file
This is my configuration file for Emacs. The personal bits have NOT been included meaning you will have to fill in with your bits accordingly. I have left the code in place, with the information removed (e.g., email address).

# Modes

## Ace jump mode

    (use-package ace-jump-mode
    :bind ("C-c j" . ace-jump-char-mode))

## Bind-key

    (use-package bind-key)

## Clojure/ClojureScript

The languages I spend my software development time with the most are these two. This section includes setup for them.

    (use-package cider
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
      :config
      (add-hook 'clojure-mode-hook #'paredit-mode)
      (add-hook 'clojure-mode-hook #'eldoc-mode))
    
    (use-package ob-clojurescript
      :config
      (setq org-babel-clojure-backend 'cider))


## clj-refactor

This is such a nice library. I need to learn more about it.

    (use-package clj-refactor
      :disabled t
      :config
      (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
      (cljr-add-keybindings-with-prefix "C-c C-m")
      (setq cljr-warn-on-eval 1))

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

## Eldoc

    (use-package eldoc
      :config
        (eldoc-mode)
        (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
        (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
        (add-hook 'ielm-mode-hook 'eldoc-mode)
        (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)
      :diminish "")


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


### Killing the daemon from within emacs

    (defun mpd/kill-music-daemon ()
      "Stops playback and kill the music daemon."
      (interactive "p")
      (emms-stop)
      (call-process "killall" nil nil nil "mpd")
      (message "MPD Killed!"))
    (global-set-key (kbd "s-m k") 'mpd/kill-music-daemon)


### Updating the database easily.

    (defun mpd/update-database ()
      "Updates the MPD database synchronously."
      (interactive "p")
      (call-process "mpc" nil nil nil "update")
      (message "MPD Database Updated!"))
    (global-set-key (kbd "s-m u") 'mpd/update-database)


## ESS

This is for R integration into Emacs. I use R for mainly data analysis and visualization.

    (use-package ess
      :init
      (progn
        (setq ess-ask-for-ess-directory nil)
        (add-to-list 'org-structure-template-alist
    		 '("d" "ditaa"))
        (add-to-list 'org-structure-template-alist   
    		 '("E" "emacs-lisp"))
        (add-to-list 'org-structure-template-alist
    		 '("r" "R"))))


## Gnus

Email management (and much more!) through Emacs. Some personal information has been left out.

    (use-package gnus
      :commands gnus
      :config
      (setq gnus-select-method '(nnnil))
      (setq gnus-secondary-select-methods
    	'((nnimap "gmail"
    		  (nnimap-address "imap.gmail.com")
    		  (nnimap-server-port 993)
    		  (nnimap-stream ssl))
    	  (nnimap "hotmail"
    		  (nnimap-address "imap-mail.outlook.com")
    		  (nnimap-server-port 993)
    		  (nnimap-stream ssl))
    	  (nnimap "yahoo"
    		  (nnimap-address "imap.mail.yahoo.com")
    		  (nnimap-server-port 993)
    		  (nnimap-stream ssl))))
    
      (setq gnus-summary-line-format "%U%I%[%-20,20f%]: %s\n")
      (setq gnus-group-line-format "%M%y: [%G]--%c\n")
      (setq gnus-site-init-file "~/.gnus.el")
      (when window-system
        (setq gnus-sum-thread-tree-indent "  ")
        (setq gnus-sum-thread-tree-root "") 
        (setq gnus-sum-thread-tree-false-root "") 
        (setq gnus-sum-thread-tree-single-indent "")
        (setq gnus-sum-thread-tree-vertical        "│")
        (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
        (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
      (setq gnus-summary-line-format
    	(concat
    	 "%0{%U%R%z%}"
    	 "%3{│%}" "%1{%d%}" "%3{│%}"   ;; date
    	 "  "
    	 "%4{%-20,20f%}"               ;; name
    	 "  "
    	 "%3{│%}"
    	 " "
    	 "%1{%B%}"
    	 "%s\n"))
      (setq gnus-summary-display-arrow t))


## Helm

    (use-package helm
      :diminish helm-mode
      :config
      (progn
        (use-package helm-config)
        (setq helm-candidate-number-limit 100
    	  helm-idle-delay 0.0
    	  helm-input-idle-delay 0.01  
    	  helm-yas-display-key-on-candidate t
    	  helm-quick-update t
    	  helm-M-x-requires-pattern nil
    	  helm-ff-skip-boring-files t)
        (helm-mode)
    
        (use-package helm-bibtex
        :disabled true
        :bind (("C-c h" . helm-mini)
    	   ("C-h a" . helm-apropos)
    	   ("C-x C-b" . helm-buffers-list)
    	   ("C-x b" . helm-buffers-list)
    	   ("M-y" . helm-show-kill-ring)
    	   ("M-x" . helm-M-x)
    	   ("C-x C-f" . helm-find-files)))


# Custom functions


## clear shell

    (defun my/clear-shell ()
       (interactive "p")
       (let ((old-max comint-buffer-maximum-size))
           (setq comint-buffer-maximum-size 0)
           (comint-truncate-buffer)
           (setq comint-buffer-maximum-size old-max)))
       (bind-key "C-c x" 'my/clear-shell)


## switch theme

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


## Disable active theme

Sometimes switching from one theme to another will leave artefacts. This function resets to the default theme to avoid these artefacts.

    (defun my/disable-active-themes ()
      "Disables any currently active themes listed in `custom-enabled-themes'."
      (interactive "p")
      (mapc #'disable-theme custom-enabled-themes))
    (bind-key "s-<f11>" 'my/disable-active-themes)


## (re)load config file

    ; reload init file
    (global-set-key (kbd "C-c i")
    (lambda () (interactive) (org-babel-load-file (concat user-emacs-directory "config.org"))))


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


# Ledger

I periodically use this mode to keep track of my transactions. I have bash aliases that tend to be used more often, however.

    (use-package ledger-mode
      :ensure t
      :init
      (setq ledger-clear-whole-transactions 1)
    
      :mode "\\.dat$")


# Magit

I manage version control through Magit, as one does when one uses Emacs.

    (use-package magit
      :defer t
      :bind 
      ("C-x g" . magit-status))


# Org-mode

I spend heaps of time in org mode. This section lists the configuration I use.

    (use-package org
      :config
      (setq org-ellipsis "▾")
      (set-face-attribute 'org-ellipsis nil :underline nil )
    
      (setq org-directory "")
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
      (setq business-file "")
      (setq ledger-file "")
      (setq work-file "")
      (setq research-file "")
    
      (setq org-capture-templates
    	'(("h" "Home" entry (file "")
    
    	  ("b" "Business")
    	  ("be" "ESL Speed Reading" entry (file+olp business-file "Inventful" "ESL Speed Reading") "* TODO %?\n %a")
    	  ("bf" "Footbag Dictionary" entry (file+olp business-file "Inventful" "Footbag Dictionary App") "* TODO %?")
    	  ("bs" "Shred Log" entry (file+olp business-file "Inventful" "Shred Log") "* TODO %?")
    	  ("bt" "Tasks" entry (file+headline business-file "Tasks") "* TODO %?")
    
    	  ("j"  "Journal" plain (file+olp+datetree "")
    
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


# Org bullets

I suppose I wanted a bit more flavor when looking at my org mode headlines. This is one nice library for that.

    (use-package org-bullets
      ; :ensure t
      :config
      (add-hook 'org-mode-hook 'org-bullets-mode))


# ox-latex

This is for LaTeX support for org's exporter.

    (use-package ox-latex)


# Org-ref

I find this to be extremely helpful for formatting and citing references. The path 

    (use-package org-ref
    :init 
    
    (setq	bibtex-completion-bibliography  "/path/to/biblio"
          bibtex-completion-library-path  "/path/to/library"
          bibtex-completion-notes-path    "/path/to/notes"))


# Paredit

Maintaining parenthetical agreement, and more.

    (use-package paredit)


# Php-mode

From time to time I find myself looking at PHP code. This section helps with that.

    (use-package php-mode
      ; :ensure t
      :defer t
      :config
      (defun bs-php-mode-hook ()
        (setq indent-tabs-mode nil)
        (setq c-basic-offset 2)
        (setq php-template-compatibility nil)
        (subword-mode 1))
      (add-hook 'php-mode-hook 'bs-php-mode-hook)
    
      (use-package ac-php
        :ensure t))


# Smtp (sending mail)

This section includes configuration to send email from Emacs (using Gnus)

    (use-package smtpmail
      :disabled t
      :init
      (setq smtpmail-default-smtp-server "smtp.gmail.com")
      :config
      (setq smtpmail-smtp-server "smtp.gmail.com"
    	smtpmail-stream-type 'ssl
    	smtpmail-smtp-service 465))


# Shortcuts

I have created a couple of key chords for things like changing text scale, and for visiting files I frequent.

    (bind-key "C-+" 'text-scale-increase)
    (bind-key "C--" 'text-scale-decrease)
    
    (global-set-key (kbd "C-c t") ; visit todo file
    		(lambda () "Visit your home TODO file" (interactive) (find-file "")))
    
    (global-set-key (kbd "C-c o") ; visit config file
    		(lambda () "Visit your config file" (interactive) (find-file "~/.emacs.d/config.org"))) 
    
    (global-set-key (kbd "C-c e") ; visit ledger file
    		(lambda () "Vist your ledger file" (interactive) (find-file ledger-file))) 


# Web-mode

    (use-package web-mode
       :ensure t
       ;:disabled t
       :config
       (defun bs-web-mode-hook ()
         (local-set-key '[backtab] 'indent-relative)
         (setq indent-tabs-mode nil)
         (setq web-mode-markup-indent-offset 2
    	   web-mode-css-indent-offset 2
    	   web-mode-code-indent-offset 2))
       (add-hook 'web-mode-hook 'bs-web-mode-hook)
       (defun toggle-php-flavor-mode ()
         (interactive "p")
         "Toggle mode between PHP & Web-Mode Helper modes"
         (cond ((string= mode-name "PHP")
    	    (web-mode))
    	   ((string= mode-name "Web")
    	    (php-mode))))
       (global-set-key [f5] 'toggle-php-flavor-mode))

