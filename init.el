;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
            '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
 	      '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    evil
    monokai-theme
    yasnippet
    evil-nerd-commenter 
    projectile
    window-numbering
    atom-dark-theme
    py-autopep8))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'atom-dark t)
;; (load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; Changes all lyes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; add m-files to octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; for thunderbird emails
(setq auto-mode-alist
      (append '(("\.eml$" . text-mode))
              auto-mode-alist))

;; Wrap lines
(global-visual-line-mode 1)

;; start server
(if (not server-mode)
    (server-start nil 1))

;; set shorcuts
(global-set-key (kbd "<C-s-up>") 'shrink-window)
(global-set-key (kbd "<C-s-down>") 'enlarge-window)
(global-set-key (kbd "<C-s-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-s-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f5>") 'ispell)


;; Org-mode settings
;; --------------------------------------
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)

;; Set aganda file(s)
(setq org-agenda-files (list "~/Dropbox/ToDo/projects.org"
                             "~/Dropbox/ToDo/todo.org"
                             "~/Dropbox/ToDo/postdoc.org"))
;; Set tags
(setq org-tag-alist '(("@work" . ?k)
                      ("@home" . ?h)
                      ("IMPORTANT" . ?i)
                      ("email" . ?e)
                      ("phone" . ?p)
                      ("write" . ?w)
                      ("analysis" . ?a)))

;; function to archive done tasks
;; (defun my-org-archive-done-tasks ()
;;   (interactive)
;;   (org-map-entries 'org-archive-subtree "/DONE" '"~/Dropbox/ToDo/todo_archive.org")

;; PYTHON CONFIGURATION
;; --------------------------------------
(defun set-exec-path-from-shell-PATH ()
        (interactive)
        (let ((path-from-shell (replace-regexp-in-string "^.*\n.*shell\n" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
        (setenv "PATH" path-from-shell)
        (setq exec-path (split-string path-from-shell path-separator))))
 
(set-exec-path-from-shell-PATH)


(elpy-enable)
(elpy-use-ipython)
;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'pyll-autopep8-enable-on-save)


;; DIV PACKAGE SETUP
;; --------------------------------------
(require 'iedit)
(define-key global-map (kbd "C-c i") 'iedit-mode)
(global-set-key [C-tab] 'company-complete)

;; (add-to-list 'load-path "~/.emacs.d/plugins/")
;; (require 'evil-leader)
;; (global-evil-leader-mode)
;; (evil-leader/set-leader ",")
;; (evil-leader/set-key
;;   "e" 'find-file
;;   "b" 'switch-to-buffer
;;   "k" 'kill-buffer)

(require 'evil)
(evil-mode 1)
;; EVIL settings
;; Map jk to esc
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(setf sentence-end-double-space nil)

(evilnc-default-hotkeys)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))


(require 'yasnippet)
(setq yas-snippet-dirs
      '("/home/mje/snippets"                 ;; personal snippets
        ;; "/home/mje/.emacs.d/yasnippet/yasmate/snippets" ;; the yasmate collection
        "/home/mje/.emacs.d/snippets"         ;; the default collection
        ))
(yas-global-mode 1)
;; setup for tab completion in term
(add-hook 'term-mode-hook (lambda()
        (setq yas-dont-activate t)))

(require 'magit)
(global-set-key (kbd "C-c m") 'magit-status)
(defun magit-expand-git-file-name (filename)
  (unless (file-name-absolute-p filename)
    (setq filename (expand-file-name filename)))
  (if (and (eq system-type 'windows-nt) ; together with cygwin git, see #1318
           (string-match "^/\\(cygdrive/\\)?\\([a-z]\\)/\\(.*\\)" filename))
      (concat (match-string 2 filename) ":/"
              (match-string 3 filename))
    filename))

(setq projectile-enable-caching t)

;; window numbers
(require 'window-number)
(setq window-number-mode t)

;; ESS SETUP
;; --------------------------------------

;; start ess
(require 'ess-site)
(require 'ess-jags-d)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a1289424bbc0e9f9877aa2c9a03c7dfd2835ea51d8781a0bf9e2415101f70a7e" "f8fceb5cce25882d0842aac0e75000bc1a06e3c4eac89b61103c6dbfa88e40ad" "3f78849e36a0a457ad71c1bda01001e3e197fe1837cb6eaa829eb37f0a4bdad5" "705f3f6154b4e8fac069849507fd8b660ece013b64a0a31846624ca18d6cf5e1" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "0c49a9e22e333f260126e4a48539a7ad6e8209ddda13c0310c8811094295b3a3" default)))
 '(org-agenda-tags-column -105)
 '(org-auto-align-tags t)
 '(package-selected-packages
   (quote
    (atom-dark-theme jbeans-theme ujelly-theme atom-one-dark-theme tangotango-theme magit with-editor e monokai-theme py-autopep8 window-number twilight-theme smartparens projectile nlinum neotree molokai-theme markdown-mode flycheck evil-org evil-nerd-commenter evil-leader evil-iedit-state ess epc elpy ein better-defaults))))

 '(markdown-command "/usr/bin/pandoc")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 90 :width normal)))))


;; Open files at startup
(find-file "~/Dropbox/ToDo/todo.org") 
(find-file "~/Dropbox/ToDo/projects.org") 
;; '(*scratch* t)
(switch-to-buffer "*scratch*")
;; init.el ends here
