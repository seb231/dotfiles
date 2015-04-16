;;; package --- Summary
;;; I have some pacakges

;;; Commentary:

;;; Code:

;; (require 'package)
;; (add-to-list 'package-archives
;;   '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; graphical setups
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-nlinum-mode 1)
(global-visual-line-mode t)
(menu-bar-mode -1)

(setq tab-width 4)
(setq indent-tabs-mode nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq password-cache-expiry nil)

;; Setup packages
(require 'evil)
(evil-mode 1)
(require 'evil-org)
(require 'evil-leader)
(require 'magit)
(global-set-key (kbd "C-c m") 'magit-status)

(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(require 'epc)
(smartparens-global-mode t)
(require 'smartparens-config)

;; org-mode setup
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; setup yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("/home/mje/snippets"                 ;; personal snippets
        "/home/mje/.emacs.d/yasnippet/yasmate/snippets" ;; the yasmate collection
        "/home/mje/.emacs.d/yasnippet/snippets"         ;; the default collection
        ))
(yas-global-mode 1)

; Elpy setup
(elpy-enable)			   
(elpy-use-ipython)
;; Fixing a key binding bug in elpy
;; Fixing another key binding bug in iedit mode
(define-key global-map (kbd "C-c i") 'iedit-mode)
(global-set-key [C-tab] 'company-complete)
;; (add-hook 'after-init-hook 'global-company-mode)
;; (add-to-list 'company-backends 'company-anaconda)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'eldoc-mode)

;; Tramp setup
(setq tramp-default-method "ssh")

;; set shorcuts
(global-set-key (kbd "<C-s-up>") 'shrink-window)
(global-set-key (kbd "<C-s-down>") 'enlarge-window)
(global-set-key (kbd "<C-s-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-s-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<f5>") 'ispell)

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

(evilnc-default-hotkeys) ; setup evil-nerd-commenter

;; Setup python
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (autoload 'jedi:setup "jedi" nil t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:setup-keys t)
;; (eval-after-load "jedi"
;;     '(setq jedi:server-command (list "/home/mje/Toolboxes/anaconda/bin/python" jedi:server-script)))

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; setup whitespace marker
;; (require 'whitespace)
;; (setq whitespace-style '(face empty tabs lines-tail trailing))
;; (global-whitespace-mode t)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(my-long-line-face ((((class color)) (:background "gray10"))) t)
;;  '(my-tab-face ((((class color)) (:background "grey10"))) t)
;;  '(my-trailing-space-face ((((class color)) (:background "gray10"))) t))

;; load theme
;; (load-theme 'smyx t)
(load-theme 'tangotango t)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; add m-files to octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


;; setup thunderbird mode
;; (setq load-path (append load-path '("~/.emacs.d/lisp/")))
;; (require 'tbemail)
(setq auto-mode-alist
      (append '(("\.eml$" . text-mode))
              auto-mode-alist))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

; use pandocs as markdown tool
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "990920bac6d35106d59ded4c9fafe979fb91dc78c86e77d742237bc7da90d758" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "64581032564feda2b5f2cf389018b4b9906d98293d84d84142d90d7986032d33" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "49eea2857afb24808915643b1b5bd093eefb35424c758f502e98a03d0d3df4b1" "30a8a5a9099e000f5d4dbfb2d6706e0a94d56620320ce1071eede5481f77d312" "09233dff5af535c4ba3ccabc4c9267bb7bf1131cccbfab5db65e96103c7aa023" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7fbb8d064286706fb1e319c9d3c0a8eafc2efe6b19380aae9734c228b05350ae" "761d44dc06b3c8fff771435fd771b170d1bbdd71348b6aaaa6c0d0270d56cb70" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" default)))
 '(elpy-rpc-backend "rope")
 '(markdown-command "/usr/bin/pandoc")
 '(org-agenda-files (quote ("~/Dropbox/ToDo/idees.org")))
 '(python-shell-exec-path nil)
 '(python-shell-interpreter "/home/mje/Toolboxes/anaconda/bin/ipython")
 '(safe-local-variable-values (quote ((require-final-newline)))))


;; start ess
(require 'ess-site)

;; LaTeX configuration
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)

;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;; (add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; (setq TeX-PDF-mode t)

;; (setq TeX-output-view-style
;;     (quote
;;      (("^pdf$" "." "evince -f %o")
;;       ("^html?$" "." "iceweasel %o"))))

;; ;; Setting up writegood-mode
;; (require 'writegood-mode)
;; (global-set-key "\C-cg" 'writegood-mode)

;; start server
(if (not server-mode)
    (server-start nil t))



;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
