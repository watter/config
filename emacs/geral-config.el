;;; geral-config.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Leslie Harlley Watter

;; Author: Leslie Harlley Watter <leslieh@celepar.pr.gov.br>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:



(provide 'geral-config)

;; it just opens the place where I had the point
(use-package saveplace
  :init (save-place-mode)
  (setq save-place-ignore-files-regexp "\\(?:COMMIT_EDITMSG\\|.git/COMMIT_EDITMSG\\|hg-editor-[[:alnum:]]+\\.txt\\|svn-commit\\.tmp\\|bzr_log\\.[[:alnum:]]+\\)$")
  (setq save-place-save-skipped nil)
  (setq save-place-file "~/.emacs.d/saveplace")
  )

;; If anything multi-language should work, UTF-8 encoding is a must, so let’s make sure we try to use that everywhere
(prefer-coding-system 'utf-8)

;; para abrir pdfs com o acroread
(use-package openwith
  :ensure t
  :config
  ;; (setq openwith-associations '(("\\.pdf\\'" "acroread" (file))
  ;; 				("\\.ods\\'" "libreoffice" (file))
  ;; 				("\\.odt\\'" "libreoffice" (file))
  ;; 				("\\.mm\\'" "freeplane" (file))
  ;; 				("\\.mp4\\'" "vlc" (file))
  ;; 				("\\.drawio\\'" "drawio" (file))
  ;; 				("\\.xls?x\\'" "libreoffice" (file))
  ;; 				))
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv" "webm"
                  "ogm" "ogg" "mkv"))
               "mpv"
               '(file))
	 
	 (list (openwith-make-extension-regexp
                '("ods" "odt" "xls" "xlsx")) 
               "libreoffice"
               '(file))

	 (list (openwith-make-extension-regexp
                '("mm")) 
               "freeplane"
               '(file))
	 
	 (list (openwith-make-extension-regexp
                '("drawio")) 
               "drawio"
               '(file))
	 
         (list (openwith-make-extension-regexp
                '("pdf"))
               "acroread"
               '(file))))

  :config
  (openwith-mode t)
  )

;------------------------------------------------------------------------------
; fill mode

(log-message "fill mode ... ")

 (set-default 'fill-column 80)
 (set-default 'default-justification 'left)

(log-message "ok!\n")
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; change the annoying yes or no message to y or n

(log-message "substitui yes-or-no por y-or-n ... ")
(fset 'yes-or-no-p 'y-or-n-p)
(log-message "ok!\n")
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;;;---------->helper applications/diff<----------
;;; configuration for diff 

(log-message "diff configuration ... ")
(setq diff-switches "-b -t -u")
(setq compare-ignore-case t)
(log-message "ok!\n")



;------------------------------------------------------------------------------
(log-message "Misc configuration ... ")

;;;---------->helper applications/open file<----------
;;; configuration of find file 
(setq find-file-existing-other-name t)
(setq find-file-visit-truename t)

;;;---------->misc/bookmarks<----------
;;; configuration of book marks 
(setq bookmark-save-flag 1)

;;;---------->misc/completion<----------
;;; completion 
(setq completion-ignored-extensions (append
	(list ".ps" ".bak" ".o")
	(if (boundp 'completion-ignored-extensions)
		completion-ignored-extensions)))

;;;---------->misc/misc<----------
;;; misc 
(setq next-line-add-newlines nil)
(setq track-eol t)
(setq require-final-newline t)

;;; Indentation can insert tabs if this is non-nil. 
;; identa com espaços ao inves de tabs (nil ou t)
(setq-default indent-tabs-mode t)
(setq kill-whole-line t)

;;;---------->misc/tags<----------
;;; tags configuration 
;; (setq tags-file-name "TAGS")

(log-message "ok!\n")
;------------------------------------------------------------------------------


(log-message "Backup Configuration ...")

;;;---------->saving/backup<----------
;;; Configuration of backups 
(setq backup-by-copying-when-linked t)
(setq backup-by-copying-when-mismatch t)

(setq truncate-partial-width-windows t)

;; backup handling

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 )

;;;---------->saving/version control<----------
;;; This configures version control 
(setq vc-initial-comment t)

;; Cria versoes numeradas dos arquivos como backups ( com nome como nome~n~ )
(setq version-control t)

;; numero de versoes mais novas a serem mantidas no caso de criar 
;;versoes numeradas de backup

(setq kept-new-versions 2)

;; mantem as duas ultimas versoes
(setq kept-old-versions 2)

;; deleta as versoes em excesso

(setq delete-old-versions t)

;; Segundos de inatividade apos os quais o Emacs vai fazer o auto-save
(setq auto-save-timeout 30)
;; https://emacs.stackexchange.com/questions/18677/prevent-auto-save-list-directory-to-be-created
(setq auto-save-list-file-prefix nil)

(log-message "ok!\n")
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------

(log-message "Mouse Configuration ...")

;;;---------->terminals/mouse<----------
;;; mouse configuration 
(setq mouse-yank-at-point t)
(mwheel-install)                    ; though a mouse wheel is nice

(log-message "ok!\n")
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
(log-message "Configuração da Hora  ...")

(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

(log-message "ok!\n")
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
(log-message "Configuração do Desktop  ...")
; Start with the same buffers, major modes and buffer positions:
; You must do a M-x desktop-save the first time it's used. Emacs
; must be started in the same current directory.

(log-message "desktop \n")

(use-package desktop
  :init
  (setq desktop-auto-save-timeout 300)
  (setq desktop-dirname "~/.emacs.d/")
  (setq desktop-base-file-name "desktop")
  (setq desktop-files-not-to-save nil)
  (setq desktop-globals-to-clear nil)
  (setq desktop-load-locked-desktop t)
  (setq desktop-missing-file-warning t)
  (setq desktop-restore-eager 3)
  (setq desktop-save 'ask-if-new)
  (setq desktop-enable t)
  (setq history-length 250)
  :config
  (desktop-save-mode 1)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'planner-mode)
  )





(log-message "... revendo arquivo de sessão com session.el  ...")
;; (use-package session
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'session-initialize)
;; )

(log-message "ok!\n")
; ------------------------------------------------------------------------------


;------------------------------------------------------------------------------
(log-message "Abrir arquivos gzipados  ...")

;; Automatic opening of zipped files.
;; permite abrir arquivos .tar.{gz,bz2} abrindo um Dired (listagem diretórios)
;; e abrir os arquivos que estão dentro do compactado
(auto-compression-mode 1)
(log-message "ok!\n")
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
(log-message "Paren-mode  ...\n")

;; mostra os abre parenteses e fecha parenteses quando passa com o cursor
;; por sobre ele
;;
(use-package paren
  :config
  (show-paren-mode t) )

(use-package smartparens
  :ensure t
  :config
    (progn
      (show-smartparens-global-mode t))
    (add-hook 'shell-script-mode-hook #'smartparens-mode)
    (add-hook 'org-mode-hook #'smartparens-mode)
    ;; seleciona o texto e adiciona o * no inicio e final do texto selecionado
    (sp-with-modes '(org-mode)
      (sp-local-pair "*" "*")
;;      (sp-local-pair "/" "/")
      (sp-local-pair "=" "=")
      (sp-local-pair "_" "_")
      (sp-local-pair "~" "~")
;;      (sp-local-pair "<" ">")
      (sp-local-pair "[" "]")
      )
    )
  

(log-message "  Visible Bell  ...\n")
;; i hate my computer beeping at me unless something is really wrong.
(setq visible-bell t)

(log-message "  Interactive Buffer switching  ...")
;; interactive buffer switching
;; Removido na atualização para o emacs 22.2.1
(iswitchb-mode)
;; (iswitchb-default-keybindings)
(add-to-list 'iswitchb-buffer-ignore ":INFO$") ; ignore erc info buffers

;; substitui o iswitchb-mode
;;(ido-mode)

;; i'm no novice anymore
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(log-message "ok!\n")
; ------------------------------------------------------------------------------


;------------------------------------------------------------------------------
(log-message "Default tab width 4  ...")
; Tab length
(setq default-tab-width 4)
(log-message "ok!\n")
; ------------------------------------------------------------------------------

;------------------------------------------------------------------------------
(log-message "Flyspell-mode configuration  ...")

;; para o flyspell

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; the default flyspell behaviour
(put 'LaTeX-mode 'flyspell-mode-predicate 'tex-mode-flyspell-verify)
;; some extra flyspell delayed command
(setq flyspell-doublon-as-error-flag nil)

(log-message "ok!\n")
; ------------------------------------------------------------------------------

(log-message " TRAMP inicialization ...")
(use-package tramp
  :config
  (add-to-list 'tramp-default-method-alist '("" "leslie" "ssh"))
  (add-to-list 'tramp-default-method-alist
			   '("\\`localhost\\'" "\\`root\\'" "su"))
  (tramp-set-completion-function
   "ssh"
   '((tramp-parse-sconfig "/etc/ssh_config")
	 (tramp-parse-sconfig "~/.ssh/config")))
  )

(log-message "ok!\n")
; ------------------------------------------------------------------------------


(log-message " Newsticker ...")
(autoload 'newsticker-start "newsticker" "Emacs Newsticker" t)
(autoload 'newsticker-show-news "newsticker" "Emacs Newsticker" t)
(add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)
(log-message "ok\n")

;------------------------------------------------------------------------------

;; Configuração para mostrar imagens dentro do emacs
(log-message "Mostrar imagens dentro do Emacs ... ")
(use-package jka-compr
  :init
  (if (fboundp 'auto-compression-mode) (auto-compression-mode 1))
  (if (fboundp 'auto-image-file-mode)  (auto-image-file-mode 1))
  )

(log-message "ok!\n")

; ------------------------------------------------------------------------------
(log-message "Mudando a cor do cursor de acordo com o modo ...")

(setq hcz-set-cursor-color-color "") 
(setq hcz-set-cursor-color-buffer "") 
(defun hcz-set-cursor-color-according-to-mode () 
  "change cursor color according to some minor modes." 
  ;; set-cursor-color is somewhat costly, so we only call it when needed: 
  (let ((color 
		 (if buffer-read-only "black" 
		   (if overwrite-mode "red" 
			 "green")))) 
	(unless (and 
			 (string= color hcz-set-cursor-color-color) 
			 (string= (buffer-name) hcz-set-cursor-color-buffer)) 
	  (set-cursor-color (setq hcz-set-cursor-color-color color)) 
	  (setq hcz-set-cursor-color-buffer (buffer-name))))) 
(add-hook 'post-command-hook 'hcz-set-cursor-color-according-to-mode) 

(log-message "ok!\n")

;; útil, mas tenho que ver se consigo colocar para pegar de dicionarios online externos 
;; (log-message "Dictionary...") 
;; ;; pacote no debian  dictionary-el 
;; (use-package dictionary-init
;;   :bind (("\C-cs" .  dictionary-search)
;; 		 ("\C-cm" . dictionary-match-words))
;;   )

;; (log-message "ok!\n")

; --------------------------------------------------------------------------------
(log-message "Configuração de Transparência...")

(use-package cl
  :config
  ;; para iniciar com transparencia mude os valores abaixo de 100 100 para 85 50
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (add-to-list 'default-frame-alist '(alpha 100 100))
  
  (defun toggle-transparency ()
	(interactive)
	(if (/=
		 (cadr (find 'alpha (frame-parameters nil) :key #'car))
		 100)
		(set-frame-parameter nil 'alpha '(100 100))
	  (set-frame-parameter nil 'alpha '(85 60))))
  :bind ("C-c e" . toggle-transparency)
  )


(log-message "ok!\n")


;------------------------------------------------------------------------------

(log-message "Configuração de alarme audível...")
;; audible appt alarm
(setq appt-audible t)

(log-message "ok!\n")

;------------------------------------------------------------------------------

(log-message "Edit Server...")

(setq inhibit-startup-message t
      initial-scratch-message "")
(setq inhibit-default-init t)
;;; inicia o SERVER !!! TENHA o EMACS SEMPRE ABERTO

(use-package edit-server
  :ensure t
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t)
  :config
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient-snapshot") 
  )


(log-message "ok!\n")

;------------------------------------------------------------------------------

(log-message "Configuração de Tooltip...")

(tooltip-mode -1)
(setq tooltip-use-echo-area t)

(log-message "ok!\n")

;------------------------------------------------------------------------------



(log-message "Configuração do ESS + R...")

;; teste com R no emacs
;; git clone https://github.com/emacs-ess/ESS.git /path/to/ESS
;; cd /path/to/ESS/
;; make

;; http://orgmode.org/worg/org-faq.html#keeping-current-with-Org-mode-development
;;  This snippet in your .emacs will bind C-M-] and M-] to global and local cycling:
;; http://gongzhitaao.org/dotemacs/

(use-package ess-site
  :load-path "~/emacs/scm/ESS/lisp/"
  :defer t
  :mode ("\\.R\\'" . R-mode)
  :init
  (setq ess-history-directory "~/.R/")
  (setq ess-font-lock-mode t)
  (setq ess-ask-for-ess-directory nil)
  :config
  ;; We don’t want R evaluation to hang the editor, hence
  (setq ess-eval-visibly 'nowait)
  (setq ess-R-font-lock-keywords
          '((ess-R-fl-keyword:modifiers . t)
            (ess-R-fl-keyword:fun-defs . t)
            (ess-R-fl-keyword:keywords . t)
            (ess-R-fl-keyword:assign-ops . t)
            (ess-R-fl-keyword:constants . t)
            (ess-fl-keyword:fun-calls)
            (ess-fl-keyword:numbers)
            (ess-fl-keyword:operators)
            (ess-fl-keyword:delimiters)
            (ess-fl-keyword:=)
            (ess-R-fl-keyword:F&T . t)
            (ess-R-fl-keyword:%op% . t)))

  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls)
          (ess-fl-keyword:numbers)
          (ess-fl-keyword:operators)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T . t)))

  (defun my-ess-init ()
    "Init my ess mode."
    (setq ess-help-own-frame 'one)
    (setq ess-tab-complete-in-script t)
    (setq ess-first-tab-never-complete
          'symbol-or-paren-or-punct))

  (add-hook 'ess-mode-hook #'my-ess-init)
  (add-hook 'inferior-ess-mode-hook #'turn-on-smartparens-mode)
  )


;;  (load "ess-site")

(log-message "ok!\n")

;------------------------------------------------------------------------------

(log-message "Evita nomes com <2>...")

;; para mudar os nomes duplicados
;; https://www.emacswiki.org/emacs/uniquify
(use-package uniquify)

(log-message "ok!\n")
;------------------------------------------------------------------------------


(log-message "Guide Key...")

;; guide-key
(use-package guide-key
  :ensure t
  :init
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
  (setq guide-key/popup-window-position (quote bottom))
  :config
  (guide-key-mode 1)  ; Enable guide-key-mode

  (defun guide-key/my-hook-function-for-org-mode ()
	(guide-key/add-local-guide-key-sequence "C-c")
	(guide-key/add-local-guide-key-sequence "C-c C-x")
	(guide-key/add-local-highlight-command-regexp "org-"))
  (add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)
  )

;; guide key tip
;; (require 'guide-key-tip)
;; (setq guide-key-tip/enabled t)
(log-message "ok!\n")
;------------------------------------------------------------------------------




(log-message "SMEX...")
;; instalando o smex http://github.com/nonsequitur/smex/blob/master/smex.el?raw=true

;;(add-to-list 'load-path "~/emacs/scm/smex/")

(use-package smex
  :load-path "~/emacs/scm/smex/"
  :init
  (bind-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

  (bind-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))
  )

(log-message "ok!\n")
;------------------------------------------------------------------------------


(log-message "Browser configuration ... ")
(setq browse-url-browser-function 'browse-url-chrome
          browse-url-new-window-flag  t)

;; (setq browse-url-browser-function 'browse-url-firefox 
;;          browse-url-firefox-new-window-is-tab t)
;; (setq browse-url-generic-program "/usr/bin/firefox ")
(log-message "ok!\n")
;------------------------------------------------------------------------------


(log-message "Mutt Dentro do Emacs...")
(defun mutt ()
  "Insere uma entidade entre os simbolos application no doctype."
  (interactive)
  (ansi-term "/usr/bin/mutt" "MUTT"))
  ;; (pop-to-buffer (find-file-noselect "~/.muttrc"))
  ;; (goto-char (point-min))
  ;; (search-forward "#set editor=emacsclient" nil t) 
  ;; (replace-match "set editor=emacsclient")
  ;; (save-buffer)
  ;; (switch-to-buffer (find-file-noselect "~/.muttrc"))
  ;; (goto-char (point-min))
  ;; (search-forward "set editor=emacsclient" nil t) 
  ;; (replace-match "#set editor=emacsclient")
  ;; (save-buffer)
(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
(setq mail-header-separator "")
(add-hook 'message-mode-hook 'auto-fill-mode)
(define-key message-mode-map (kbd "C-c C-c")  #'(lambda ()
                                                 "save and exit quickly"
                                                 (interactive)
                                                 (save-buffer)
                                                 (server-edit)))
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
;; para usar o mail mode com um agente definido
;;(setq mail-user-agent 'sendmail-user-agent)
(setq compose-mail-user-agent-warnings nil)
(log-message "ok!\n")



(log-message "AucTex ... ")
(use-package tex-site
  :load-path "/home/leslie/.emacs.d/elpa/auctex-12.3.1/"
  :config
  (load "auctex-autoloads.el" nil t t)
  (load "preview.el" nil t t)
  ;; usa o pdflatex
  (setq TeX-PDF-mode t)
  )
  
(log-message "ok!\n")

(log-message "Golden ratio ... ")
;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :if window-system
  :init
  (if (> (display-pixel-width) 3000)
      (golden-ratio-mode 0)
    (golden-ratio-mode 1)
  )
  :config
  (setq golden-ratio-auto-scale t)
  )
(log-message "ok!\n")


(log-message "Atomic Chrome para editar texto no emacs ... ")
;; https://github.com/alpha22jp/atomic-chrome/blob/master/atomic-chrome.el
;; extensões para firefox e chrome
;; https://github.com/GhostText/GhostText
;; https://github.com/rememberYou/.emacs.d/blob/master/config.org/#atomic-chrome
(use-package atomic-chrome
  :defer 2
  :hook (atomic-chrome-edit-mode . flyspell-mode)
  :init
  (defun atomic-chrome-server-running-p ()
    (cond ((executable-find "lsof")
           (zerop (call-process "lsof" nil nil nil "-i" ":64292")))
          ((executable-find "netstat") ; Windows
           (zerop (call-process-shell-command "netstat -aon | grep 64292")))))

  (if (atomic-chrome-server-running-p)
      (message "Can't start atomic-chrome server, because port 64292 is already used")
    (atomic-chrome-start-server)))



(log-message "usando ibuffer para separar lista de buffers ... ")
;; https://github.com/rememberYou/.emacs.d/blob/master/config.org
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-saved-filter-groups
        (quote (("default"
		 ("Archive" (name . "^.*org_archive$"))
		 ("Agenda" (or (mode . org-agenda-mode)
                               (mode . diary-mode)
                               (predicate . (my-org-agenda-filter))))
                 ("Dired" (mode . dired-mode))
                 ("Org" (name . "^.*\\.org$"))
                 ("Images" (or (name . "^.*png$")  (name . "^.*jpg$") ))
                 ("Web" (or (name . "^.*css$") (name . "^.*html$") (mode . web-mode) (mode . js2-mode)))
                 ("Shell" (or (name . "^.*sh$") (mode . eshell-mode) (mode . shell-mode)))
                 ("Journal" (or (name . "^2020.*$") ))
                 ("Programming" (or
                                 (mode . python-mode)))
                 ("Elisp" (or
                           (name . "^.*el$")
			   ))
		 ("LaTeX" (or (name . "^.*tex$") ))
		 ("TXT" (or (name . "^.*txt$") ))
		 ("Trello" (or (name . "^.*trello$") ))
		 ("tramp" (name . "^\\*tramp.*"))
		 ("Emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*ESS\\*$")
                           (name . "^\\*Compile-Log\\*$")
                           (name . "^\\*Backtrace\\*$")
                           (name . "^\\*Completions\\*$")
                           (name . "^\\*Org PDF LaTeX Output\\*$")
                           (name . "^\\*Calculator\\*$")
                           (name . "^\\*Calc Trail\\*$")
                           (name . "^\\*Calendar\\*$")
                           (name . "^\\*Messages\\*$")
                           (name . "^\\*log\\*$")
                           (name . "^\\*vc\\*$")
                           (name . "^\\*Fancy Diary Entries\\*$")
                           (name . "^\\*git-gutter:diff\\*$")
                           (name . "^\\*Warnings\\*$")
			   )
		  )
		 ("Magit" (or (name . "^magit.*$") (mode . "Magit Process") (mode . "Magit")))
                 ))))

  (defun my-org-agenda-filter ()
  (let ((fname (buffer-file-name)))
    (and fname
         (member (file-truename fname)
                 (mapcar 'file-truename (org-agenda-files))))))

  (setq ibuffer-show-empty-filter-groups nil)

  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-switch-to-saved-filter-groups "default")))

  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))
  
  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
	'((mark modified read-only " "
		(name 18 18 :left :elide)
		" "
		(size-h 9 -1 :right)
		" "
		(mode 16 16 :left :elide)
		" "
		filename-and-process)))
  
  )


;; history
;; https://github.com/rememberYou/.emacs.d/blob/master/config.org/#history
 
(setq history-delete-duplicates t)
(setq history-length t)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))
(setq savehist-file (expand-file-name "history" user-emacs-directory))
(setq savehist-save-minibuffer-history 1)
(savehist-mode 1)


;; busca a partir do emacs
;; selecione o texto e C-x / + a tecla e ele abre o navegador no mecanismo de busca
;; 

(use-package engine-mode
  :defer 10
  :ensure t
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine acoes-statusinvest
    "https://statusinvest.com.br/acoes/%s"
    :keybinding "A")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine fiis-statusinvest
    "https://statusinvest.com.br/fundos-imobiliarios/%s"
    :keybinding "f")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s"
    :keybinding "i")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :keybinding "m"
    :docstring "Mappin' it up.")

  (defengine google
    "https://www.google.com/search?q=%s"
    :keybinding "o")

  (defengine google-translate
    "https://translate.google.com/#view=home&op=translate&sl=auto&tl=pt&text=%s"
    :keybinding "r")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine 17trac
    "https://t.17track.net/en#nums=%s"
    :keybinding "t")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")
  (engine-mode t))


;; habilitando o winner mode para salvar o estado das janelas
;; C-c <- e C-c -> (seta esquerda e seta direita) para desfazer/refazer a mudança nos paineis
;; https://github.com/cyrus-and/dotfiles/tree/master/emacs/.emacs.d#winner

(custom-set-variables
 '(winner-mode t))

;; http://emacsrocks.com/e15.html
(use-package restclient)

;; novo estilo de bookmarks
;; https://github.com/joodland/bm
(use-package bm
         :ensure t
         :demand t

         :init
         ;; restore on load (even before you require bm)
         (setq bm-restore-repository-on-load t)


         :config
         ;; Allow cross-buffer 'next'
         (setq bm-cycle-all-buffers t)

         ;; where to store persistant files
         (setq bm-repository-file "~/.emacs.d/bm-repository")

         ;; save bookmarks
         (setq-default bm-buffer-persistence t)

         ;; Loading the repository from file when on start up.
         (add-hook' after-init-hook 'bm-repository-load)

         ;; Restoring bookmarks when on file find.
         (add-hook 'find-file-hooks 'bm-buffer-restore)

         ;; Saving bookmarks
         (add-hook 'kill-buffer-hook #'bm-buffer-save)

         ;; Saving the repository to file when on exit.
         ;; kill-buffer-hook is not called when Emacs is killed, so we
         ;; must save all bookmarks first.
         (add-hook 'kill-emacs-hook #'(lambda nil
                                          (bm-buffer-save-all)
                                          (bm-repository-save)))

         ;; The `after-save-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state.
         (add-hook 'after-save-hook #'bm-buffer-save)

         ;; Restoring bookmarks
         (add-hook 'find-file-hooks   #'bm-buffer-restore)
         (add-hook 'after-revert-hook #'bm-buffer-restore)

         ;; The `after-revert-hook' is not necessary to use to achieve persistence,
         ;; but it makes the bookmark data in repository more in sync with the file
         ;; state. This hook might cause trouble when using packages
         ;; that automatically reverts the buffer (like vc after a check-in).
         ;; This can easily be avoided if the package provides a hook that is
         ;; called before the buffer is reverted (like `vc-before-checkin-hook').
         ;; Then new bookmarks can be saved before the buffer is reverted.
         ;; Make sure bookmarks is saved before check-in (and revert-buffer)
         (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


	 ;; left-fringe == margem esquerda
	 ;; mouse 1 == botão esquerdo
	 ;; mouse 4 e 5 == roda pra frente / trás
         :bind (("<f8>" . bm-next)
                ("S-<f8>" . bm-previous)
                ("C-<f8>" . bm-toggle)
		("<left-fringe> <mouse-5>" . bm-next-mouse)
		("<left-fringe> <mouse-4>" . bm-previous-mouse)
		("<left-fringe> <mouse-1>" . bm-toggle-mouse)
		)
         )


;; instalar a partir do melpa-stable
;; https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; para pegar a previsão do tempo

(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Saarbrücken" "Curitiba" "Paranagua")))

(setq wttrin-default-accept-language '("Accept-Language" . "pt-BR"))


;; fontes para os delimitadores (parênteses)
;; [[https://ericscrivner.me/2015/06/better-emacs-rainbow-delimiters-color-scheme/][Better Emacs Rainbow Delimiters Color Scheme - Eric Scrivner]]
(custom-set-faces
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))

;; instalando o screenshot de dentro do emacs
;; https://github.com/tecosaur/screenshot
(add-to-list 'load-path "~/emacs/scm/screenshot/")
(eval-when-compile
  (require 'screenshot))


;; conectar dispositivos bluetooth
(log-message "BlueTooth Devices ... ")


(defun lhw/bluetooth-connect-hbs730 ()
    (interactive)
    (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect FC:58:FA:A3:24:90"))

(defun lhw/bluetooth-connect-qc35 ()
    (interactive)
    (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 4C:87:5D:51:43:33"))

(defun lhw/bluetooth-connect-bta100 ()
    (interactive)
    (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 41:42:BB:6D:78:B1"))

(log-message "ok!\n")


;; Auto reverting changed files

;; Revert Dired and other buffers
  (setq global-auto-revert-non-file-buffers t)

  ;; Revert buffers when the underlying file has changed
  (global-auto-revert-mode 1)


;; =display-time-world= command provides a nice display of the time at a specified list of timezones.  Nice for working in a team with remote members.

  (setq display-time-world-list
    '(("Etc/UTC" "UTC")
      ("America/Los_Angeles" "San Francisco")
      ("America/New_York" "Boston")
      ("Europe/Berlin" "Berlin")
      ("America/Sao_Paulo" "São Paulo")
      ("Pacific/Auckland" "Auckland")
      ))
  (setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")



;; avaliar

;; exemplo de uso
;; (dw/org-file-jump-to-heading "~/org/celepar.org" "Curl line")

  (defun dw/org-file-jump-to-heading (org-file heading-title)
    (interactive)
    (find-file (expand-file-name org-file))
    (goto-char (point-min))
    (search-forward (concat "* " heading-title))
    (org-overview)
    (org-reveal)
    (org-show-subtree)
    (forward-line))


;; https://marcohassan.github.io/bits-of-experience/pages/emacs/
;; Fix the scrolling C-v and M-v such that the cursor will return to the previous scrolling position if going down and up.
   (setq scroll-conservatively 10000
	scroll-preserve-screen-position t)


;; Now pressing M-. on an identifier should open a buffer at the place
;; where it is defined, or a list of candidates if uncertain. This
;; list can be navigated using M-g M-n (next-error) and M-g M-p
;; (previous-error).
;; To enable Dumb Jump, add the following to your initialisation file:
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


;; auto-save changed files
;; comentado ate encontrar como fazer o org-crypt rodar antes
;; (use-package super-save
;;   :ensure t
;;   :diminish super-save-mode
;;   :config
;;   (super-save-mode +1)
;;   (setq super-save-auto-save-when-idle t)
;;   )

;; https://systemcrafters.cc/emacs-tips/streamline-completions-with-vertico/
  (use-package vertico
    :ensure t
    ;; :bind (:map vertico-map
    ;; 	 ("C-j" . vertico-next)
    ;; 	 ("C-k" . vertico-previous)
    ;; 	 ("C-f" . vertico-exit)
    ;; 	 :map minibuffer-local-map
    ;; 	 ("M-h" . backward-kill-word))
    :custom
    (vertico-cycle t)
    :init
    (vertico-mode))

  (use-package marginalia
    :after vertico
    :ensure t
    :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
    :init
    (marginalia-mode))



;; pacote para controlar horas trabalhadas

;; (use-package org-arbeitszeit
;;   :ensure t
;;   :config
;;   ;; You work 5 days per week...
;;   (setq org-arbeitszeit-days-per-week 5
;; 	;; ... but 8 hours on each day.
;; 	org-arbeitszeit-hours-per-day 8
;; 	;; Don't include worked clocked on headlines that are tagged
;; 	;; with break, nonwork or private
;; 	org-arbeitszeit-match "-break-nonwork-private")
;;   )

;; exemplo de tabela em https://raw.githubusercontent.com/bkaestner/org-arbeitszeit/5b307ebc0db0d58ed6b94d4db30f2653b5782e41/examples/Simple.org
;; #+BEGIN: arbeitszeit :tstart "2022-01-03" :tend "2022-01-10"
;; | Week     | Hours | +Time |
;; |----------+-------+-------|
;; | 2022-W01 | 47:25 | 07:25 |
;; |----------+-------+-------|
;; | Total:   | 47:25 | 07:25 |
;; #+TBLFM: $3=$2-144000;U::@>$2..@>$>=vsum(@I..@II);U
;; #+END:


;; função para, caso o buffer tenha sido modificado e seja morto, oferecer um diff na saída
;; fonte https://www.reddit.com/r/emacs/comments/13b2z5z/add_a_diff_to_the_yesnosave_and_quit_choices/

(defun my-ask-kill-buffer ()
     "Ask to diff, save or kill buffer"
     (if (and (buffer-file-name) (buffer-modified-p))
	 (cl-loop for ch = (read-event "(K)ill buffer, (D)iff buffer, (S)ave buffer, (N)othing?")
		  if (or (eq ch ?k) (eq ch ?K))
		  return t
		  if (or (eq ch ?d) (eq ch ?D))
		  do (diff-buffer-with-file)
		  if (or (eq ch ?s) (eq ch ?S))
		  return (progn (save-buffer) t)
		  if (or (eq ch ?n) (eq ch ?N))
		  return nil)
       t))
(add-to-list 'kill-buffer-query-functions #'my-ask-kill-buffer)



;; https://github.com/rnadler/password-menu
(use-package password-menu
  :ensure t)

(global-set-key (kbd "C-x j") 'password-menu-transient)
(global-set-key (kbd "C-x J") 'password-menu-completing-read)

(use-package calc
  :defer t)

(use-package casual-calc
  :ensure nil
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu))
  :after (calc))
