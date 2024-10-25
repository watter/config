;;; orgmode-config.el --- configuração do org-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Leslie Harlley Watter

;; Atenção: Essas configurações foram adicionadas ao longo de vários anos, use como referência para criar a sua config ;-)

;; Author: Leslie Harlley Watter <leslie@watter.net>
;; Keywords: 

(provide 'orgmode-config)

(log-message "Orgmode config -- ")

;; load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (use-package org)
      (use-package org-install)
      (use-package ob-tangle)
)))

(setq org-odt-data-dir "/home/leslie/emacs/scm/org-mode/etc")


(use-package org
;; The following lines are always needed.  Choose your own keys.
;;		 ("\C-cb" . org-ido-switchb)
  :ensure htmlize                       ; For org-publish
  :mode ("\\.org\\'" . org-mode)
  :bind (
		 ("\C-cl" . org-store-link)
		 ("\C-ca" . org-agenda)
		 ("\C-cb" . org-iswitchb)
		 ("C-c d" . org-decrypt-entry)
		 )
  )
(use-package org-install)
(use-package ob-tangle)
(use-package ob-R)
(use-package ob-ledger)
(use-package ob-shell)
(use-package ob-sql)
;; https://github.com/astahlman/ob-async  -- permite executar o bloco de forma asincrona
(use-package ob-async
    :ensure t
)

(use-package ob-ditaa
  :config
  (setq org-ditaa-jar-path (expand-file-name  "~/.emacs.d/ditaa/ditaa.jar"))
  )
(use-package ob-plantuml)

(use-package ox)         
(use-package ox-odt)         
(use-package ox-beamer)
(use-package ox-texinfo)
(use-package ox-html)
(use-package ox-latex)
(use-package ox-man)
(use-package ox-md)
(use-package ox-publish)
(use-package ox-ascii)
(use-package ox-org)
(use-package ox-icalendar)
(use-package ox-reveal)

;; org-cliplink lets you insert a link from your clipboard with a title that is fetched from the page's metadata.
(use-package org-cliplink
  :ensure t
  :bind ("C-x p i" . org-cliplink))


;; https://github.com/jkitchin/ox-clip
;; copia a área do org-mode formatada para a área de transferência
;; https://github.com/mwfogleman/.emacs.d/blob/master/michael.org#formatted-copy-commands-for-org-mode
(use-package htmlize
  :after org
  :ensure t)
(use-package ox-clip
  :after org
  :ensure t
  :config
  (defun ox-clip-dwim ()
    "If the region is active, call ox-clip as normal. Otherwise, call ox-clip on whole buffer (or visible / narrowed section, if applicable)."
    (interactive)
    (if (region-active-p)
        (ox-clip-formatted-copy (region-beginning) (region-end))
      ;; if buffer is narrowed, this will work on visible; if not, it will capture whole buffer
      (ox-clip-formatted-copy (point-min) (point-max))))
  (bind-keys ("C-c x" . ox-clip-dwim)))



;;
(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

;;;; Drag And Drop
(use-package org-download
  :ensure t
  :bind (:map org-mode-map
              ("C-c i" . org-download-screenshot))
  :hook ((org-mode dired-mode) . org-download-enable)
  :init
  (setq-default org-download-screenshot-method "gnome-screenshot -d 3 -f %s")
)


;; coloca o gráfico na coluna 60
(setq org-habit-graph-column 60)

(require 'org-habit nil t)

;; (add-to-list 'load-path "~/scm/org-mode/lisp/")
;; (require 'org-mobile)

(defun org-add-my-extra-fonts ()
  "Add alert and overdue fonts."
  (add-to-list 'org-font-lock-extra-keywords '("\\(!\\)\\([^\n\r\t]+\\)\\(!\\)" (1 '(face org-habit-alert-face invisible t)) (2 'org-habit-alert-face t) (3 '(face org-habit-alert-face invisible t))) t)
  (add-to-list 'org-font-lock-extra-keywords '("\\(%\\)\\([^\n\r\t]+\\)\\(%\\)" (1 '(face org-habit-overdue-face invisible t)) (2 'org-habit-overdue-face t) (3 '(face org-habit-overdue-face invisible t))) t))

;;(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-fonts)
(add-hook 'org-finalize-agenda-hook #'org-add-my-extra-fonts)

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h) ("hábito" . ?H)
                      (:endgroup . nil)
		      (:startgroup . nil)
                      ("export" . ?e) ("noexport" . ?n)
                      (:endgroup . nil)
                      ("crypt" . ?c) ("reunião" . ?r)
		      )
		      )

;; https://github.com/james-stoup/emacs-org-mode-tutorial?tab=readme-ov-file#colorizing-tags
;; Tag colors
(setq org-tag-faces
      '(
        ("planning"   . (:foreground "mediumPurple1" :weight bold))
        ("pendência"  . (:foreground "yellow1"       :weight bold))
        ("openshift"  . (:foreground "red"           :weight bold))
        ("url"        . (:foreground "royalblue1"    :weight bold))
        ("abra"       . (:foreground "forest green"  :weight bold))
        ("hábito"     . (:foreground "green"         :weight bold))
        ("meeting"    . (:foreground "yellow1"       :weight bold))
        ("ATTACH"     . (:foreground "SlateBlue"     :weight bold))
        ("wiki"       . (:foreground "DarkViolet"    :weight bold))
        )
      )



;; para usar com o C-c C-j para ir para um headline
;; https://stackoverflow.com/questions/15011703/is-there-an-emacs-org-mode-command-to-jump-to-an-org-heading
(setq org-goto-interface 'outline-path-completion
      org-goto-max-level 10)
(setq org-outline-path-complete-in-steps nil)

;;(setq org-export-odt-styles-file "~/emacs/OrgOdtStyles.xml")
;;(setq org-export-odt-content-template-file "~/emacs/OrgOdtContentTemplate.xml")

;; não coloca o grid na agenda
(setq org-agenda-use-time-grid nil)
(setq org-agenda-start-on-weekday nil) ;; start on current day
(setq org-agenda-tags-column 95)

;; default
;;(setq org-agenda-window-setup 'reorganize-frame)
;; só uma janela
(setq org-agenda-window-setup 'only-frame)
;; M-x toggle-frame-tab-bar . This command allows to enable the display of the Tab Bar on some frames and disable it on others, regardless of the values of tab-bar-mode and tab-bar-show
;; um tab específico
;;(setq org-agenda-window-setup 'other-tab)


;; cores das prioridades
 (setq org-priority-faces
       '((65 :foreground "black" :background "red" )
	 (66 :foreground "yellow" :background "blue")
	 (67 :foreground "purple" :background "yellow")
	 )
 )


;; Agenda View "d"
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

  PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))



(setq org-agenda-custom-commands
      '(
;;;
	("c" "Empresa agenda"
	 (
	 (agenda ""
	 ((org-agenda-overriding-header "Agenda na Empresa")
	  (org-agenda-span 1)
	  (org-agenda-files
           (quote
            (
	     "~/org/empresa.org"
	     "~/org/datam.org"
	     "~/org/meetings.org"
	     "~/org/calendars/sogo.org"
	     )
	    ))
	  )
	 )
	 (tags-todo "pendência" ((org-agenda-files '("~/org/empresa.org" "~/org/datam.org" "~/org/meetings.org"))
                     (org-agenda-overriding-header "Tarefas com tag de Pendências"))
	       )
	 (todo "WAITING" ((org-agenda-files '("~/org/empresa.org" "~/org/datam.org" "~/org/meetings.org"))
                     (org-agenda-overriding-header "Waiting TODO"))
	       )
	 (todo "TODO"
	       ((org-agenda-files '("~/org/empresa.org" "~/org/datam.org" "~/org/meetings.org"))
		(org-agenda-overriding-header "In progress")
		(org-agenda-skip-function
		 '(org-agenda-skip-entry-if 'scheduled
					    'notregexp "CLOCK: \\["))))
	 (todo ".*" ((org-agenda-files '("~/org/inbox.org"))
                     (org-agenda-overriding-header "Unprocessed Inbox Items"))
	 )

	 )
	 )


	("l" "Leslie agenda" agenda ""
	 ((org-agenda-overriding-header "Agenda do Leslie")
	  (org-agenda-files (quote
	     ("~/org/TODO.org"
	      "~/org/cursos/cursos.org"
	      "~/org/freelas/freelancers.org"
	      )))
          (org-agenda-compact-blocks t)
	  ))

	("E" "estudos agenda" agenda ""
	 ((org-agenda-overriding-header "Agenda de Estudos")
	  (org-agenda-files
           (quote
            ("~/org/estudos.org" )))
	  ))

	("F" "Freelancers" agenda ""
	 ((org-agenda-overriding-header "Agenda de Freelancers")
	  (org-agenda-files (quote
	     ("~/org/freelas/freelancers.org"
	      )))
	  ))


	;; https://github.com/james-stoup/emacs-org-mode-tutorial?tab=readme-ov-file#agenda-custom-command
        ;; Daily Agenda & TODOs
        ("d" "Daily agenda and all TODOs"

         ;; Display items with priority A
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(or (org-agenda-skip-entry-if 'todo 'done)
						(org-agenda-skip-entry-if 'scheduled))
		 )
                 (org-agenda-overriding-header "High-priority unfinished unscheduled tasks:")))

          ;; View 7 days in the calendar view
          (agenda "" ((org-agenda-span 7)))

          ;; Display items with priority B (really it is view all items minus A & C)
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-priority ?A)
                                                   (air-org-skip-subtree-if-priority ?C)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:")))

          ;; Display items with pirority C
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority Unfinished tasks:")))
          )

         ;; Don't compress things (change to suite your tastes)
         (
	  (org-agenda-files (quote
	     ("~/org/TODO.org"
	      "~/org/empresa.org"
	      "~/org/datam.org"
              "~/org/calendars/sogo.org"
	      )))

	  (org-agenda-compact-blocks nil)))
	 
	
;;;
	("D" "Upcoming deadlines" agenda "" 
	 ((org-agenda-time-grid nil)
	  (org-deadline-warning-days 365)        ;; [1]
	  (org-agenda-entry-types '(:deadline))  ;; [2]
	  ))
;;;	
	("f" "Scheduled tasks" tags "TODO<>\"\"&TODO<>{INFO\\|APPT\\|DONE\\|CANCELLED\\|NOTE\\|PROJECT}&STYLE<>\"habit\""
	 ((org-agenda-overriding-header "Scheduled tasks: ")
	  (org-agenda-skip-function
           (quote
            (org-agenda-skip-entry-if
             (quote notscheduled))))
	  (org-agenda-sorting-strategy
           (quote
            (category-up)))))
;;;
	
	("u" "Leslie's Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|INFO\\|CANCELED\\|NOTE\\|PROJECT}"
	 ((org-agenda-overriding-header "Tarefas sem agendamento do Leslie")
	  (org-agenda-skip-function
           (quote
            (org-agenda-skip-entry-if
             (quote scheduled)
             (quote regexp)
             "\\* \\(DEFERRED\\|SOMEDAY\\)")))
	  (org-agenda-sorting-strategy
           (quote
            (user-defined-up)))
	  (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
	  (org-agenda-files
           (quote
            ("~/org/TODO.org" "~/org/cursos/cursos.org" )))))

;;;

	("U" "Untagged Tasks" tags-todo "-{.*}"
	  ((org-agenda-overriding-header "Tarefas sem Tags")) )

	
;;;
	("p" "Projetos" tags "avaliar\\|broker\\|git"
	 ((org-agenda-overriding-header "Projetos")))
;;;; 
	("P" "Printed agenda"
         ((agenda "" ((org-agenda-span 7)                      ;; overview of appointments
                      (org-agenda-start-on-weekday nil)         ;; calendar begins today
                      (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-entry-types '(:timestamp :sexp))))
          (agenda "" ((org-agenda-span 1)                      ; daily agenda
                      (org-deadline-warning-days 7)            ; 7 day advanced warning for deadlines
                      (org-agenda-todo-keyword-format "[ ]")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))
          (todo "TODO"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
         ((org-agenda-with-colors t)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
           (ps-landscape-mode t))
         ("~/agenda.ps")
	 )

;;
        ("q" "Archive search" search ""
         ((org-agenda-files (file-expand-wildcards "~/org/archive/*.org*")))) 

;;	
	("r" "Calendar" agenda ""
         ((org-agenda-ndays 7)
          (org-agenda-start-on-weekday 0)
          (org-agenda-time-grid nil)
          (org-agenda-repeating-timestamp-show-all t)
          (org-agenda-entry-types '(:timestamp :sexp)))) 

;;;
	("O" "All TODOs" tags "TODO<>\"\""
	 ((org-agenda-overriding-header "All TODOs")
	  ;;	  (org-agenda-files (directory-files-recursively "~/org/" "\.org$"))
	  (org-agenda-files
           (quote
            ("~/org/russo.org" "~/org/empresa.org" "~/org/datam.org" "~/org/TODO.org" "~/org/projetos/fotografias.org" "~/org/exercicios.org"    "~/org/freelas/freelancers.org")
	    ))
	  ))

;;;
	("W" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
          ;; type "l" in the agenda to review logged items 
          (stuck "") ;; review stuck projects as designated by org-stuck-projects
          (todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
          (todo "MAYBE") ;; review someday/maybe items
          (todo "WAITING"))) ;; review waiting items 

;;;
	("w" "Unscheduled WORK-related tasks" tags "TODO<>\"\"&TODO<>{DONE\\|INFO\\|CANCELED\\|NOTE\\|PROJECT}"
	 ((org-agenda-overriding-header "Unscheduled work-related tasks")
	  (org-agenda-files
           (quote
            ("~/org/inbox.org" "~/org/empresa.org" "~/org/datam.org" "~/org/from-mobile.org")))
	  (org-agenda-sorting-strategy
           (quote
            (category-down user-defined-up)))
	  (org-agenda-skip-function
           (quote
            (org-agenda-skip-entry-if
             (quote scheduled)
             (quote deadline)
             (quote timestamp))))
	  (org-agenda-prefix-format "%-11c%5(org-todo-age) ")))
;;;
	("x" "Agenda e todo para HOJE"
         (
	  (agenda "" (
	  (org-agenda-files
           (quote
            ("~/org/TODO.org" "~/org/empresa.org" "~/org/datam.org" "~/org/meetings.org")
	    ))
	  (org-agenda-sorting-strategy
           (quote
            (habit-up)))
	  (org-agenda-span 1) (org-agenda-ndays 1))
	  )
	   (alltodo)
	   )
	 )
	)
      )
      
      


;; muda a fonte de itens com prioridade
(setq org-agenda-fontify-priorities t)

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-capture)

;; evita que na edição de códigos com C-c ' ele idente o código.
;; útil para evitar os 2 espaços em branco antes de scripts shell
(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

;; define os estados do todo
;; @ precisa de nota quando entra no estado
;; ! marca com timestamp quando mudou de estado 
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "DarkViolet" :weight bold))
        ("MAYBE" . (:foreground "sea green"))
        ("DONE" . (:foreground "light sea green"))
        ("CANCELLED" . (:foreground "forest green"))
        ("WAITING" . (:foreground "DeepSkyBlue"))
        ("WISHLIST" . (:foreground "gold"))
        ("INFO" . (:foreground "DodgerBlue"))
        ("REUNIÃO" . (:foreground "OrangeRed"))
        ("PRODUTO" . (:foreground "SandyBrown"))
        ("IMPORTANTE" . (:foreground "Gold"))
        ("REVIEW" . (:foreground "LawnGreen"))
        ("TASK" . (:foreground "magenta"))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)" "DEFERRED(f@)")
        (sequence "TASK(f)" "|" "DONE(d!)")
        (sequence "INFO(i)" "|" )
        (sequence "REUNIÃO(r)" "PRODUTO(p)" "|" "DONE(d!)")
        (sequence "IMPORTANTE(I)" "|" "DONE(d!)")	
        (sequence "APPT(a)" "|" "DONE(d!)")
        (sequence "REVIEW(v)" "|" "DONE(d!)")
        (sequence "MAYBE(m)" "|" "CANCELLED(c@)")))


;; mudando face dos itens já agendados
;; https://emacs.stackexchange.com/a/74908
(set-face-foreground 'org-scheduled-previously "HotPink")

;; Resolve open clocks if the user if idle more than 10 minutes.
(setq org-clock-idle-time 15)

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file in org directory - up to 5 levels deep
;;(setq org-refile-files (directory-files-recursively "~/org/" "\\.org$"))


(setq org-refile-files
      '(
       "~/org/inbox.org"
       "~/org/TODO.org"
       "~/org/exercicios.org")
      )

(setq org-refile-targets (quote ((org-refile-files :maxlevel . 5) (nil :maxlevel . 5))))
(advice-add 'org-refile :after 'org-save-all-org-buffers)

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Save Org buffers after refiling

(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; faz com que o timestamp seja colocado quando mudando pra done
(setq org-log-done 'time)



;; Funções para Agenda

; Use appointment data from org-mode
;; (defun my-org-agenda-to-appt ()
;;   (interactive)
;;   (setq appt-time-msg-list nil)
;;   (org-agenda-to-appt))

;; Get appointments for today
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (let ((org-deadline-warning-days 0)
	;; aqui defino quais arquivos serão usados para criar os avisos de popup de agenda
	(org-agenda-files
	 (quote
	  ("~/org/TODO.org"
	   
	   "~/org/cursos/cursos.org"
	   "~/org/cursos/git/cronograma.org"
	   "~/org/empresa.org"
	   "~/org/calendars/sogo.org"
	   "~/org/datam.org")))
	)
    (org-agenda-to-appt)))

(defun my-appt-display (min-to-app new-time msg)
  (if (atom min-to-app)
	  (start-process "my-appt-notification-app" nil my-appt-notification-app min-to-app msg)
	(dolist (i (number-sequence 0 (1- (length min-to-app))))
	  (start-process "my-appt-notification-app" nil my-appt-notification-app (nth i min-to-app) (nth i msg)))))

(use-package appt
  :config
  (appt-activate t)

  (setq appt-message-warning-time 5) ; Show notification 5 minutes before event
  (setq appt-display-interval appt-message-warning-time) ; Disable multiple reminders
  (setq appt-display-mode-line nil)

; Update alarms when...
; (1) ... Starting Emacs
  (my-org-agenda-to-appt)

; (2) ... Everyday at 12:05am (useful in case you keep Emacs always on)
  (run-at-time "12:01" 5400 'my-org-agenda-to-appt) ;; update agenda every 90 minutes

;; This will call org-agenda-to-appt when emacs starts, and, if you never exit emacs, it will automatically call org-agenda-to-appt at mid-night. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For org appointment reminders


  ;; Run once, activate and schedule refresh
  (my-org-agenda-to-appt)
  (appt-activate t)
  (run-at-time "24:01" nil 'my-org-agenda-to-appt)

  ; 5 minute warnings
  (setq appt-message-warning-time '15)
  (setq appt-display-interval '5)

  ; Update appt each time agenda opened.
  (add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)

; (3) ... When TODO.txt is saved
;; removendo o after save pq salvo direto automaticamente 
  ;; (add-hook 'after-save-hook
  ;; 			'(lambda ()
  ;; 			   (if (string= (buffer-file-name) (concat (getenv "HOME") "/org/TODO.org"))
  ;; 				   (my-org-agenda-to-appt))
  ;; 			   ;; (if (string= (buffer-file-name) (concat (getenv "HOME") "/org/empresa.org"))
  ;; 			   ;; 	   (my-org-agenda-to-appt))
  ;; 			   ))

; Display appointments as a window manager notification
  (setq appt-disp-window-function 'my-appt-display)
  (setq appt-delete-window-function (lambda () t))

  (setq my-appt-notification-app (concat (getenv "HOME") "/config/shell/appt-notification"))
)


;; retirado do org-hacks 
;; se ficar 15 minutos inativo, mostre o org-agenda
;; John Wiegley: Displaying your Org agenda after idle time

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              )))
      (call-interactively 'org-agenda-list)))
  )

(run-with-idle-timer 1200 t 'jump-to-org-agenda)





(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usetheme{Madrid} \\usecolortheme{default} 
      \\usepackage[brazil]{babel}  % data em portugues 
      \\usepackage{fancyvrb}  % para Verbatim 
      \\usepackage{gensymb}  % para o \\degree 
      \\usepackage{hyperref}  % para o o href{./arquivo} link para arquivos 
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{}}}}\n          
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")
     
     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"
     
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-export-latex-classes

  '("article"
     "\\documentclass[12pt,a4paper]{article}
      \\usepackage[brazil]{babel}
      \\usepackage{color}
      \\usepackage{amsfonts}
      \\usepackage{geometry}
      \\addtolength{\\hoffset}{-1.5cm}
      \\addtolength{\\textwidth}{1cm}
      \\addtolength{\\voffset}{-1cm}
      \\addtolength{\\textheight}{1cm}
      \\linespread{1}
      \\pagestyle{plain}"
     
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



;; para publicar projetos no ~/public_html


(defun wlh-org-latex-publish-code-to-pdf () 
  (interactive)
  ;; lembre que tem que ter o pygments instalado
  ;; easy_install Pygments
  (setq oll org-latex-packages-alist)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (let
      (
       (org-latex-minted-options
	(quote
	 (("fontsize" "\\footnotesize")
	  ("linenos" "true")
	  ("xleftmargin" "0em"))))
       (org-latex-listings 'minted)
       (org-latex-pdf-process
	'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")) 
       )
    (org-latex-export-to-pdf)
    ) ; let
  (setq org-latex-packages-alist oll)
  ) ; defun


;; para habilitar criptografia - http://orgmode.org/worg/org-tutorials/encrypting-files.html
(setq epg-gpg-program "gpg2")

(use-package epa-file
  :config
  (epa-file-enable)
;;  (setq epg-pinentry-mode 'loopback)
  )

;; faz o cache da autenticação por 15 minutos
(require 'password-cache)
(setq password-cache-expiry (* 15 60))
(setq epa-file-cache-passphrase-for-symmetric-encryption t)


(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  ;; (setq org-crypt-key nil)
(setq org-crypt-key "E8752E00")


;; deixa a agenda mais limpa
(setq org-use-tag-inheritance nil)

(use-package mobileorg-config
  :after org
  :defer 3
)


;;; teste com cores para itens

(use-package org-protocol)

;; ajusta o nível de subtree colada automaticamente
(setq org-yank-adjusted-subtrees t)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)

;; marca as entradas do diário no calendário quando colocado o
;; %%(org-diary)
;; no arquivo de diário
(setq calendar-mark-diary-entries-flag t)

(if (string= system-name "colossus")
    (progn
      (setq org-agenda-files
	    (quote
	     ("~/org/TODO.org"
	      
	      "~/org/cursos/cursos.org"
	      "~/org/cursos/git/cronograma.org"
	      )))
      )
  ;; não estou no notebook então estou na empresa
  (progn
    (setq org-agenda-files
	  (quote
	   ("~/org/TODO.org"
	    
	    "~/org/cursos/cursos.org"
	    "~/org/cursos/git/cronograma.org"
	    "~/org/empresa.org"
	    "~/org/datam.org")))
    )
  )

;; Variable: noninteractive -- This variable is non-nil when Emacs is running in batch mode.
(when (equal noninteractive nil)
  (setq org-agenda-include-diary t)
)

;; para que tenha fontificação dos blocos do R
(use-package ess)

 (setq org-babel-load-languages
   (quote
	((emacs-lisp . t)
	 (C . t)
	 (ditaa . t)
	 (latex . t)
	 (python . t)
	 (R . t)
	 (dot . t)
	 (sql . t)
	 (sqlite . t)
	 (plantuml . t)
	 (ledger . t)
	 (http . t)
	 (shell . t))))

(org-babel-do-load-languages
 'org-babel-load-languages
   (quote
	((emacs-lisp . t)
	 (C . t)
	 (ditaa . t)
	 (latex . t)
	 (python . t)
	 (R . t)
	 (dot . t)
	 (sql . t)
	 (sqlite . t)
	 (plantuml . t)
	 (ledger . t)
	 (http . t)
	 (shell . t)))
)



;; ao fazer o capture, deixe só a janela do capture ativa

(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; templates de capture


(setq org-capture-templates
      (quote
       (
	("l" "Leslie")
	("lf" "TODO - Entrada" entry
	 (file+headline "~/org/TODO.org" "Entrada")
	 "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:URL:      %c
:END:
  %i
  %a
" :empty-lines 2 )
	("t" "Empresa - Tarefa -> Entrada Empresa" entry
	 (file+headline "~/org/datam.org" "Entrada")
	 "* TODO %^{Titulo} %?
  :PROPERTIES:
  :ID:       %(shell-command-to-string \"uuidgen\")  :CREATED:  %U
  :URL:      %c
  :Effort: %^{Esforço|0:10|0:15|0:30|1:00}
  :END:
  %i
  %a
" :empty-lines 1 :prepend t :unnarowed t :jump-to-captured t :clock-in t :clock-resume t)

	("c" "Empresa -- Coisas da Empresa")
	("cb" "CLP - Planejamento de Objetivos da Semana" entry
	 (file+headline "~/org/empresa.org" "Entrada")
	 "*** %^{Data Inicial}u -- %^{Data Final}u - %T %? 
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:
  %i
 
**** %^{Atividades} 

 ")
	("c1" "CLP - Captura de Seleção -> Entrada" entry
	 (file+headline "~/org/empresa.org" "Entrada")
	 "*  %?
  %^C 
  %a
")
	("cr" "CLP - Agendamento de Reunião" entry
	 (file+headline "~/org/empresa.org" "Entrada")
	 "* REUNIÃO - %^{Assunto}%?\nSCHEDULED: %^{Scheduled}t
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:
  %i
  + Data: 
  + Local: %^{Local}
  + Participantes:
     + %^{Participantes}

%a 
"  :empty-lines 1)
	("ct" "Empresa - Tarefa -> Entrada Empresa" entry
	 (file+headline "~/org/empresa.org" "Entrada")
	 "* TODO %?
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:URL:      %c
:END:
  %i
  %a
" :empty-lines 1 :prepend t :unnarowed t :jump-to-captured t )
	
	("w" "Revisão: Revisão da Semana com templates" entry
	 (file+datetree "~/org/reviews.org")
	 (file "~/org/templates/weeklyreviewtemplate.org")
	 )


;; https://github.com/james-stoup/emacs-org-mode-tutorial?tab=readme-ov-file#capture-template-4
        ("m" "Meeting"
         entry (file+datetree "~/org/meetings.org")
         "* %? :meeting:%^g \n  :Created: %T\n** Participantes\n  + \n** Notas\n** Ações a serem tomadas \n*** TODO [#A] "
         :tree-type week
         :clock-in t
         :clock-resume t
         :empty-lines 0)


	;; http://orgmode.org/worg/org-contrib/org-protocol.html
	("x" "Captura Área de Transferência - org-protocol" entry
	 (file "~/org/capture.org")
	 "* REVER %a
SCHEDULED: %T
:PROPERTIES:
:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U
:END:

%c
%:link
Capturado em: %U
%i
** %:description
 %?
 %i
" :empty-lines-before 1)
	("R" "Receitas")
	("Ru" "Receita a partir de URL" entry (file "~/org/receitas/receitas.org")
         "%(org-chef-get-recipe-from-url)"
         :empty-lines 1)
	("Rm" "Digitar Receita Manual" entry (file "~/org/receitas/receitas.org")
         "* %^{Título da Receita: }\n  :PROPERTIES:\n  :source-url:\n  :servings:\n  :prep-time:\n  :cook-time:\n  :ready-in:\n  :END:\n** Ingredientes\n   %?\n** Modo de Fazer\n\n") 
	)
       )
      )




 (setq org-confirm-babel-evaluate nil)
 (setq org-export-backends (quote (ascii html icalendar latex odt)))
 (setq org-export-default-language "pt_BR")
 (setq org-export-html-postamble nil)
 (setq org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo todo-start)))
 (setq org-icalendar-alarm-time 15)
 (setq org-log-into-drawer t)
 (setq org-modules
   (quote
	(ol-bbdb ol-bibtex ol-crypt ol-docview ol-gnus ol-info ol-protocol ol-jsinfo ol-habit ol-irc ol-mew ol-mhe ol-rmail ol-special-blocks ol-vm ol-wl ol-w3m ol-choose ol-git-link ol-man ol-odt org2rem)))
 (setq org-plantuml-jar-path "~/org/jar/plantuml.jar")
 (setq org-src-fontify-natively t)


;; o ? indica onde o cursor será posicionado ao final.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonte [[http://www.coli.uni-saarland.de/~slemaguer/emacs/main.html#orgc54cbf5][Emacs configuration file]]

(use-package hydra :ensure t
  :config
  ;; Define the templates

  (setq org-structure-template-alist
   (quote
    (("s" . "src")
     ("e" . "example")
     ("q" . "quote")
     ("v" . "verse")
     ("c" . "center")
     ("l" . "export latex")
     ("h" . "export html")
     ("a" . "export ascii")
     ("I" . "include: %file ?")
     ("D" . "idea")
     ("N" . "note")
     ("W" . "warning")
     ("O" . "workout")
     ("Q" . "question")
     ("C" . "caveira")
     ("F" . "info")
     ("T" . "tip")
     ("U" . "url")
     ("p" . "pwd")
     ("m" . "img \n#+CAPTION: \n#+LABEL: \n#+ATTR_LaTeX: :width cm  :placement [h!]\n[[file:./images/?]]\n")
     )
    )
   )

(setq org-tempo-keywords-alist
'(("L" . "latex")
  ("H" . "html")
  ("A" . "ascii")
  ("i" . "index"))
)

  ;; Shortcuts
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (if (fboundp 'org-try-structure-completion)
          (org-try-structure-completion) ; < org 9
        (progn
          ;; New template expansion since org 9
          (require 'org-tempo nil t)
          (org-tempo-complete-tag)))
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
     Org template

 block               src block         structure                 text structs
---------------------------------------------------------------------------------------
_c_: center        _s_: src         _L_: LATEX           _D_: Idea         _U_: Url
_q_: quote         _E_: emacs lisp  _i_: index           _N_: Note         _p_: PWD
_e_: example       _b_: bash        _I_: INCLUDE         _W_: Warning
_v_: verse         _j_: json        _H_: HTML            _O_: workOut
_a_: ascii         _u_: Plantuml    _A_: ASCII           _Q_: Question
_l_: latex         _d_: ditaa       _m_: image           _C_: Caveira
_h_: html          _S_: shell       _X_: attrlateX       _F_: inFo
                   _x_: xml         _Z_: exportopts      _T_: Tip
"
    ("s" (hot-expand "<s"))
    ("e" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("E" (hot-expand "<s" "emacs-lisp"))
    ("b" (hot-expand "<s" "bash :tangle SCRIPT.sh :tangle-mode (identity #o755)"))
    ("j" (hot-expand "<s" "json"))
    ("x" (hot-expand "<s" "xml"))
    ("S" (hot-expand "<s" "sh"))
    ("d" (hot-expand "<s" "ditaa :file CHANGE.png :cache yes"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.svg :cache yes"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("m" (hot-expand "<m"))
    ("X" (hot-expand "<X"))
    ("D" (hot-expand "<D"))
    ("N" (hot-expand "<N"))
    ("W" (hot-expand "<W"))
    ("O" (hot-expand "<O"))
    ("Q" (hot-expand "<Q"))
    ("C" (hot-expand "<C"))
    ("F" (hot-expand "<F"))
    ("T" (hot-expand "<T"))
    ("U" (hot-expand "<U"))
    ("p" (hot-expand "<p"))
    ("Z" (hot-expand "<Z"))
    ("<" self-insert-command "ins")
    ("ESC" nil "quit"))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
          (hydra-org-template/body)
        (self-insert-command 1))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (http://sachachua.com/blog/2010/07/using-org2blog-to-publish-org-mode-subtrees/)
;; Then I can go to the entry and call
;; M-x org2blog-post-subtree
;; to post a draft or
;; C-u M-x org2blog-post-subtree
;; to publish it.
;; Note that the code uses whatever heading level you’re on, so if you’re under a sub-heading of the post you want to publish, use
;; C-c C-u outline-up-heading
;; to go up headings until you’re at the right level.
;; git clone https://github.com/sachac/org2blog
(log-message "...org2blog")

(use-package org2blog
  :disabled
  :load-path "~/emacs/scm/org2blog"
  :config
  (setq org2blog-server-url "http://lezz.wordpress.com/xmlrpc.php"
		org2blog-server-user "lezz"
		org2blog-server-weblog-id ""
		org2blog-use-tags-as-categories t)
; como eu carrego a 'pia-da-cozinha' junto no emacs, e nem sempre estou conectado, deixo comentado o login
;  (org2blog-login) 
  )


(log-message "...org-journal")

(use-package org-journal
    :ensure t
    :bind (("C-c j" . org-journal-new-entry)
           ("C-c y" . journal-file-yesterday))
    :custom
    ;;    (setq org-journal-dir (concat "~/org/journal/" (format-time-string "%Y/" )))
    (org-journal-dir (concat "~/org/journal/" (format-time-string "%Y/" )))
    (org-journal-file-format "%Y%m%d")
    (org-journal-date-format "%e %b %Y (%A)")
    (org-journal-enable-agenda-integration t)
    (org-journal-update-org-agenda-files)
;;    (org-journal-time-format "")
    :preface
    (defun get-journal-file-yesterday ()
      "Gets filename for yesterday's journal entry."
      (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
             (daily-name (format-time-string "%Y%m%d" yesterday)))
        (expand-file-name (concat org-journal-dir daily-name))))

    (defun journal-file-yesterday ()
      "Creates and load a file based on yesterday's date."
      (interactive)
      (find-file (get-journal-file-yesterday)))
      )



;; http://cestlaz.github.io/posts/using-emacs-26-gcal/#.WxgZLnUvxhE
;; https://github.com/myuhe/org-gcal.el
;;(setq package-check-signature nil)
;;

;; usado pelo gcal
(require 'plstore)
(add-to-list 'plstore-encrypt-to '("leslie@watter.net"))


 ;; export headlines to separate files
 ;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files
 (defun org-export-headlines-to-pdf ()
   "Export all subtrees that are *not* tagged with :noexport: to
 separate files.

 Subtrees that do not have the :EXPORT_FILE_NAME: property set
 are exported to a filename derived from the headline text."
   (interactive)
   (save-buffer)
   (let ((modifiedp (buffer-modified-p)))
     (save-excursion
       (goto-char (point-min))
       (goto-char (re-search-forward "^*"))
       (set-mark (line-beginning-position))
       (goto-char (point-max))
       (org-map-entries
	(lambda ()
          (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
            (unless export-file
              (org-set-property
               "EXPORT_FILE_NAME"
               (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
            (deactivate-mark)
            (org-latex-export-to-pdf nil t)
            (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
            (set-buffer-modified-p modifiedp)))
	"-noexport" 'region-start-level))))

;; To save the clock history across Emacs sessions, use

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; por padrão usa o arquivo org-clock-persist-file "~/.emacs.d/org-clock-save.el"


;;https://stackoverflow.com/questions/22888785/is-it-possible-to-get-org-mode-to-show-breadcrumbs-in-agenda-todo-list
;; Coloca o breadcrumb na view com TODO
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
         (timeline . "  % s")
         (todo .
               " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
         (tags .
               " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
         (search . " %i %-12:c"))
      )



;; [[http://www.gonsie.com/blorg/org-highlight.html][Highlighting in Org-Mode]]

;; com essas duas funções, texto marcado com :#algum texto aqui:# será exportado
;; para html com a tag <mark> do html5

(defun org-add-my-extra-markup ()
  "Add highlight emphasis."
  (add-to-list 'org-font-lock-extra-keywords
               '("[^\\w]\\(:#\\[^\n\r\t]+:#\\)[^\\w]"
                 (1 '(face highlight invisible nil)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-markup)

(defun my-html-mark-tag (text backend info)
  "Transcode :blah: into <mark>blah</mark> in body text."
  (when (org-export-derived-backend-p backend 'html)
    (let ((text (replace-regexp-in-string "[^\\w]\\(:#\\)[^\n\t\r]+\\(:#\\)[^\\w]" "<mark>"  text nil nil 1 nil)))
      (replace-regexp-in-string "[^\\w]\\(<mark>\\)[^\n\t\r]+\\(:#\\)[^\\w]" "</mark>" text nil nil 2 nil))))

(add-to-list 'org-export-filter-plain-text-functions 'my-html-mark-tag)

;; Começa a agenda com o que já foi feito hoje
;; ajuda a manter o log
(setq org-agenda-start-with-log-mode t)

;; colocando o log em re-scheduling
(setq
 org-log-redeadline (quote note)
 org-log-reschedule (quote time)
)

;; permite o uso de uma tecla para selecionar as tags
;; existentes usando o tab
(setq org-fast-tag-selection-single-key (quote expert))

;;
;; Acho que fica mais de exemplo do que dá pra fazer com as teclas de atalho que outra coisa
;; agendando coisas para o dia de hoje
(defun mrb/org-schedule-for-today()
  "Schedule the current item for today"
  (interactive)
  (org-schedule nil
                (format-time-string "%Y-%m-%d")))
(bind-key "C-." 'mrb/org-schedule-for-today org-mode-map)

(defun mrb/org-agenda-schedule-for-today()
  "Schedule the current item in the agenda for today"
  (interactive)
  (org-agenda-schedule nil
                       (format-time-string "%Y-%m-%d")))
(bind-key "C-." 'mrb/org-agenda-schedule-for-today org-agenda-mode-map)



(setq org-agenda-prefix-format
      (quote
       ((agenda . "%-12c%?-12t% s")
        (timeline . "% s")
        (todo . "%-12c")
        (tags . "%-12c")
        (search . "%-12c"))))

(setq org-agenda-deadline-leaders (quote ("!D!: " "D%2d: " "")))
(setq org-agenda-scheduled-leaders (quote ("" "S%3d: ")))

(log-message "...org-msg")

(use-package org-msg
 :ensure t
 :config
 (setq mail-user-agent 'message-user-agent)
 (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nOlá *%s*,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-signature "

 Atenciosamente,

 #+begin_signature
 --- \\\\
 *Leslie H. Watter* \\\\
 #+end_signature")
 (org-msg-mode)
 )

;; org-latex-logfiles-extensions

;; org-chef para receitas -- [[https://github.com/Chobbes/org-chef][GitHub - Chobbes/org-chef: A package for making a cookbook and managing recip...]]

(log-message "...org-chef")
(use-package org-chef
  :ensure t)

;; [[https://github.com/alphapapa/org-web-tools][GitHub - alphapapa/org-web-tools: View, capture, and archive Web pages in Org...]]
(log-message "...org-web-tools")

(use-package org-web-tools :ensure t)

(defun org-todo-age-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "CREATED" t)))
    (when stamp
      (time-subtract (current-time)
                     (org-time-string-to-time
                      (org-entry-get (or pos (point)) "CREATED" t))))))

(defun org-todo-age (&optional pos)
  (let ((days (time-to-number-of-days (org-todo-age-time pos))))
    (cond
     ((< days 1)   "today")
     ((< days 7)   (format "%dd" days))
     ((< days 30)  (format "%.1fw" (/ days 7.0)))
     ((< days 358) (format "%.1fM" (/ days 30.0)))
     (t            (format "%.1fY" (/ days 365.0))))))


;; adicionando linguagem do plantuml
(add-to-list
 'org-src-lang-modes '("plantuml" . plantuml))

;; permite o archive a partir da agenda
(setq org-archive-subtree-save-file-p t)

;; Coloca a janela de src code block do lado direito
(setq org-src-window-setup 'split-window-right)

;; para o auto-commit de attachments
(require 'org-attach-git)


;; turn on the display of the first data row of the table at point if the window header line when this first row is not visible anymore in the buffer

(setq org-table-header-line-mode t)

(log-message " -- ok!\n")

;; This is an helper function for org-download. It creates an \"./image\" folder within the same directory of the org file. Images are separated inside that image folder by additional folders one per org file.
;; https://gist.github.com/daviderestivo/ad3dfa38d3f7266d014ce469aafd18dc

(defun drestivo/org-download-method (link)
  "This is an helper function for org-download.
It creates an \"./image\" folder within the same directory of the org file.
Images are separated inside that image folder by additional folders one per
org file.
More info can be found here: https://github.com/abo-abo/org-download/issues/40.
See the commit message for an example:
https://github.com/abo-abo/org-download/commit/137c3d2aa083283a3fc853f9ecbbc03039bf397b"
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link)))))
        (dir (concat
              (file-name-directory (buffer-file-name))
              (format "%s/%s/%s"
                      "images"
                      (file-name-base (buffer-file-name))
                      (org-download--dir-2)))))
    (progn
      (setq filename-with-timestamp (format "%s%s.%s"
                                            (file-name-sans-extension filename)
                                            (format-time-string org-download-timestamp)
                                            (file-name-extension filename)))
      ;; Check if directory exists otherwise creates it
      (unless (file-exists-p dir)
        (make-directory dir t))
      (message (format "Image: %s saved!" (expand-file-name filename-with-timestamp dir)))
      (expand-file-name filename-with-timestamp dir))))

(setq org-download-method  'drestivo/org-download-method)

(defun org-next-table (&optional arg)
  "Jump to the next table.

With a prefix argument ARG, jump forward ARG many tables."
  (interactive "p")
  (cl-loop
     for n below (abs arg)
     with backward = (< arg 0)
     with search-fn = (if backward #'re-search-backward #'re-search-forward)
     do  
       (setq pt (point))
       (when (org-at-table-p)
         (funcall search-fn org-table-border-regexp nil :move))
     if (funcall search-fn org-table-line-regexp nil t) do
       (when (org-invisible-p)
         (org-reveal t)
         (org-show-entry)
         (unless (org-at-table-p)
           (cl-decf n)))
     else return (goto-char pt)
     finally (when backward
               (when (funcall search-fn org-table-border-regexp nil :move)
                 (next-line))
               (forward-char))))

(defun org-previous-table (&optional arg)
  "Jump to the previous table.

With a prefix argument ARG, jump backward ARG many tables."
  (interactive "p")
  (org-next-table (- arg)))


;; Set the width of inline images:
  (setq org-image-actual-width '(800))


;; colocar link em cada headline exportado

   (defun tec/org-export-html-headline-anchor (text backend info)
     (when (org-export-derived-backend-p backend 'html)
       (replace-regexp-in-string
	"<h\\([0-9]\\) id=\"\\([a-z0-9-]+\\)\">" ; this is quite restrictive, but due to `org-heading-contraction' I can do this
	"<h\\1 id=\"\\2\">\
  <a class=\"anchor\" aria-hidden=\"true\" href=\"#\\2\">🔗</a>"
	text))

     (add-to-list 'org-export-filter-headline-functions
		  'tec/org-export-html-headline-anchor)
     )

;; pomodoro

;; use the quelpa mirror because of that
(use-package pomodoro
  ;; :quelpa (pomodoro :fetcher github :repo "emacsmirror/pomodoro")
  :config (pomodoro-add-to-mode-line)
  (setq pomodoro-break-start-sound  (expand-file-name "config/emacs/sons/ayrton-senna-tema-da-vitoria.mp3"  (getenv "HOME"))
	pomodoro-work-start-sound (expand-file-name "config/emacs/sons/321-lift.mp3"  (getenv "HOME"))
	)
  )


(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
   )
  (setq
   org-pomodoro-length 50
   org-pomodoro-short-break-length 10
   )

   (setq org-pomodoro-audio-player "/usr/bin/mpg123")
   (setq org-pomodoro-play-sounds t
	   org-pomodoro-start-sound-p t
	   org-pomodoro-clock-break t
	   org-pomodoro-finished-sound-p t
	   org-pomodoro-short-break-sound-p t
	   org-pomodoro-long-break-sound-p t
	   org-pomodoro-start-sound (expand-file-name "config/emacs/sons/321-lift.mp3"  (getenv "HOME"))
	   org-pomodoro-finished-sound (expand-file-name "config/emacs/sons/ayrton-senna-tema-da-vitoria.mp3"  (getenv "HOME"))
	   org-pomodoro-short-break-sound (expand-file-name "config/emacs/sons/bemtevi.mp3"  (getenv "HOME"))
	   org-pomodoro-long-break-sound (expand-file-name "config/emacs/sons/tiro.mp3"  (getenv "HOME"))
	   )
  
  )

;; Download the sound at https://freesound.org/people/.Andre_Onate/sounds/484665/
(setq org-clock-sound "~/config/shell/ding.wav")
;; mostra numero de linhas temporariamente no src block
;; https://marcohassan.github.io/bits-of-experience/pages/emacs/

(defun temp-line-src-block ()
  (interactive)
      (defvar number-line-overlays '()
	"List of overlays for line numbers.")

      (make-variable-buffer-local 'number-line-overlays)

      (defun number-line-src-block ()
	(save-excursion
	  (let* ((src-block (org-element-context))
		 (nlines (- (length
			     (s-split
			      "\n"
			      (org-element-property :value src-block)))
			    1)))
	    (goto-char (org-element-property :begin src-block))
	    (re-search-forward (regexp-quote (org-element-property :value src-block)))
	    (goto-char (match-beginning 0))

	    (loop for i from 1 to nlines
		  do
		  (beginning-of-line)
		  (let (ov)
		    (setq ov (make-overlay (point) (point)))
		    (overlay-put ov 'before-string (format "%3s " (number-to-string i)))
		    (add-to-list 'number-line-overlays ov))
		  (next-line))))

	;; now read a char to clear them
	(read-key "Press a key to clear numbers.")
	(mapc 'delete-overlay number-line-overlays)
	(setq number-line-overlays '()))

      (number-line-src-block)
      )

;; define keyboard shortcuts for special mode
(eval-after-load "org-mode"
     (define-key org-mode-map (kbd "C-c C-v 1") 'temp-line-src-block)
)


(defun my-org-agenda-to-appt-block (agenda-files)
  (interactive)
  (setq appt-time-msg-list nil)
  (let ((org-deadline-warning-days 0)
	;; aqui defino quais arquivos serão usados para criar os avisos de popup de agenda
	(org-agenda-files agenda-files
	 )
	)
    (message "agenda-files: %s" (org-agenda-files))
    (org-agenda-to-appt)
    (org-agenda-list)
    ))



;; Colocar links nos headers html automaticamente

(setq org-html-self-link-headlines 't)


(setq org-publish-use-timestamp-flag 'nil)
;; https://orgmode.org/worg/org-hacks.html -- Org-mode and saveplace.el
(add-hook 'org-mode-hook
          (lambda ()
            (when (outline-invisible-p)
              (save-excursion
                (outline-previous-visible-heading 1)
                (org-show-subtree)))))


;; Add an effort estimate on the fly when clocking in

(add-hook 'org-clock-in-prepare-hook
          'my-org-mode-ask-effort)

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (or (org-entry-get (point) "Effort") (bound-and-true-p org-capture-mode))
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))


;; (use-package org-timeline
;;   :commands org-agenda
;;   :init
;;   (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append))

;; https://emacs.stackexchange.com/questions/66891/org-mode-agenda-load-new-org-files-without-restart
;; https://emacs.stackexchange.com/a/67896
;; sudo apt-get install inotify-tools


(setq ndk/inotify-org-agenda-process
   (make-process 
      :name "ndk-inotify-org-agenda"
      :command `("inotifywait"
                 "-e" "create"
                 "-e" "delete"
                 "-m" "-r" ,(expand-file-name "~/org"))
      :buffer (get-buffer-create "*ndk/inotify-org-agenda-process-buffer*")
      :connection-type 'pipe
      :stderr (get-buffer-create "*ndk/inotify-org-agenda-process-stderr*")))



;; interessante para colocar itens e outras coisas na tarefa 
;; https://howardism.org/Technical/Emacs/capturing-content.html
(add-to-list 'org-capture-templates
             `("k" "Item to Current Clocked Task" item
               (clock)
               "%i%?" :empty-lines 1))

(add-to-list 'org-capture-templates
             `("K" "Kill-ring to Current Clocked Task" plain
               (clock)
               "%c" :immediate-finish t :empty-lines 1))

(add-to-list 'org-capture-templates
             `("C" "Selected Region Contents to Current Clocked Task" plain
               (clock)
               "%i" :immediate-finish t :empty-lines 1))



;---- caldav

(require 'org-caldav)

;; URL of the caldav server
(setq org-caldav-url "https://instalacao-do-sogo-da-empresa.net/SOGo/dav/usuario/Calendar")

;; calendar ID on server
(setq org-caldav-calendar-id "personal")

;; Org filename where new entries from calendar stored
(setq org-caldav-inbox "~/org/calendars/sogo.org")

;; Additional Org files to check for calendar events
(setq org-caldav-files nil)


;; só quero trazer as informações pro org-mode, sem alterar o SOGO que não controlo
(setq org-caldav-sync-direction 'cal->org)

;; Usually a good idea to set the timezone manually
(setq org-icalendar-timezone "America/Brasilia")

(setq org-caldav-delete-org-entries 'always)
(setq org-caldav-delete-calendar-entries 'never)
(setq org-caldav-resume-aborted 'never)


(defun lhw-atualiza-sogo ()
  "Remove TODAS as entradas do arquivo do calendário do Sogo e executa a sincronização para buscar tudo de novo."
  (interactive)
  (pop-to-buffer (find-file-noselect "~/org/calendars/sogo.org"))
  (goto-char (point-min))
  (beginning-of-line)
  (setq myStartPos2 (line-beginning-position))
  (goto-char (point-max))
  (end-of-line)
  (setq myEndPos2 (line-end-position))
  ;; delete the region
  (delete-region myStartPos2 myEndPos2)
  (basic-save-buffer)
  (message "Arquivo Atualizado" )
  (kill-buffer (find-buffer-visiting "~/org/calendars/sogo.org"))
  (org-caldav-sync)
)


(run-at-time "08:01" 1800 'lhw-atualiza-sogo) ;; update agenda every 30 minutes

