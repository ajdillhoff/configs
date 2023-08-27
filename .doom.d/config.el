;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Dillhoff"
      user-mail-address "ajdillhoff@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/ajdillhoff@gmail.com/notes/")

(setq ajdillhoff/default-bibliography (list "~/ajdillhoff@gmail.com/bibliography/master.bib"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(use-package! ctrlf
              :hook
              (after-init . ctrlf-mode))

(require 'org)
(require 'find-lisp)
(require 'org-download)

;; Dragon-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq ajdillhoff/org-agenda-directory (file-truename "~/ajdillhoff@gmail.com/notes/gtd/"))
(setq org-agenda-files
      (find-lisp-find-files ajdillhoff/org-agenda-directory "\.org$"))

(setq org-capture-templates
      `(("i" "Inbox" entry (file "gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "Entered on %U"))))

(defun ajdillhoff/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(map! :map org-agenda-mode-map
      "c" #'ajdillhoff/org-inbox-capture)

(use-package! org-agenda
              :init
              (map! "<f12>" #'ajdillhoff/switch-to-agenda)
              (defun ajdillhoff/switch-to-agenda ()
                (interactive)
                (org-agenda nil " "))
              :config
              (defun ajdillhoff/is-project-p ()
                "Any task with a todo keyword subtask"
                (save-restriction
                  (widen)
                  (let ((has-subtask)
                        (subtree-end (save-excursion (org-end-of-subtree t)))
                        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
                    (save-excursion
                      (forward-line 1)
                      (while (and (not has-subtask)
                                  (< (point) subtree-end)
                                  (re-search-forward "^\*+ " subtree-end t))
                        (when (member (org-get-todo-state) org-todo-keywords-1)
                          (setq has-subtask t))))
                    (and is-a-task has-subtask))))
              (setq org-agenda-custom-commands `((" " "Agenda"
                                                  ((agenda ""
                                                           ((org-agenda-span 'week)
                                                            (org-deadline-warning-days 365)))
                                                   (todo "TODO"
                                                         ((org-agenda-overriding-header "Active Research")
                                                          (org-agenda-files `(,(expand-file-name "gtd/research.org" org-directory)))
                                                          ))
                                                   (todo "TODO"
                                                         ((org-agenda-overriding-header "Teaching")
                                                          (org-agenda-files `(,(expand-file-name "gtd/teaching.org" org-directory)))))
                                                   (todo "TODO"
                                                         ((org-agenda-overriding-header "Personal")
                                                          (org-agenda-files `(,(expand-file-name "gtd/personal.org" org-directory)))
                                                          ))
                                                   (todo "TODO"
                                                         ((org-agenda-overriding-header "Inbox")
                                                          (org-agenda-files `(,(expand-file-name "gtd/inbox.org" org-directory)))
                                                          ))
                                                   )))))

(use-package! org-roam
              :init
              (setq org-roam-v2-ack t)
              (map! :leader
                    :prefix "n"
                    :desc "org-roam" "l" #'org-roam-buffer-toggle
                    :desc "org-roam-node-insert" "i" #'org-roam-node-insert
                    :desc "org-roam-node-find" "f" #'org-roam-node-find
                    :desc "org-roam-ref-find" "r" #'org-roam-ref-find
                    :desc "org-roam-show-graph" "g" #'org-roam-show-graph
                    :desc "org-roam-capture" "c" #'org-roam-capture)
              (setq org-roam-directory (file-truename "~/ajdillhoff@gmail.com/notes/org-roam/")
                    org-roam-db-gc-threshold most-positive-fixnum
                    org-id-link-to-org-use-id t)
              :config
              (org-roam-db-autosync-enable)
              (set-popup-rules!
                `((,(regexp-quote org-roam-buffer)
                    :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 1)
                  ("^\\*org-roam: "
                   :side right :width .33 :height .5 :ttl nil :modeline nil :quit nil :slot 2)))

              (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
              (setq org-roam-capture-templates
                    '(("d" "default" plain
                       "%?"
                       :if-new (file+head "main/${slug}.org"
                                         "#+title: ${title}\n#+hugo_base_dir: ../published\n")
                       :immediate-finish t
                       :unnarrowed t)
                      ("r" "bibliography reference" plain "%?"
                       :if-new
                       (file+head "references/${slug}.org" "#+title: ${title}\n")
                       :unnarrowed t)
                      ("a" "article" plain "%?"
                       :if-new
                       (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n#+hugo_base_dir: ../published\n#+hugo_section: articles\n")
                       :immediate-finish t
                       :unnarrowed t)))
              (cl-defmethod org-roam-node-type ((node org-roam-node))
                "Return the TYPE of NODE."
                (condition-case nil
                    (file-name-nondirectory
                     (directory-file-name
                      (file-name-directory
                       (file-relative-name (org-roam-node-file node) org-roam-directory))))
                  (error "")))
              (setq org-roam-node-display-template
                    (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(use-package! org-roam-dailies
              :init
              (map! :leader
                    :prefix "n"
                    :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
              (map! :leader
                    :prefix "n"
                    :desc "org-roam-dailies-goto-today" "d" #'org-roam-dailies-goto-today)
              (map! :leader
                    :prefix "n"
                    :desc "org-roam-dailies-goto-date" "D" #'org-roam-dailies-goto-date)
              :config
              (setq org-roam-dailies-directory "daily/")
              (setq org-roam-dailies-capture-templates
                    '(("d" "default" entry
                       "* %?"
                       :if-new (file+head "%<%Y-%m-%d>.org"
                                          "#+title: %<%Y-%m-%d>\n")))))

(use-package! org-roam-bibtex
              :after org-roam
              :init
              (org-roam-bibtex-mode)
              (map! :leader
                    :prefix "n"
                    :desc "orb-insert-link" "I" #'orb-insert-link))

(use-package! org-roam-protocol
              :after org-protocol)

(use-package! bibtex-completion
              :config
              (setq bibtex-completion-notes-path org-roam-directory
                    bibtex-completion-bibliography ajdillhoff/default-bibliography
                    org-cite-global-bibliography ajdillhoff/default-bibliography
                    bibtex-completion-library-path "~/ajdillhoff@gmail.com/Zotero/storage"
                    bibtex-completion-pdf-field "file"
                    bibtex-completion-notes-template-multiple-files
                    (concat
                      "#+TITLE: ${title}\n"
                      "#+ROAM_KEY: cite:${=key=}\n"
                      ":PROPERTIES:\n"
                      ":Custom_ID: ${=key=}\n"
                      ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                      ":AUTHOR: ${author-abbrev}\n"
                      ":JOURNAL: ${journaltitle}\n"
                      ":DATE: ${date}\n"
                      ":YEAR: ${year}\n"
                      ":DOI: ${doi}\n"
                      ":URL: ${url}\n"
                      ":END:\n\n"
                      "* Summary"
                      )))

(after! bibtex-completion
  (after! org-roam
    (setq! bibtex-completion-notes-path org-roam-directory)))

;; org-mode hooks
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook 'org-latex-preview nil 'make-it-local)))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; fragtog
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

;; ox-hugo
(use-package! ox-hugo
  :ensure t
  :after ox
  :init
  (setq org-hugo-section "notes"))

;; markdown-mode
(use-package! markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; copilot
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))
