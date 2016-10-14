(provide 'org-setup)

(use-package org
  :config
  (setq org-directory "~/Dropbox/org")
  (auto-fill-mode +1)
  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))
  (setq org-todo-keywords
        '((sequence "TODO" "STARTED" "VERIFY" "|" "DONE")))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
        '(("t" "TODO" entry (file (concat org-directory "/todo.org"))
           "* TODO %?\n %i\n %a")
          ("j" "Journal" entry (file+datetree (concat org-directory
                                                      "/notes.org"))
           "* %?\nEntered on %U\n %i\n %a")))
  (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
  (use-package ox-publish)
  (use-package ox-md)
  (setq org-publish-project-alist
        '(("blog-notes"
           :base-directory "~/blog/"
           :base-extension "org"
           :publishing-directory "~/prog/web/blog/"
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t)
          ("blog-static"
           :base-directory "~/blog/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/prog/web/blog/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("blog" :components ("blog-notes" "blog-static"))))
  (add-to-list 'org-latex-classes
               '("memoir"
                 "\\documentclass[10pt]{memoir}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[12pt,a4]{article}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-export-backends 'md)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c b" . org-iswitchb)
  ("C-c c" . org-capture))


