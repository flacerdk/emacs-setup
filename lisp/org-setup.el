(provide 'org-setup)

(setq org-agenda-files (quote ("~/Dropbox/org")))

(add-hook 'org-mode-hook
          (lambda ()
            (global-set-key "\C-cl" 'org-store-link)
            (global-set-key "\C-ca" 'org-agenda)
            (global-set-key "\C-cb" 'org-iswitchb)
            (auto-fill-mode +1)))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

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

(require 'ox-publish)
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
