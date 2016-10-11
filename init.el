(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'functions)

(require 'iso-transl)
(transient-mark-mode 1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(column-number-mode 1)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq scroll-conservatively 10000)
(setq inhibit-startup-message t)
(setq-default fill-column 80)
(setq line-move-visual nil)
(delete-selection-mode t)
(setq-default require-final-newline t)
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(setq-default whitespace-auto-cleanup t)
(setq-default whitespace-check-trailing-whitespace t)
(setq-default mac-right-option-modifier nil)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")
(current-fill-column)

(setq tramp-default-method "ssh")

;;(require 'web-mode)
;;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(eval-after-load 'desktop
  '(setq desktop-base-lock-name
         (format "%s-%d" desktop-base-lock-name (emacs-pid))))
(desktop-save-mode 1)
(desktop-load-default)

(if (featurep 'tool-bar)
    (tool-bar-mode -1))
;; (if (featurep 'menu-bar)
;;     (menu-bar-mode -1))
(if (featurep 'scroll-bar)
    (scroll-bar-mode -1))
(if (featurep 'tooltip)
    (tooltip-mode -1))

(load-theme 'ahungry t)
(add-to-list 'default-frame-alist
             '(font . "Monaco-10:weight=bold"))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (turn-on-auto-fill)
              (visual-line-mode +1)
              (turn-on-reftex)
              (TeX-PDF-mode +1)
              (add-to-list 'TeX-command-list
                           '("LuaLaTeX" "lualatex %t" TeX-run-command t t :help "Run LuaLaTeX") t)
              (setq-default LaTeX-math-abbrev-prefix "#")
              (LaTeX-math-mode +1)
              (setq reftex-plug-into-auctex t)
              (local-set-key (kbd "C-c C-t x") 'TeX-toggle-escape)))

(add-hook 'markdown-mode-hook
          (lambda () (turn-on-auto-fill)))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-x C-k C-k") 'kill-region)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-b") 'iswitch-buffer)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'org-setup)

(fset 'yes-or-no-p 'y-or-n-p)

(setenv "PATH"
        (concat (getenv "PATH")
                ":/Library/TeX/texbin:/Users/felipe/Library/Python/2.7/bin:/usr/local/bin"))

(setq exec-path
      (append exec-path
              '("/Library/TeX/texbin" "/Users/felipe/Library/Python/2.7/bin" "/usr/local/bin")))

(setenv "WORKON_HOME" "/opt/anaconda/envs")
(elpy-enable)
(when (executable-find "ipython")
  (elpy-use-ipython))

(add-to-list 'load-path "~/.emacs.d/lisp/emacs-async")
(add-to-list 'load-path "~/.emacs.d/lisp/helm")
(require 'helm-config)
(require 'helm-setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix "#")
 '(magit-branch-arguments nil)
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox %s")
     ("\\.pdf\\'" . "evince %s"))))
 '(package-selected-packages
   (quote
    (use-package js2-mode ac-emmet emmet-mode react-snippets web-mode auctex auctex-latexmk auctex-lua helm-projectile markdown-mode haskell-mode cargo rust-mode rustfmt magit python readline-complete yasnippet elpy ahungry-theme)))
 '(python-shell-completion-native-disabled-interpreters (quote ("ipython" "pypy")))
 '(safe-local-variable-values (quote ((TeX-engine . xelatex)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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


(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-modes 'web-mode)

(require 'snippet-setup)
