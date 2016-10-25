(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'functions)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-math-abbrev-prefix "#")
 '(magit-branch-arguments nil)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/smoke-signal.org" "~/Dropbox/org/work.org" "~/Dropbox/org/danish.org" "~/Dropbox/org/prog.org" "/home/felipe/Dropbox/org/notes.org" "/home/felipe/Dropbox/org/todo.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . "firefox %s")
     ("\\.pdf\\'" . "evince %s"))))
 '(package-selected-packages
   (quote
    (omnisharp flycheck paredit use-package js2-mode ac-emmet emmet-mode web-mode auctex auctex-latexmk auctex-lua helm-projectile markdown-mode haskell-mode cargo rust-mode rustfmt magit python readline-complete yasnippet elpy ahungry-theme)))
 '(python-shell-completion-native-disabled-interpreters (quote ("ipython" "pypy")))
 '(safe-local-variable-values (quote ((TeX-engine . xelatex)))))


(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setenv "PATH"
        (concat (getenv "PATH")
                ":/Library/TeX/texbin:/Users/felipe/Library/Python/2.7/bin:/usr/local/bin"))

(setq exec-path
      (append exec-path
              '("~/.local/bin" "/Library/TeX/texbin" "/Users/felipe/Library/Python/2.7/bin" "/usr/local/bin")))

(setenv "WORKON_HOME" "/opt/anaconda/envs")

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
(fset 'yes-or-no-p 'y-or-n-p)
(if (featurep 'tool-bar)
    (tool-bar-mode -1))
(if (featurep 'scroll-bar)
    (scroll-bar-mode -1))
(if (featurep 'tooltip)
    (tooltip-mode -1))
(electric-pair-mode +1)

(load-theme 'ahungry t)
(add-to-list 'default-frame-alist
             '(font . "Monaco-10:weight=bold"))

(eval-when-compile
  (require 'use-package))

(require 'org-setup)
(require 'snippet-setup)

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook #'electric-pair-mode)
  (add-hook 'rust-mode-hook #'auto-complete-mode))

(use-package desktop
  :config
  (setq desktop-base-lock-name
        (format "%s-%d" desktop-base-lock-name (emacs-pid)))
  (desktop-save-mode 1))

(use-package LaTeX-mode
  :commands (turn-on-reftex TeX-PDF-mode LaTeX-math-mode TeX-toggle-escape)
  :config
  (turn-on-auto-fill)
  (visual-line-mode +1)
  (turn-on-reftex)
  (TeX-PDF-mode +1)
  (add-to-list 'TeX-command-list
               '("LuaLaTeX" "lualatex %t" TeX-run-command t t :help "Run LuaLaTeX") t)
  (setq-default LaTeX-math-abbrev-prefix "#")
  (LaTeX-math-mode +1)
  (setq reftex-plug-into-auctex t)
  :bind ("C-c C-t x" . TeX-toggle-escape))

(use-package markdown-mode
  :config
  (use-package markdown-setup)
  :bind
  ("C-c C-c k" . as/markdown-region-to-latex))


(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-k") 'kill-line)
(global-set-key (kbd "C-x C-k C-k") 'kill-region)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-b") 'iswitch-buffer)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package elpy
  :init
  (elpy-enable)
  (setq python-indent-offset 4))

(use-package helm-config
  :load-path ("lisp/emacs-async/" "lisp/helm/")
  :config
  (use-package helm-setup))

(use-package auto-complete-config
  :config
  (ac-config-default)
  (defadvice auto-complete-mode (around disable-auto-complete-for-python)
    (unless (eq major-mode 'python-mode) ad-do-it))
  (ad-activate 'auto-complete-mode))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2)
  (setq js2-basic-offset 2))

(use-package flycheck
  :config
  (global-flycheck-mode)
  (add-to-list 'flycheck-disabled-checkers
        'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
