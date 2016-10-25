(use-package emmet-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  :config
  (setq-default emmet-move-cursor-between-quote t)
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap))

(use-package hippie-exp
  :ensure nil
  :defer t
  :bind ("<C-return>" . hippie-expand)
  :config
  (setq-default hippie-expand-try-functions-list
                '(yas-hippie-try-expand emmet-expand-line)))

(use-package yasnippet
  :defer t
  :init
  (add-hook 'web-mode-hook 'yas-minor-mode)
  (add-hook 'sgml-mode-hook 'yas-minor-mode)
  (add-hook 'rust-mode-hook 'yas-minor-mode)
  :config
;  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))

(use-package es6-snippets
  :load-path ("lisp/es6-snippets"))

(use-package web-mode
  :mode ("\\.jsx\\'" "jsx/.*\\.js[x]?\\'" "\\.html?\\'")
  :config
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-code-indent-offset 2)
  (yas-activate-extra-mode 'js-mode)
  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (ac-emmet-html-setup)
               (let ((web-mode-cur-language
                      (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil)))))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it)))

(provide 'snippet-setup)
