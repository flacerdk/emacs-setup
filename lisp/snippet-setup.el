(provide 'snippet-setup)

(use-package emmet-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
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
  (add-hook 'js-mode-hook 'yas-minor-mode)
  (add-hook 'sgml-mode-hook 'yas-minor-mode)
  (add-hook 'rust-mode-hook 'yas-minor-mode)
  :config
;  (setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))

(use-package web-mode
  :config
  (setq web-mode-ac-sources-alist
        '(("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (ac-emmet-html-setup)
               (let ((web-mode-cur-language
                      (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil))))))

