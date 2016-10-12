(provide 'functions)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun TeX-toggle-escape nil (interactive)
  "Toggle Shell Escape"
  (setq LaTeX-command
    (if (string= LaTeX-command "pdflatex") "pdflatex -shell-escape" "latex")))

(defun current-line-empty-p ()
  (string-match-p "^\s*$" (thing-at-point 'line)))

(defun count-size ()
  (let ((start (point)))
    (while (not (= 0 (current-line-empty-p)))
      (next-line))
    (previous-line)
    (let ((end (point)))
      (goto-char start)
      (count-lines start end))))

(defun move-to-next-column ()
  (let ((size (count-size)))
    (forward-line (+ 2 size))))

(defun move-to-previous-column ()
  (let ((size (count-size)))
    (forward-line (- (+ 2 size)))))

(defun kill-column ()
  (let ((start (point))
        (size (count-size)))
    (forward-line size)
    (move-end-of-line nil)
    (kill-rectangle start (point))
    (forward-line 2)
    (delete-region start (point))))

(defun kill-and-paste-column ()
  (let ((size (count-size)))
    (kill-column)
    (forward-line (- (+ 2 size)))
    (move-end-of-line nil)
    (yank-rectangle))
  (forward-line 2)
  (move-beginning-of-line nil))

(defun kill-and-paste-all-columns ()
  (move-to-next-column)
  (dotimes (i 4)
    (kill-and-paste-column)))

(defun counsel-locate-action-extern (x)
  "Use xdg-open shell command on X."
  (call-process shell-file-name nil
                nil nil
                shell-command-switch
                (format "%s %s"
                        (if (eq system-type 'darwin)
                            "open"
                          "xdg-open")
                        (shell-quote-argument x))))

(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (counsel-locate-action-extern file)))

