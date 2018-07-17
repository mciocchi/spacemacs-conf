; Org Mode
(global-set-key (kbd "C-c o r") 'cyanide-org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda)

; Code Checking
(global-set-key (kbd "C-c c a") 'check-parens)

(global-set-key (kbd "M-n") 'up-list)
(global-set-key (kbd "M-p") 'down-list)

(global-set-key (kbd "C-\\") 'fill-region)

(global-set-key "\M-\""
                (lambda () (interactive)
                  (wrap "\"" "\"")))
(global-set-key "\M-\["
                (lambda () (interactive)
                  (wrap "\[" "\]")))
(global-set-key "\M-\{"
                (lambda () (interactive)
                  (wrap "\{" "\}")))

(global-set-key "\M-\'"
                (lambda () (interactive)
                  (wrap "\'" "\'")))

(global-set-key "\M-\,"
                (lambda () (interactive)
                  (wrap "<" ">")))

(global-set-key (kbd "M-D")
                (lambda () (interactive)
                  (progn
                    (delete-char 1)
                    (backward-delete-char-untabify 1))))

(global-set-key (kbd "C-c f f")
                (lambda () (interactive)
                  (find-file-at-point)))

(global-set-key (kbd "M-#")
                (lambda () (interactive)
                  (wrap "/*" "*/")))

(global-set-key (kbd "M-W")
                (lambda () (interactive)
                  (wrap)))

(provide 'global-keybindings)
