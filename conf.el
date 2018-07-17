(require 'defuns)
(require 'global-keybindings)

;; emacs-lisp-mode
(add-hook 'emacs-lisp-mode-hook (lambda () (linum-mode t)))

;; dired-mode
(setq dired-listing-switches "-ltah")
(setq-default dired-hide-details-mode)

;; comint-mode
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map (kbd "C-c C-l")
       (lambda () (interactive)
         (comint-clear-buffer)
         (end-of-buffer)))))

;; json-mode
(add-hook 'json-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      'json-lint-file
                      nil
                      'make-it-local)))

(setenv "NODE_NO_READLINE" "1")

;; js2-mode
(eval-after-load "js2-mode"
  '(progn
     (define-key js2-mode-map (kbd "C-c d d b")
       (lambda () (interactive)
         (insert "debugger;")))

     (setq symmetric-wrap-matches'(("(" ")")
                                   ("{" "}")
                                   ("\"" "\"")
                                   ("[" "]")
                                   ("/*" "*/")
                                   ("<" ">")
                                   ("'" "'")))

     (define-key js2-mode-map (kbd "C-c w j")
       'wrap-json-stringify)))

;; text-mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

;; java-mode
(defun functions-occur-java ()
  nil
  (occur "\\(public\\|private\\|protected\\).*{"))

(defun definition-from-point-moccur-java nil
  "Find all definitions relating to an arbitrary class at point."
  (multi-occur-in-matching-buffers
   ".*"
   (concat
    "\\(extends\\|implements\\|class\\|interface\\).*"
    (thing-at-point 'word 1))))

;; Run current file through the java compiler.
;; NOTE: need more abstraction here around the java compiler to handle
;; project-specific paths, etc.
(add-hook 'java-mode-hook
          (lambda () (interactive)
            (define-key java-mode-map (kbd "C-c c c")
              (lambda () (interactive)
                (async-shell-command
                 (concat "javac " (buffer-file-name)))))))

(add-hook 'java-mode-hook
          (lambda () (interactive)
            '(define-key java-mode-map (kbd "C-c c u o")
               (lambda () (interactive)
                 (functions-occur-java)))))

(add-hook 'java-mode-hook
          (lambda () (interactive)
            (define-key java-mode-map (kbd "C-c c p l")
              (lambda () (interactive)
                (definition-from-point-moccur-java)))))

;; python-mode
(setq parens-require-spaces nil)
(setq tab-width 4)
(setq electric-indent-mode nil)
(setq python-check-command "pyflakes")

;; perl-mode
(eval-after-load "perl-mode"
  '(progn
     (define-key perl-mode-map (kbd "C-c d d c")
       (lambda () (interactive)
         (async-shell-command
          (concat "perl -c "
                  (buffer-file-name)))))

     (define-key perl-mode-map (kbd "C-c d d d")
       (lambda () (interactive)
         (async-shell-command
          (concat "perl -d "
                  (buffer-file-name)))))))

;; inferior-emacs-lisp-mode
(eval-after-load "ielm"
  '(progn
     (define-key inferior-emacs-lisp-mode-map
       (kbd "M-n") 'up-list)

     (define-key inferior-emacs-lisp-mode-map
       (kbd "M-p") 'down-list)))

;; comint-mode
(add-hook 'comint-output-filter-functions 'comint-strip-nodejs-garbage)

;; fill-column-indicator
(setq-default fill-column 80)

;; enable fci-mode in code modes but not others
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (if (and
         (not (string-match "^\*.*\*$" (buffer-name)))
         (not (eq major-mode 'dired-mode))
         (not (eq major-mode 'org-mode))
         (not (eq major-mode 'ag-mode)))
        (fci-mode 1))))
(global-fci-mode 1)

;; misc
;; When using minibuffer inside of minibuffer, show how many minibuffers we
;; currently have open.
(minibuffer-depth-indicate-mode 1)

;; smoother mousewheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
(setq mouse-wheel-progressive-speed t)

;; turn off dumb terminal mode to use interactive progs from terminal
;; I have disabled this for now because these don't render very well in shell
;; modes.
;; (setenv "TERM" "xterm")

;; use Fira Mono font if it exists
(when (x-list-fonts "Fira Mono")
  (add-to-list 'default-frame-alist '(font . "Fira Mono"))
  (set-face-attribute 'default t :font "Fira Mono"))

(provide 'conf)
