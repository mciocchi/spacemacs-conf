(require 'cyanide-pathable)
(require 'cyanide-describeable)
(require 'cyanide-project)

;; Source:
;; https://stackoverflow.com/questions/43765/pin-emacs-buffers-to-windows-for-cscope
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun clear-registers ()
  (interactive
   (setq register-alist nil)))

;; Insert an arbitrary symbol in between each element of a list.
;; (interleave 'foo '(a b c)
;; output:
;; (a foo b foo c)
(defun interleave (sym list)
  (let ((list2 '()))
    (while list
      (progn
        (push (pop list) list2)
        (when list (push sym list2))))
    (reverse list2)))

;; insert a block of strings to prefix every line in the selected region.
(defun prefix-region (&optional stmnt)
  (interactive)
  (when (not (bound-and-true-p stmnt))
    (setq stmnt (read-string "prefix: ")))
  (if (region-active-p)
      (replace-regexp "^"
                      stmnt
                      nil
                      (region-beginning)
                      (region-end))
    (insert stmnt))
  nil)

(defun kill-buffer-if-exists (buffer-or-name)
  (let ((buff (get-buffer buffer-or-name)))
    (when buff
      (kill-buffer buff))))

(defun narrow-to-function ()
  (interactive)
  (progn
    (search-forward "{")
    (backward-char)
    (mark-sexp)
    (move-beginning-of-line nil)
    (narrow-to-region (region-beginning) (region-end))
    (deactivate-mark)))

(defun next-register-get-pos (pos)
  (let ((reg (nth pos register-alist)))
    (when reg
      (if (integerp (car reg))
          (progn
            (setq register-alist-pos (+ 1 register-alist-pos))
            reg) ; return reg
        (next-register-get-pos (+ 1 pos))))))

(defun next-register ()
  (let ((reg (next-register-get-pos register-alist-pos)))
    (when reg
      (switch-to-buffer (marker-buffer (cdr (car register-alist))))
      (goto-char (car (car register-alist))))))

(defun comint-strip-nodejs-garbage (&optional _string)
  "Strip trailing `^M' characters from the current output group.
This function could be on `comint-output-filter-functions' or bound to a key."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (condition-case nil
          (goto-char
           (if (called-interactively-p 'interactive)
               comint-last-input-end comint-last-output-start))
        (error nil))
      (while (re-search-forward "\\[1G\\[0K" pmark t)
        (replace-match "" t t)))))

(defun back-up-current-project ()
  (interactive)
  (back-up-current-project-1))

(defun back-up-current-project-1 ()
  (back-up-project-with-descriptor-to-separate-directory
   (cyanide-get-one-by-slot cyanide-current-project
                            cyanide-project-collection
                            ":id"
                            'eq)
   ""))

(defun back-up-current-project-with-descriptor ()
  (interactive)
  (back-up-current-project-with-descriptor-1 (completing-read
                                              "backup descriptor: "
                                              nil)))

(defun back-up-current-project-with-descriptor-1 (descriptor)
  (back-up-project-with-descriptor-to-separate-directory
   (cyanide-get-one-by-slot cyanide-current-project
                                            cyanide-project-collection
                                            ":id"
                                            'eq)
   descriptor))

(defun back-up-project-with-descriptor (proj descriptor)
  (let ((backup-date (format-time-string "%Y-%m-%d-%H-%M-S"))
        (proj-root (oref proj :path)))
    (let ((stripped-proj-root (remove-trailing-slashes-except-root proj-root)))
      (let ((destination (concat stripped-proj-root
                                 "." descriptor
                                 ".backup."
                                 backup-date)))
      (shell-command (concat "cp -rf " stripped-proj-root " " destination))
      (message (concat "backed up project "
                       stripped-proj-root
                       " to "
                       destination))))))

(defun back-up-project-with-descriptor-to-separate-directory (proj
                                                              descriptor
                                                              &optional
                                                              backup-dir)
  (let ((backup-date (format-time-string "%Y-%m-%d-%H-%M-%S"))
        (proj-root (oref proj :path))
        (dot (if (equal "" descriptor) "" "."))
        (backup-dir (if backup-dir backup-dir global-backup-dir)))
    (when (not (or (file-exists-p backup-dir)
                   (file-directory-p backup-dir)))
      (error "backup-dir does not exist"))
    (let ((stripped-proj-root (remove-trailing-slashes-except-root proj-root)))
      (let ((destination (concat backup-dir
                                 (file-name-nondirectory stripped-proj-root)
                                 "."
                                 descriptor
                                 dot
                                 "backup."
                                 backup-date)))
        (print (concat "descriptor:" (format "%s" descriptor)":"))
      (shell-command (concat "cp -rf " stripped-proj-root " " destination))
      (message (concat "backed up project "
                       stripped-proj-root
                       " to "
                       destination))))))

(defun back-up-project (proj)
  (let ((backup-date (format-time-string "%Y-%m-%d-%H-%M-%S"))
        (proj-root (oref proj :path)))
    (let ((stripped-proj-root (remove-trailing-slashes-except-root proj-root)))
      (shell-command (concat "cp -rf " stripped-proj-root " "
                         stripped-proj-root ".backup." backup-date))
      (message (concat "backed up project " stripped-proj-root " to "
                       (concat stripped-proj-root ".backup." backup-date))))))

(defvar global-backup-dir "~/backups/")

(defun remove-trailing-slashes (path)
  (let ((split-str  (split-string path "/")))
    (if (eq "" (car (last split-str)))
        (mapconcat 'identity
                   (cl-subseq split-str 0 (- (length split-str) 1))
                   "/")
      path)))

(defun remove-trailing-slashes-except-root (path)
  (if (equal "/" path)
      path
    (remove-trailing-slashes path)))

(defun launch-terminal ()
  (interactive (let ((proj (cyanide-get-one-by-slot cyanide-current-project
                                                    cyanide-project-collection
                                                    ":id"
                                                    'eq)))
                 ;; (call-process-shell-command "terminator&" nil 0)
                 (call-process-shell-command
                  (concat "cd"
                          " "
                          (oref proj :path)
                          ";"
                          " terminator -m & ") nil 0)
                 nil))) ; return nil

(defun walk-window-buffers (fun &optional minibuf all-frames)
  """ Execute function fun on all buffers in current frame. If minibuf is
  non-nil, execute fun on minibuf as well. If all-frames is non-nil, do this
  across all frames """
  (let ((buffers '()))
    (walk-windows (lambda (win) (push (window-buffer win) buffers))
                  minibuf all-frames)
    (mapcar fun buffers)
    nil))

(defun get-frame-window-buffers (&optional minibuf all-frames)
  """ Return buffers of windows in current frame. If minibuf is non-nil, return
  minibuf as well. If all-frames is non-nil, do this across all frames. """
  (let ((bufs '()))
    (walk-window-buffers (lambda (buf) (push buf bufs)) minibuf all-frames)
    bufs))

(defun buffer-visible (buf-or-name &optional minibuf all-frames)
  """ Return a true-ish value from `member' if buf-or-name is not backgrounded,
  else return nil. If arg minibuf is non-nil, consider whether buf-or-name
  matches minibuf. If arg all-frames is non-nil, do this comparison across all
  frames """
  (let ((bufs (get-frame-window-buffers minibuf all-frames)))
    (if (stringp buf-or-name)
        (let ((buf-names (mapcar 'buffer-name bufs)))
          (member buf-or-name buf-names))
      (if (bufferp buf-or-name)
          (member buf-or-name bufs)
        ;; if buf-or-name is neither buffer nor string:
        (error (concat "Wrong type argument: stringp or bufferp: "
                       (format "%s" buf-or-name)))))))

(defmacro all-projects ()
  (append '(concat) (interleave
                     " "
                     (mapcar (lambda (x) (oref x :path))
                             cyanide-project-collection))))

(defmacro construct-ls-all-projects () `(concat "ls " ,(all-projects)))

(defmacro exec-ls-all-projects () `(shell-command ,(construct-ls-all-projects)))

(defun dired-proj-root ()
  (interactive)
  (dired-proj-root-1)
  nil)

(defun dired-proj-root-1 ()
  (dired (cyanide-project-oref :path))
  nil)

;; some of this, like the after-save-hook, should go into js2-mode config, to
;; avoid running js formatting on non-js files.
(defun use-semistandard-style ()
  "need to if def each of these, to handle case where the var being set to
   -orig is undefined. This was causing this func to error out."
  (progn
    (message "Loading semistandard style...")
    (when (boundp 'tab-width)
      (setq-default tab-width-orig tab-width))
    (when (boundp 'js2-basic-offset)
      (setq-default js2-basic-offset-orig js2-basic-offset))
    (when (boundp 'indent-tabs-mode)
      (setq indent-tabs-mode-orig indent-tabs-mode))
    (when (boundp 'fill-column)
      (setq fill-column-orig fill-column))
    (setq-default tab-width 2)
    (setq js2-basic-offset 2)

    (add-hook 'js2-mode-hook
              (function (lambda ()
                          (setq js2-basic-offset 2))))

    (add-hook 'js2-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          'format-file-with-semistandard
                          nil
                          'make-it-local)))

    (add-hook 'js2-mode-hook
              (lambda ()
                (setq-local indent-line-function 'tab-to-tab-stop)))

    (add-hook 'js2-mode-hook
              'disable-spacemacs-js2-autoindent)

    (add-to-list 'spacemacs-indent-sensitive-modes 'js2-mode)

    (setq-default indent-tabs-mode nil)
    (setq-default fill-column 80)))

(defun use-sqlformat-style ()
  "lint sql files with sqlformat"
  (message "Loading sqlformat style...")

  (add-hook 'sql-mode-hook
            (lambda ()
              (add-hook 'after-save-hook
                        'lint-file-with-sqlformat
                        nil
                        'make-it-local))))

(defun disable-spacemacs-js2-autoindent ()
  (setq return-orig (lookup-key js2-mode-map (kbd "<return>")))
  (define-key js2-mode-map (kbd "<return>") 'newline))

(defun tear-down-semistandard-style ()
  (progn
    (message "Tearing down semistandard style...")
    
    (when (boundp 'tab-width-orig)
      (setq-default tab-width tab-width-orig))

    (when (boundp 'js2-basic-offset-orig)
      (setq-default js2-basic-offset js2-basic-offset-orig))

    (remove-hook 'js2-mode-hook
                 (function (lambda ()
                             (setq js2-basic-offset 2))))
    (remove-hook 'js2-mode-hook
                 (lambda ()
                   (add-hook 'after-save-hook
                             'format-file-with-semistandard
                             nil
                             'make-it-local)))

    (when (boundp 'indent-tabs-mode-orig)
      (setq-default indent-tabs-mode indent-tabs-mode-orig))

    (when (boundp 'fill-column-orig)
      (setq-default fill-column fill-column-orig))

    ;; re-enable spacemacs autoindentation
    (remove-hook 'js2-mode-hook
                 'disable-spacemacs-js2-autoindent)
    (delq 'js2-mode spacemacs-indent-sensitive-modes)
    (when (bound-and-true-p return-orig)
      (define-key js2-mode-map (kbd "<return>") return-orig))))

(defun lint-file-with-semistandard ()
  (interactive)
  (save-some-buffers)
  (async-shell-command
   (concat "semistandard " buffer-file-name)))

(defun format-file-with-semistandard ()
  (interactive)
  (format-file-with-semistandard-1))

(defun format-file-with-semistandard-1 ()
  (save-some-buffers)
  (shell-command
   (concat "semistandard --fix " buffer-file-name)
   "*semistandard-output*"
   "*semistandard-errors*")
  (revert-buffer nil t t))

(defun use-semistandard-and-prettier-styles ()
  "need to if def each of these, to handle case where the var being set to
   -orig is undefined. This was causing this func to error out."
  (progn
    (message "Loading semistandard and prettier styles...")
    (when (boundp 'tab-width)
      (setq-default tab-width-orig tab-width))
    (when (boundp 'js2-basic-offset)
      (setq-default js2-basic-offset-orig js2-basic-offset))
    (when (boundp 'indent-tabs-mode)
      (setq indent-tabs-mode-orig indent-tabs-mode))
    (when (boundp 'fill-column)
      (setq fill-column-orig fill-column))
    (setq-default tab-width 2)
    (setq js2-basic-offset 2)

    (add-hook 'js2-mode-hook
              (function (lambda ()
                          (setq js2-basic-offset 2))))

    (add-hook 'js2-mode-hook
              (lambda ()
                (add-hook 'after-save-hook
                          'format-file-with-semistandard-and-prettier
                          nil
                          'make-it-local)))

    (add-hook 'js2-mode-hook
              (lambda () (setq-local indent-line-function 'tab-to-tab-stop)))

    (add-hook 'js2-mode-hook
              'disable-spacemacs-js2-autoindent)

    (add-to-list 'spacemacs-indent-sensitive-modes 'js2-mode)

    (setq-default indent-tabs-mode nil)
    (setq-default fill-column 80)))

(defun tear-down-semistandard-and-prettier-styles ()
  (progn
    (message "Tearing down semistandard and prettier styles...")

    (when (boundp 'tab-width-orig)
      (setq-default tab-width tab-width-orig))

    (when (boundp 'js2-basic-offset-orig)
      (setq-default js2-basic-offset js2-basic-offset-orig))

    (remove-hook 'js2-mode-hook
                 (function (lambda ()
                             (setq js2-basic-offset 2))))
    (remove-hook 'js2-mode-hook
                 (lambda ()
                   (add-hook 'after-save-hook
                             'format-file-with-semistandard-and-prettier
                             nil
                             'make-it-local)))

    (when (boundp 'indent-tabs-mode-orig)
      (setq-default indent-tabs-mode indent-tabs-mode-orig))

    (when (boundp 'fill-column-orig)
      (setq-default fill-column fill-column-orig))

    ;; re-enable spacemacs autoindentation
    (remove-hook 'js2-mode-hook
                 'disable-spacemacs-js2-autoindent)
    (delq 'js2-mode spacemacs-indent-sensitive-modes)
    (when (bound-and-true-p return-orig)
      (define-key js2-mode-map (kbd "<return>") return-orig))))

(defun format-file-with-semistandard-and-prettier ()
  (interactive)
  (format-file-with-semistandard-and-prettier-1))

(defun format-file-with-semistandard-and-prettier-and-prompt ()
  (interactive)
  (let ((result
         (read-string (format "%s%s"
                              "Are you sure you would like to reformat "
                              "this file? (y or n) "))))
    (if (equal "y" result)
        (format-file-with-semistandard-and-prettier-1)
      (if (equal "n" result)
          nil
        (error "invalid input")))))

(defun format-file-with-semistandard-and-prettier-1 ()
  (save-some-buffers)
  (shell-command
   (concat "prettier --write " buffer-file-name)
   "*prettier-output*"
   "*prettier-errors*")
  (shell-command
   (concat "semistandard --fix " buffer-file-name)
   "*prettier-output*"
   "*prettier-errors*")
  (revert-buffer nil t t))

(defun save-buffer-without-hooks ()
    (interactive)
  (let ((after-save-hook nil))
    (save-buffer)))

(defun node (&optional buffer-name)
  (interactive)
  """
Custom node interpreter.
"""
(progn (cd-proj-root)
       (when (not (bound-and-true-p buffer-name)) (setq buffer-name "*node*"))
       (kill-buffer-if-exists buffer-name)
       (shell buffer-name)
       (insert "nvm use 8 ; node")
       (comint-send-input)))

(defun node-debugger-connect (&optional buffer-name)
  (interactive)
  (when (not (bound-and-true-p buffer-name))
    (setq buffer-name "*node-debugger*"))
  (progn (cd-proj-root)
         (kill-buffer-if-exists buffer-name)
         (shell buffer-name)
         (insert "nvm use 6 ; node debug localhost:5858")
         (comint-send-input)))

(defun json-lint-file ()
  (interactive)
  (json-lint-file-1))

(defun json-lint-file-1 ()
  (save-some-buffers)
  (shell-command
   (concat "cat "
           buffer-file-name
           " |json_verify")
   "*json-verify-output*"
   "*json-verify-errors*"))

(defvar symmetric-wrap-matches '(("(" ")")
                                 ("{" "}")
                                 ("\"" "\"")
                                 ("[" "]")
                                 ("/*" "*/")
                                 ("<" ">")
                                 ("'" "'")))

(defun wrap (&optional prefix suffix)
  (interactive)
  (when (eq nil prefix)
    (setq prefix (read-string "prefix: ")))
  (when (eq nil suffix)
    (setq suffix (read-string "suffix: " (assocadr prefix symmetric-wrap-matches))))
  (if (region-active-p)
      (let ((beginning (region-beginning))
            (end (region-end)))
        (progn
          (goto-char end)
          (insert suffix)
          (goto-char beginning)
          (insert prefix)))
    (progn
      (insert prefix)
      (insert suffix)
      (backward-char))))

(defun wrap-json-stringify (&optional beginning end)
  (interactive)
  (wrap "console.log(JSON.stringify(" ", null, 2));"))

(defvar sqlformat-excludes '()
  "list of strings to exclude from sqlformat output")

(defun format-exclude-clause (excludes)
  (let ((value ""))
    (dolist (element excludes value)
      (setq value (concat value " |grep -v " element)))))

(defun lint-file-with-sqlformat ()
  (interactive)
  (save-some-buffers)
  (let ((exclude-clause (format-exclude-clause sqlformat-excludes)))
    (shell-command
     (format "%s%s%s%s%s%s%s%s%s%s"
             "sqlformat "
             "--identifiers lower "
             "--keywords upper "
             (buffer-file-name)
             "|diff -W 160 -y "
             (buffer-file-name)
             " - "
             " |grep '|' "
             exclude-clause
             "; echo 0")
     "*sqlformat-output*"
     "*sqlformat-errors*")))

(defun awslogs ()
  (interactive)
  (async-shell-command (read-string "" "awslogs get '/aws/' --start='1 hour'")))

(defun ephemeral-buffer ()
  (interactive)
  (switch-to-buffer "*ephemeral*")
  (setq make-backup-files nil)
  (fset 'save-buffer (lambda () (interactive)
                       (message "ephemeral buffer: skipped save"))))

(defun cyanide-project-initialize ()
  (interactive)
  (let ((proj-path (directory-file-name (read-file-name "project directory: ")))
        (view (read-string "default-view: "))
        (name nil)
        init-path)
    (setq name (file-name-nondirectory proj-path))
    (setq init-path (concat proj-path "/.cy/init.el" ))
    (write-file init-path)
    (switch-to-buffer "*tmp*")
    (insert (print `(cyanide-project :id (quote ,(intern name))
                                     :display-name ,name
                                     :default-view (quote ,(intern view)))))
    (kill-buffer "*tmp*")))

(provide 'defuns)
