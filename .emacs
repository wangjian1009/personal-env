;;; { globl setting

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-goodies-el")

(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
(setq default-tab-width 4)
 
(ansi-color-for-comint-mode-on)
(global-font-lock-mode t)
(transient-mark-mode t)
(setq search-highlight t)  
(setq query-replace-highlight t)
 
(setq-default kill-whole-line t)
(setq inhibit-startup-message t)
 
(setq-default case-fold-search nil)
(setq-default case-replace nil)
 
(show-paren-mode t)
(setq show-paren-stype 'parentheses)
(setq uniquify-buffer-name-style 'forward)
(fset 'yes-or-no-p 'y-or-n-p)
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
(setq enable-recursive-minibuffers t)
(setq-default indent-tabs-mode nil)
 
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)

(global-set-key (kbd "ESC C-l") 'revert-buffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c f") 'ff-find-other-file)

;;; }
;;; { cygwin support

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
(let* ((cygwin-root "f:/cygwin")
       (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))
    
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    
    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))
    
    ;; NT-emacs assumes a Windows shell. Change to baash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)
    
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))
(when (eq 'windows-nt system-type)
  (require 'cygwin-mount)
  (setq cygwin-mount-table
        '(
          ("F:\\cygwin" . "/")
          ("F:\\cygwin\\bin" . "/usr/bin/")))
  (cygwin-mount-activate))

;;; }
;;; { ido mode setup

(require 'ido)
(ido-mode t)
(setq ido-case-fold t)
(add-hook 'ido-define-mode-map-hook 'ido-my-keys)
(defun ido-my-keys ()
  "Set up the keymap for `ido'."
 
  ;; common keys
  (define-key ido-mode-map "\C-e" 'ido-edit-input) 
  (define-key ido-mode-map "\t" 'ido-complete) ;; complete partial
  (define-key ido-mode-map "\C-j" 'ido-select-text)
  (define-key ido-mode-map "\C-m" 'ido-exit-minibuffer)
  (define-key ido-mode-map "?" 'ido-completion-help) ;; list completions
  (define-key ido-mode-map [(control ? )] 'ido-restrict-to-matches)
  (define-key ido-mode-map [(control ?@)] 'ido-restrict-to-matches)
 
  ;; cycle through matches
  (define-key ido-mode-map "\C-r" 'ido-prev-match)
  (define-key ido-mode-map "\C-s" 'ido-next-match)
  (define-key ido-mode-map [right] 'ido-next-match)
  (define-key ido-mode-map [left] 'ido-prev-match)
 
  ;; toggles
  (define-key ido-mode-map "\C-t" 'ido-toggle-regexp) ;; same as in isearch
  (define-key ido-mode-map "\C-p" 'ido-toggle-prefix)
  (define-key ido-mode-map "\C-c" 'ido-toggle-case)
  (define-key ido-mode-map "\C-a" 'ido-toggle-ignore)
 
  ;; keys used in file and dir environment
  (when (memq ido-cur-item '(file dir))
    (define-key ido-mode-map "\C-b" 'ido-enter-switch-buffer)
    (define-key ido-mode-map "\C-d" 'ido-enter-dired)
    (define-key ido-mode-map "\C-f" 'ido-fallback-command)
 
    ;; cycle among directories
    ;; use [left] and [right] for matching files
    (define-key ido-mode-map [down] 'ido-next-match-dir)
    (define-key ido-mode-map [up]   'ido-prev-match-dir)
 
    ;; backspace functions
    (define-key ido-mode-map [backspace] 'ido-delete-backward-updir)
    (define-key ido-mode-map "\d"        'ido-delete-backward-updir)
    (define-key ido-mode-map [(meta backspace)] 'ido-delete-backward-word-updir)
    (define-key ido-mode-map [(control backspace)] 'ido-up-directory)
 
    ;; I can't understand this
    (define-key ido-mode-map [(meta ?d)] 'ido-wide-find-dir)
    (define-key ido-mode-map [(meta ?f)] 'ido-wide-find-file)
    (define-key ido-mode-map [(meta ?k)] 'ido-forget-work-directory)
    (define-key ido-mode-map [(meta ?m)] 'ido-make-directory)
 
    (define-key ido-mode-map [(meta down)] 'ido-next-work-directory)
    (define-key ido-mode-map [(meta up)] 'ido-prev-work-directory)
    (define-key ido-mode-map [(meta left)] 'ido-prev-work-file)
    (define-key ido-mode-map [(meta right)] 'ido-next-work-file)
 
    ;; search in the directories
    ;; use C-_ to undo this
    (define-key ido-mode-map [(meta ?s)] 'ido-merge-work-directories)
    (define-key ido-mode-map [(control ?\_)] 'ido-undo-merge-work-directory)
    )
 
  (when (eq ido-cur-item 'file)
    (define-key ido-mode-map "\C-k" 'ido-delete-file-at-head)
    (define-key ido-mode-map "\C-l" 'ido-toggle-literal)
    (define-key ido-mode-map "\C-o" 'ido-copy-current-word)
    (define-key ido-mode-map "\C-v" 'ido-toggle-vc)
    (define-key ido-mode-map "\C-w" 'ido-copy-current-file-name)
    )
 
  (when (eq ido-cur-item 'buffer)
    (define-key ido-mode-map "\C-b" 'ido-fallback-command)
    (define-key ido-mode-map "\C-f" 'ido-enter-find-file)
    (define-key ido-mode-map "\C-k" 'ido-kill-buffer-at-head)
    ))

;;; }
;;; { ibuffer settings
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; }
;;; { color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-classic)
;;; }
;;; { folding
(require 'folding)
(setq folding-fold-on-startup t)
(folding-mode-add-find-file-hook)
;;; }
;;; { git-emacs
(add-to-list 'load-path "~/.emacs.d/site-lisp/git-emacs")
(require 'git-emacs)
;;; }
;;; { markdown mode
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
;;; }
;;; { backup settings
(setq-default make-backup-files nil)
(setq backup-directory-alist '(("" . "~/emacs.d/autosave")))
(auto-save-mode nil)
(setq delete-auto-save-files t)
;;; }
;;; { personal yaml model

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; }
;;; { personal lisp mode settings

(add-hook 'emacs-lisp-mode-hook (lambda () (folding-mode t)))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'comment-region)
(folding-add-to-marks-list 'emacs-lisp-mode ";;; {" ";;; }" "")

;;; }
;;; { personal c cpp and objc mode settings

(require `find-file)
(add-to-list ff-other-file-alist '("\\.m\\'" (".h")))
(add-to-list ff-other-file-alist '("\\.h\\'" (".m" ".c")))
(add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))
 
(defun personal-c-cpp-setup()
  ;(c-toggle-auto-state)
  (c-toggle-hungry-state t)
  (which-function-mode t)
  (c-set-style "stroustrup")
  (setq c-basic-offset 4)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'friend '-)
  (c-set-offset 'innamespace -80)
  (c-set-offset 'namespace-close -80)
  (c-set-offset 'block-open -4)
  (c-set-offset 'template-args-cont '+)
;  (gtags-mode t)
  )
 
(add-hook 'c-mode-hook 'personal-c-cpp-setup)
(add-hook 'c++-mode-hook 'personal-c-cpp-setup)
(add-hook 'objc-mode-hook 'personal-c-cpp-setup)

;;; }
;;; { personal make setup

(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))

(folding-add-to-marks-list 'makefile-gmake-mode "# {{{ " "# }}}" nil)
(add-hook 'makefile-gmake-mode-hook
          (lambda ()
            (folding-mode t)))

;;; }
;;; { personal perl mode settings
(defvar perl-syntax-bin "perl" "The perl binary used to check syntax.")
(defun perl-syntax-check-only ()
  "Return a either nill or t depending on whether the current buffer passws perl`s syntax check."
  (interactive)
  (let ((buf (get-buffer-create "*Perl syntax check*")))
    (let ((syntax-ok (= 0 (save-excursion
                           (widen)
                           (call-process-region
                            (point-min) (point-max) perl-syntax-bin nil buf nil "-c")))))
      (if syntax-ok (kill-buffer buf)
        (display-buffer buf)))))
 
(defvar perl-syntax-mode nil "Check perl syntax before saving.")
(make-variable-buffer-local 'perl-syntax-mode)
(defun perl-syntax-write-hook ()
  "Check perl syntax during 'write-file-hooks' for 'perl-syntax-mode'"
  (if perl-syntax-mode
      (save-excursion
       (widen)
       (mark-whole-buffer)
       (not (perl-syntax-check-only)))
    nil))
 
(defun perl-syntax-mode (&optional arg)
  "Perl syntax checking minor mode."
  (interactive "p")
  (setq perl-syntax-mode
        (if (null arg)
            (not perl-syntax-mode)
          (> (prefix-numeric-value arg) 0)))
  (make-local-hook 'write-file-hooks)
  (if perl-syntax-mode
      (add-hook 'write-file-hooks 'perl-syntax-write-hook)
    (remove-hook 'write-file-hooks 'perl-syntax-write-hook)))
 
(if (not (assq 'perl-syntax-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(perl-syntax-mode " Perl Syntax")
                minor-mode-alist)))
 
(defun perl-eval ()
  "Run selected buf as Perl code"
  (interactive)
  (save-excursion
    (widen)
    (shell-command-on-region (point-min) (point-max) "perl")))
 
(eval-after-load "cperl-mode"
  '(progn
     (define-key cperl-mode-map (kbd "C-x C-e") 'perl-eval)
     (define-key cperl-mode-map (kbd "C-c f") 'perldoc-find-module)
     (define-key cperl-mode-map (kbd "M-p") 'cperl-perldoc)
     (define-key cperl-mode-map (kbd "C-c C-c") 'comment-region)
 
     (setq cperl-auto-newline nil)
     (setq cperl-auto-newline-after-colon nil)
     (setq cperl-electric-parens nil)
   
     (defun my-perl-mode()
       )
 
     (add-hook 'cperl-mode-hook 'my-perl-mode)
     (add-hook 'cperl-mode-hook 'perl-syntax-mode)
     )
  )
 
(setq auto-mode-alist
      (cons '("\\.\\(pl\\|pm\\)\\'" . cperl-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("perl" . cperl-mode)
            (cons '("perl5" . cperl-mode) interpreter-mode-alist)))
 
;;; }
;;; { hippie-expand settings
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;;; }
;;; { utility go-to-cha
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
                     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
 
(define-key global-map (kbd "C-c C-g") 'wy-go-to-char)
;;; }
;;; { utility match-parten
(global-set-key "%" 'match-paren)
        
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
;;; }
;;; { utility ascii table

(defun ascii-table-show ()
  "Print the ascii table"
  (interactive)
  (switch-to-buffer "*ASCII table*")
  (erase-buffer)
  (let ((i   0)
        (tmp 0))
    (insert (propertize
             "                                [ASCII table]\n\n"
             'face font-lock-comment-face))
    (while (< i 32)
      (dolist (tmp (list i (+ 32 i) (+ 64 i) (+ 96 i)))
        (insert (concat
                 (propertize (format "%3d " tmp)
                             'face font-lock-function-name-face)
                 (propertize (format "[%2x]" tmp)
                             'face font-lock-constant-face)
                 "      "
                 (propertize (format "%3s" (single-key-description tmp))
                             'face font-lock-string-face)
                 (unless (= tmp (+ 96 i))
                   (propertize " | " 'face font-lock-variable-name-face)))))
      (newline)
      (setq i (+ i 1)))
    (beginning-of-buffer))
  (local-set-key "q" 'kill-this-buffer)
  (toggle-read-only 1)
  (message "Press q to quit."))

;;; }
;;; { run server
(require 'server)
(server-start)
;;; }
