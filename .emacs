;;; { globl setting

(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path (expand-file-name "setup" user-emacs-directory))

(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(require 'alist)

(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
(setq default-tab-width 4)
(setq visible-bell t)                ; 关闭出错时的提示声
 
(ansi-color-for-comint-mode-on)
(global-font-lock-mode t)
(transient-mark-mode t)
(setq search-highlight t)  
(setq query-replace-highlight t)

(setq-default tab-width 4)
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

(setq password-cache-expiry nil)

(global-set-key (kbd "ESC C-l") 'revert-buffer)
(global-set-key (kbd "ESC C-M-l") 'revert-buffer-with-coding-system)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c f") 'ff-find-other-file)

(require `edit-env)

;;; }
;;; { support functions or macros

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

;;; }
;;; { apple support

(when (eq 'darwin system-type)
  (setq mac-command-modifier 'meta) ;映射苹果键  
  (setq mac-control-modifier 'control) ;映射Ctrl键
  (setq mac-option-modifier 'control) ;映射Alt键 

  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (concat "/usr/local/lib:" (getenv "LD_LIBRARY_PATH")))
  (setenv "INFOPATH" (concat "/usr/local/info:/usr/local/share/info:/usr/info:/usr/share/info:" (getenv "INFOPATH")))
  (setenv "MANPATH" (concat "/usr/local/man:/usr/local/share/man:/usr/man:/usr/share/man:" (getenv "MANPATH")))
  (add-to-list 'exec-path "/usr/local/bin")

  (let ((home-dir (getenv "HOME")))
    (if home-dir
        (progn
          (setenv "PATH" (concat home-dir "/bin:" (getenv "PATH")))
          (setenv "LD_LIBRARY_PATH" (concat home-dir "/lib:" (getenv "LD_LIBRARY_PATH")))
          (setenv "PERL5PATH" (concat home-dir "/lib:" (getenv "LD_LIBRARY_PATH")))
          (add-to-list 'exec-path (concat home-dir "/bin"))
          )
      ))

  (eval-after-load "ido" '(add-to-list 'ido-ignore-files "\\.DS_Store"))

  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)
)

;;; }
;;; { cygwin support

;; Sets your shell to use cygwin's bash, if Emacs finds it's running
;; under Windows and c:\cygwin exists. Assumes that C:\cygwin\bin is
;; not already in your Windows Path (it generally should not be).
;;
(let* ((cygwin-root (getenv "CYGWIN_ROOT"))
       (cygwin-bin (concat cygwin-root "/bin"))
       (cygwin-local-bin (concat cygwin-root "/local/bin"))
       )

  (when (and (eq 'windows-nt system-type)
             (file-readable-p cygwin-root))
    
    (setq exec-path (cons cygwin-bin exec-path))
    (setenv "PATH" (concat cygwin-local-bin ";" cygwin-bin ";" (getenv "PATH")))
    
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
  (cygwin-mount-activate)

  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (concat "/usr/local/lib:" (getenv "LD_LIBRARY_PATH")))
  (add-to-list 'exec-path "/usr/local/bin")

  (let ((home-dir (getenv "HOME")))
    (if home-dir
        (progn
          (cd home-dir)
          (setenv "PATH" (concat home-dir "/bin;" (getenv "PATH")))
          (add-to-list 'exec-path (concat home-dir "/bin"))
          )
      ))

  (setenv "PATH" (shell-command-to-string (concat "cygpath -p '" (getenv "PATH") "'")))
  )

;;; }
;;; { coding-system

;(add-to-list `process-coding-system-alist `("bash" . gb2312))
;(add-to-list `process-coding-system-alist `("git*" . gb2312))
;(add-to-list 'file-coding-system-alist '("\\.[hc]\\(pp\\)?\\'" .gb2312 ))

(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)

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
;;; { imenu-anywhere

(require 'imenu-anywhere)
(global-set-key (kbd "C-c i") 'ido-imenu-anywhere)

;;; }
;;; { ibuffer settings

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(define-key-after ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)

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
;;; { yafolding

(require 'yafolding)
;; (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
;; (define-key yafolding-mode-map (kbd "<C-M-return>") nil)
;; (define-key yafolding-mode-map (kbd "<C-return>") nil)
;; (define-key yafolding-mode-map (kbd "C-c <C-M-return>") 'yafolding-toggle-all)
;; (define-key yafolding-mode-map (kbd "C-c <C-S-return>") 'yafolding-hide-parent-element)
;; (define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element)

;;; }
;;; { fastnav

(require 'fastnav)
(global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
(global-set-key "\M-s" 'fastnav-jump-to-char-forward)
(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
(global-set-key "\M-r" 'fastnav-replace-char-forward)
(global-set-key "\M-R" 'fastnav-replace-char-backward)
(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
(global-set-key "\M-k" 'fastnav-delete-char-forward)
(global-set-key "\M-K" 'fastnav-delete-char-backward)
(global-set-key "\M-m" 'fastnav-mark-to-char-forward)
(global-set-key "\M-M" 'fastnav-mark-to-char-backward)
(global-set-key "\M-p" 'fastnav-sprint-forward)
(global-set-key "\M-P" 'fastnav-sprint-backward)

;;; }
;;; { gtags

(if (file-accessible-directory-p "/usr/local/share/gtags")
    (progn ()
           (add-to-list 'load-path "/usr/local/share/gtags")
           (let ((gtags-suggested-key-mapping t)) (require 'gtags))
           (define-key gtags-mode-map (kbd "C-c f") 'ff-find-other-file)
           (cond
            ((eq system-type 'windows-nt)
             (setq gtags-global-command "/usr/local/bin/global.exe"))
            (t
             (setq gtags-global-command "/usr/local/bin/global")
             ))
           ))

;;; }
;;; { git-emacs

(require 'git-emacs)
(require 'git-timemachine)
(require 'git-gutter)

;;; }
;;; { markdown mode

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.\\(md\\|markdown\\)\\'" . markdown-mode) auto-mode-alist))

;;; }
;;; { backup settings

(setq-default make-backup-files nil)
(setq backup-directory-alist '(("" . "~/emacs.d/autosave")))
(auto-save-mode nil)
(setq delete-auto-save-files t)

;;; }
;;; { smex model

(require 'smex); Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;; }
;;; { yasnippet model

(require 'dropdown-list)
(require 'setup-yasnippet)
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '(".emacs.d/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;;; }
;;; { expand-region

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; }
;;; { flycheck-mode
(autoload 'flycheck-mode "setup-flycheck" nil t)
;;; }
;;; { company-mode

(autoload 'company-mode "company" nil t)
(eval-after-load "company"
  '(progn
     (define-key company-mode-map (kbd "C-c C-.") 'company-complete-common)
     (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
     (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
     )
  )

;;; }
;;; { mmm-mode

(require 'mmm-vars)
(require 'mmm-auto)
(require 'mmm-sample)

(setq mmm-global-mode 'maybe)

;;; }
;;; { font setting...

(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size) 
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                         ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))
 
    ;; Set the default English font
    ;; 
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute
     'default nil :font en-font)
 
    ;; Set Chinese font 
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh-font))))

(qiang-set-font
 '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=14"
 '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

;;; }
;;; { personal yaml model

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(define-key yaml-mode-map (kbd "C-c C-c") 'comment-region)
(add-hook 'yaml-mode-hook (lambda () (yafolding-mode)))

;;; }
;;; { personal dockerfile model

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;;; }
;;; { personal nxml model

(eval-after-load "nxml-mode"
  '(progn
     (setq nxml-child-indent 4)
     )
  )

;;; }
;;; { personal lisp mode settings

(add-hook 'emacs-lisp-mode-hook (lambda () (folding-mode t)))
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'comment-region)

(folding-add-to-marks-list 'emacs-lisp-mode ";;; {" ";;; }" "")

;;; }
;;; { personal c cpp and objc mode settings

(require `find-file)

(mapcar (lambda (x) (add-to-list ff-other-file-alist x))
        '(("\\.mm?$" (".h"))
          ("\\.cc$"  (".hh" ".h"))
          ("\\.hh$"  (".cc" ".C"))
          ("\\.c$"   (".h"))
          ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))
          ("\\.C$"   (".H"  ".hh" ".h"))
          ("\\.H$"   (".C"  ".CC"))
          ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
          ("\\.HH$"  (".CC"))
          ("\\.cxx$" (".hh" ".h"))
          ("\\.cpp$" (".hpp" ".hh" ".h"))
          ("\\.hpp$" (".cpp" ".c"))))

(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
; (add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n\\(class\\|namespace\\)" . c++-mode))
; (add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))

(add-hook 'c-mode-hook (lambda () (yafolding-mode t)))
(add-hook 'c++-mode-hook (lambda () (yafolding-mode t)))
(add-hook 'objc-mode-hook (lambda () (yafolding-mode t)))

;; ===== Opening header files =====
;; Allows to choose between objc-mode, c++-mode and c-mode
(defun loki-choose-header-mode ()
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (progn
        (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
              (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
          (if (file-exists-p dot-m-file) (progn (objc-mode))
            (if (file-exists-p dot-cpp-file) (c++-mode)
              )
            )
          )
        )
    )
  )

(add-hook 'find-file-hook 'loki-choose-header-mode)

(defun personal-c-cpp-setup()
  ;(c-toggle-auto-state)
  (c-toggle-hungry-state t)
  (which-function-mode t)
  (c-set-style "stroustrup")
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (c-set-offset 'inline-open 0)
  (c-set-offset 'friend '-)
  (c-set-offset 'innamespace -80)
  (c-set-offset 'namespace-close -80)
  (c-set-offset 'block-open -4)
  (c-set-offset 'template-args-cont '+)
  )
 
(add-hook 'c-mode-hook 'personal-c-cpp-setup)
(add-hook 'c++-mode-hook 'personal-c-cpp-setup)
(add-hook 'objc-mode-hook 'personal-c-cpp-setup)

(add-hook 'c-mode-hook 'git-gutter-mode)
(add-hook 'c++-mode-hook 'git-gutter-mode)
(add-hook 'objc-mode-hook 'git-gutter-mode)

(eval-after-load "gtags"
  '(progn
     (setenv "GTAGSFORCECPP" "1")
     (add-hook 'c-mode-hook (lambda () (gtags-mode t)))
     (add-hook 'c++-mode-hook (lambda () (gtags-mode t)))
     (add-hook 'objc-mode-hook (lambda () (gtags-mode t)))
     ))

;;; }
;;; { personal java mode settings

(defvar java-bin nil "java executabale")
(cond
 ((eq system-type 'windows-nt)
  (setq java-bin (expand-file-name "bin/java.exe" (getenv "JAVA_HOME"))))
 (t
  (setq java-bin "java")
  ))

(autoload 'meghanada-mode "meghanada.el" nil t)
(eval-after-load "meghanada"
  '(progn
     (setq meghanada-java-path java-bin)
     (cond
      ((eq system-type 'windows-nt)
       (setq meghanada-maven-path (expand-file-name "bin/mvn.cmd" (getenv "MVN_HOME"))))
      (t
       (setq meghanada-maven-path "mvn")))
     ))

(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
            ;; use code format
            ;; (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)
            ))

;;; }
;;; { personal kotlin mode settings

(autoload 'kotlin-mode "kotlin-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))

;;; }
;;; { personal go model

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(autoload 'go-guru-hl-identifier-mode "go-guru" nil t)
(defun personal-go-setup()
  (go-guru-hl-identifier-mode)
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq tab-width 4)
  )
(add-hook 'go-mode-hook 'personal-go-setup)

(eval-after-load "go-mode"
  '(progn
     (define-key go-mode-map (kbd "C-c C-g") 'go-goto-map)
     (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
     (define-key go-mode-map (kbd "C-c C-f") 'gofmt)
     (define-key go-mode-map (kbd "C-c C-k") 'godoc)
     (define-key go-mode-map (kbd "C-c C-c") 'comment-region)
     (define-key go-mode-map (kbd "M-.") 'godef-jump)
     )
  )

;;; }
;;; { personal make setup

(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))

(folding-add-to-marks-list 'makefile-gmake-mode "# {{{ " "# }}}" nil)
(add-hook 'makefile-gmake-mode-hook
          (lambda ()
            (folding-mode t)))

;;; }
;;; { personal csharp mode setup

(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist (cons '("\\.cs$" . csharp-mode) auto-mode-alist))

;; Patterns for finding Microsoft C# compiler error messages:
(require 'compile)
(push '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): error" 1 2 3 2) compilation-error-regexp-alist)
(push '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): warning" 1 2 3 1) compilation-error-regexp-alist)

;; Patterns for defining blocks to hide/show:
(push '(csharp-mode
	"\\(^\\s *#\\s *region\\b\\)\\|{"
	"\\(^\\s *#\\s *endregion\\b\\)\\|}"
	"/[*/]"
	nil
	hs-c-like-adjust-block-beginning)
      hs-special-modes-alist)

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
  (make-local-variable 'write-file-hooks)
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
      (cons '("\\.\\(pl\\|pm\\|pod\\)\\'" . cperl-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("perl" . cperl-mode)
            (cons '("perl5" . cperl-mode) interpreter-mode-alist)))

(folding-add-to-marks-list 'cperl-mode "# {{{ " "# }}}" nil)

;;; }
;;; { personal html mode settings
(require 'setup-html-mode)
;;; }
;;; { personal php mode

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)

(require 'php-mode)

(eval-after-load "php-mode"
  '(progn
     (require 'php-auto-yasnippets)
     ;(setq php-auto-yasnippet-php-program "~/path/to/Create-PHP-YASnippet.php")

     (define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

     (add-hook 'php-mode-hook
               (lambda ()
                 ))
     )
  )

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode "\\.php[s345t]?\\'" html-php))
(add-to-list 'auto-mode-alist '("\\.php[s345t]?\\'" . html-mode))

;;; }
;;; { personal json mode
(require 'json-mode)
;;; }
;;; { personal sql mode

(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end (concat java-bin " -jar " (getenv "HOME") "/.emacs.d/script/sqlbeautify/SqlBeautify-1.0.jar") nil t)))
    ;; change sqlbeautify to anbt-sql-formatter if you
    ;;ended up using the ruby gem

(defun sql-beautify-buffer ()
 "Beautify SQL in buffer."
 (interactive)
 (sql-beautify-region (point-min) (point-max)))

(defun sql-beautify-region-or-buffer ()
  "Beautify SQL for the entire buffer or the marked region between beg and end"
  (interactive)
  (if (use-region-p)
      (sql-beautify-region (region-beginning) (region-end))
    (sql-beautify-buffer)))

(eval-after-load "sql-mode"
  '(progn
     (load-library "sql-indent")

     (defvar sql-last-prompt-pos 1
       "position of last prompt when added recording started")
     (make-(vector )ariable-buffer-local 'sql-last-prompt-pos)
     (put 'sql-last-prompt-pos 'permanent-local t)
     
     (defun sql-add-newline-first (output)
       "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'
    This fixes up the display of queries sent to the inferior buffer
    programatically."
       (let ((begin-of-prompt
              (or (and comint-last-prompt-overlay
                       ;; sometimes this overlay is not on prompt
                       (save-excursion
                         (goto-char (overlay-start comint-last-prompt-overlay))
                         (looking-at-p comint-prompt-regexp)
                         (point)))
                  1)))
         (if (> begin-of-prompt sql-last-prompt-pos)
             (progn
               (setq sql-last-prompt-pos begin-of-prompt)
               (concat "\n" output))
           output)))
     
     (defun sqli-add-hooks ()
       "Add hooks to `sql-interactive-mode-hook'."
       (add-hook 'comint-preoutput-filter-functions 'sql-add-newline-first))
     
     (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)

     (define-key sql-mode-map "\C-\M-\\" 'sql-beautify-region-or-buffer)
     (define-key sql-interactive-mode-map "\C-\M-\\" 'sql-beautify-region-or-buffer)
     )
  )
;;; }
;;; { personal js mode

; for espresso-mode
;; (autoload 'espresso-mode "espresso" "espresso for editing JavaScript." t)
;; (eval-after-load "espresso-mode"
;;   '(progn
;;      (define-key espresso-mode-map (kbd "C-c C-c") 'comment-region)
;;      ))

; for js2-mode
(autoload 'js2-mode "js2-mode" "Major mode for editing JavaScript." t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(eval-after-load "js2-mode" '(require 'setup-js2-mode))

(eval-after-load "js2-mode"
  '(progn
     (require 'setup-js2-mode)
     (define-key js2-mode-map (kbd "C-c C-c") 'comment-region)
     )
  )

(eval-after-load "js-mode"
  '(progn
     (define-key js-mode-map (kbd "C-c C-c") 'comment-region)
     )
  )

; inline js editing
(mmm-add-group
 'personal-html-js
 '((personal-js-script-cdata
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
    :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
   (personal-js-script
    :submode js-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>[ \t]*\n?"
    :back "[ \t]*</script>"
    :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
                 @ "" _ "" @ "\n</script>" @)))))

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil personal-html-js))

;;; }
;;; { personal jsp mode

(add-to-list 'auto-mode-alist '("\\.jsp\\'" . html-mode))

(mmm-add-group 'personal-jsp
 `(
   (personal-jsp-comment
    :submode text-mode
    :face mmm-comment-submode-face
    :front "<%--"
    :back "--%>"
    :insert ((?- jsp-comment-tag nil @ "<%--" @ " " _ " " @ "--%>" @))
    )
   (personal-jsp-code
    :submode js-mode
    :match-face (("<%!" . mmm-declaration-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[!=]?"
    :back "%>"
    :match-name "jsp"
    :insert ((?% jsp-code-tag nil @ "<%" @ " " _ " " @ "%>" @)
             (?! jsp-decl-tag nil @ "<%!" @ " " _ " " @ "%>" @)
             (?= jsp-expr-tag nil @ "<%=" @ " " _ " " @ "%>" @))
    )
   (personal-jsp-directive
    :submode text-mode
    :face mmm-declaration-submode-face ;mmm-special-submode-face
    :front "<%@"
    :back "%>"
    :insert ((?@ jsp-directive-tag nil @ "<%@" @ " " _ " " @ "%>" @))
    )
   ))

(add-to-list 'mmm-mode-ext-classes-alist '(nil "\\.jsp\\'" personal-jsp))

;;; }
;;; { personal css mode

(eval-after-load "css-mode"
  '(progn
     (setq cssm-indent-function 'cssm-c-style-indenter)
     (setq cssm-indent-level '2)
     ))

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-css))

;;; }
;;; { personal remerber settings

(autoload 'remember "remember" nil t)
(autoload 'remember-region "remember" nil t)

;(define-key global-map [f8] 'remember)
;(define-key global-map [f9] 'remember-region)

;;; }
;;; { personal dot settings

(autoload 'graphviz-dot-mode "graphviz-dot-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))

;;; }
;;; { personal android develop settings

(require 'android-mode)

(when (eq 'darwin system-type)
  (set-alist 'android-mode-build-command-alist 'gradle "gradle")
  )

;;; }
;;; { personal org mode

;(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;; }
;;; { personal gradel mode
(autoload 'groovy-mode "groovy-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode))
;; (add-to-list 'compilation-error-regexp-alist '("^\[ERROR\] \(.*\):\[\([0-9]+\),\([0-9]+\)\]" 1 2 3))
;;; }
;;; { personal vb mode

(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" . vbnet-mode)) auto-mode-alist))

;;; }
;;; { personal cmake mode

(autoload 'cmake-mode "cmake-mode" "major-mode for editing CMake sources." t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)

(eval-after-load "cmake-mode"
  '(progn
     (setq cmake-tab-width 4)
     (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
     (add-hook 'cmake-mode-hook (lambda () (folding-mode t)))
     (folding-add-to-marks-list 'cmake-mode "# {{{ " "# }}}" nil)
     ))

;;; }
;;; { personal solidity mode

(autoload 'solidity-mode "solidity-mode" "The solidity major mode." t)
(add-to-list 'auto-mode-alist '("\\.sol\\'" . solidity-mode))

;;; }
;;; { personal swift mode

(autoload 'swift-mode "swift-mode" "Major-mode for Apple's Swift programming language." t)
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

(defun swift-mode:mk-regex-for-def (keyword)
  "Make a regex matching the identifier introduced by KEYWORD."
  (concat "\\<" (regexp-quote keyword) "\\>"
          "\\s *"
          "\\("
          "\\(?:" "\\sw" "\\|" "\\s_" "\\)" "+"
          "\\)"))

(defconst swift-mode:imenu-generic-expression
  (list
   (list "Functions" (swift-mode:mk-regex-for-def "func") 1)
   (list "Classes"   (swift-mode:mk-regex-for-def "class") 1)
   (list "Enums"     (swift-mode:mk-regex-for-def "enum") 1)
   (list "Protocols" (swift-mode:mk-regex-for-def "protocol") 1)
   (list "Structs"   (swift-mode:mk-regex-for-def "struct") 1)
   (list "Extensions"   (swift-mode:mk-regex-for-def "extension") 1)
   (list "Constants" (swift-mode:mk-regex-for-def "let") 1)
   (list "Variables" (swift-mode:mk-regex-for-def "var") 1))
  "Value for `imenu-generic-expression' in `swift-mode'.")

(eval-after-load "swift-mode"
  '(progn
     (setq swift-basic-offset 4)
     (define-key swift-mode-map (kbd "C-c C-c") 'comment-region)
     
     (when (eq 'darwin system-type)
       (require 'company)
       (require 'company-sourcekit)
       (add-to-list 'company-backends 'company-sourcekit)
       (add-hook 'swift-mode-hook
                 (lambda ()
                    (company-mode-on)
                    (setq-local imenu-generic-expression swift-mode:imenu-generic-expression)
                    )
                 )
       )
  ))

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
(unless (server-running-p) (server-start))
;;; }
;;; { emacs local

(if (file-exists-p "~/.emacs.local")
    (load-file "~/.emacs.local"))

;;; }
;;; { desktop

(setq desktop-load-locked-desktop t)
(desktop-save-mode)
(desktop-read)

;;; }
