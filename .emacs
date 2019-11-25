;;; { globl setting

(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-dir)

(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

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

;;; }

;;; { theme

(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))
(load-theme 'classic)

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


(defun my-dpi ()
  (let* ((attrs (car (display-monitor-attributes-list)))
         (size (assoc 'mm-size attrs))
         (sizex (cadr size))
         (res (cdr (assoc 'geometry attrs)))
         (resx (- (caddr res) (car res)))
         dpi)
    (catch 'exit
      ;; in terminal
      (unless sizex
        (throw 'exit 10))
      ;; on big screen
      (when (> sizex 1000)
        (throw 'exit 10))
      ;; DPI
      (* (/ (float resx) sizex) 25.4))))

(defun my-preferred-font-size ()
  (let ( (dpi (my-dpi)) )
  (cond
    ((< dpi 110) 10)
    ((< dpi 130) 14)
    ((< dpi 160) 18)
    ((< dpi 220) 20)
    ((< dpi 260) 28)
    ((< dpi 320) 36)
    (t 36))))

(defcustom global-font-size (my-preferred-font-size) "默認字體大小" :type 'integer)

(qiang-set-font
 '("Consolas" "Monaco" "DejaVu Sans Mono" "Monospace" "Courier New") (concat ":pixelsize=" (number-to-string global-font-size))
 '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

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
  (setq dired-use-ls-dired nil)
  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)
)

;;; }
;;; { cygwin support

(when (eq 'windows-nt system-type)
  (require 'setup-cygwin)
  (set-shell-bash)

  (let ((home-dir (getenv "HOME")))
    (if home-dir
        (progn
          (cd home-dir)
          (setenv "PATH" (concat home-dir "/bin;" (getenv "PATH")))
          (add-to-list 'exec-path (concat home-dir "/bin"))
          )
      ))
  )

;;; }
;;; { coding-system

;(add-to-list `process-coding-system-alist `("bash" . gb2312))
;(add-to-list `process-coding-system-alist `("git*" . gb2312))
;(add-to-list 'file-coding-system-alist '("\\.[hc]\\(pp\\)?\\'" .gb2312 ))

(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)

;;; }
;;; { backup settings

(setq-default make-backup-files nil)
(setq backup-directory-alist '(("" . "~/emacs.d/autosave")))
(auto-save-mode nil)
(setq delete-auto-save-files t)

;;; }
;;; { package

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;;; }
;;; { multi-term

(use-package multi-term
  :init
  (progn
    (setq multi-term-dedicated-close-back-to-open-buffer-p t)

    (defun last-term-buffer (l)
      "Return most recently used term buffer."
      (when l
        (if (eq 'term-mode (with-current-buffer (car l) major-mode))
            (car l) (last-term-buffer (cdr l)))))

    (defun get-term ()
      "Switch to the term buffer last used, or create a new one if
    none exists, or if the current buffer is already a term."
      (interactive)
      (let ((b (last-term-buffer (buffer-list))))
        (if (or (not b) (eq 'term-mode major-mode))
            (multi-term)
          (switch-to-buffer b))))
    )
  :ensure t
  )

;;; }
;;; { eww
;; (setq browse-url-browser-function 'eww-browse-url)
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

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (global-set-key (kbd "C-x C-f") 'ido-find-file)
  (define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)
  )

;;; }
;;; { folding

(use-package folding
  :commands (folding-mode)
  :hook ((sh-mode emacs-lisp-mode cmake-mode cperl-mode makefile-gmake-mode) . folding-mode)
  :config
  (setq folding-fold-on-startup t)
  (folding-mode-add-find-file-hook)

  (folding-add-to-marks-list 'sh-mode "# {{{ " "# }}}" nil)
  (folding-add-to-marks-list 'emacs-lisp-mode ";;; {" ";;; }" "")
  (folding-add-to-marks-list 'cperl-mode "# {{{ " "# }}}" nil)
  (folding-add-to-marks-list 'makefile-gmake-mode "# {{{ " "# }}}" nil)
  (folding-add-to-marks-list 'cmake-mode "# {{{ " "# }}}" nil)
  :ensure t)

;;; }
;;; { yafolding

(use-package yafolding
  :commands yafolding-mode
  :hook ((yaml-mode c-mode c++-mode objc-mode) . yafolding-mode)
  :ensure t)

;; (define-key yafolding-mode-map (kbd "<C-S-return>") nil)
;; (define-key yafolding-mode-map (kbd "<C-M-return>") nil)
;; (define-key yafolding-mode-map (kbd "<C-return>") nil)
;; (define-key yafolding-mode-map (kbd "C-c <C-M-return>") 'yafolding-toggle-all)
;; (define-key yafolding-mode-map (kbd "C-c <C-S-return>") 'yafolding-hide-parent-element)
;; (define-key yafolding-mode-map (kbd "C-c <C-return>") 'yafolding-toggle-element)

;;; }
;;; { personal ggtags mode

;; (use-package ggtags
;;   :commands (ggtags-mode)
;;   :hook (((c-mode c++-mode objc-mode) . ggtags-mode)
;;          )
;;   :custom
;;   (ggtags-enable-navigation-keys nil)
;;   :config
;; ;;            (cond
;; ;;             ((eq system-type 'windows-nt)
;; ;;              (setq gtags-global-command "/usr/local/bin/global.exe"))
;; ;;             (t
;; ;;              (setq gtags-global-command "/usr/local/bin/global")
;;   ;;              ))
;;   :ensure t)

;;; }
;;; { all

(use-package all :ensure t)

;;; }
;;; { git

(use-package magit :ensure t)
(use-package git-timemachine :ensure t)

(use-package magit-todos
  :after magit
  :commands (magit-todos-mode)
  :hook (magit-mode . magit-todos-mode)
  :config (setq magit-todos-recursive t
                magit-todos-depth 100)
  :custom
  (magit-todos-keywords (list "TODO" "FIXME"))
  :ensure t)

;;; }
;;; { projectile

(use-package projectile
  :config
  ;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :ensure t
  )

;;; }
;;; { dash

(when (eq 'darwin system-type)
  (use-package dash-at-point
    :commands (dash-at-point dash-at-point-with-docset)
    :bind (("\C-cd" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset)
           )
    :ensure t
    )
  )

;;; }
;;; { flymake

(require 'flymake-proc)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;;; }
;;; { flycheck-mode

(use-package flycheck
  :ensure t)

;;; }
;;; { company-mode

(use-package company
  :commands
  (company-mode company-complete-common)
  :bind
  (:map company-mode-map
        ("M-/" . company-complete-common)
        :map company-active-map
        ("<tab>" . company-complete-selection)
        ("M-/" . company-select-next-or-abort)
        ("C-n" . company-select-next-or-abort)
        ("C-p" . company-select-previous-or-abort)
        )
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-show-numbers t
        company-tooltip-limit 20
        )
  ;; (company-tng-configure-default)
  ;; (setq company-frontends
  ;;       '(company-tng-frontend
  ;;         company-pseudo-tooltip-frontend
  ;;         company-echo-metadata-frontend))

  (require 'color)
  
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

  :ensure t
  )

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode 1))

(use-package company-tabnine
  :ensure t)

;;; }
;;; { mmm-mode

(use-package mmm-mode
  :config
  (require 'mmm-auto)
  (setq mmm-global-mode 'maybe)
  :ensure t)

;;; }
;;; { personal markdown mode

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "markdown"))

(use-package markdown-mode+
  :ensure t
  :after markdown-mode
  :defer t)

(use-package poly-markdown
  :ensure t
  :after markdown-mode)

(use-package markdown-preview-eww
  :ensure t
  )

;;; }
;;; { personal lsp mode

(use-package lsp-mode
  :commands lsp
  :config
  ;(setq lsp-print-io t)
  (setq lsp-auto-guess-root t
        lsp-enable-xref t
        lsp-enable-snippet nil
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        )
  :ensure t)

;(use-package lsp-ui :ensure t)

(use-package company-lsp
  :requires (lsp-mode company)
  :config
  (setq company-lsp-cache-candidates 'auto
        company-lsp-async t
        company-lsp-enable-recompletion t
        company-lsp-enable-snippet t)
  :ensure t)

;;; }
;;; { personal yaml model

(use-package yaml-mode
  :mode ("\\.yml$" "\\.yaml$")
  :config
  (define-key yaml-mode-map (kbd "C-c C-c") 'comment-region)
  :ensure t
  )

;;; }
;;; { personal dockerfile model

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :ensure t)

(use-package docker-tramp
  :ensure t)

;;; }
;;; { personal nxml model

(eval-after-load "nxml-mode"
  '(progn
     (setq nxml-child-indent 4)
     )
  )

;;; }
;;; { personal lisp mode settings

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'comment-region)

;;; }
;;; { personal c cpp and objc mode settings

(require `find-file)

(mapcar (lambda (x) (add-to-list ff-other-file-alist x))
        '(("\\.mm?$" (".h" "_i.h"))
          ("\\.cc$"  (".hh" ".h"))
          ("\\.hh$"  (".cc" ".C"))
          ("\\.c$"   (".h" "_i.h"))
          ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))
          ("_i\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))
          ("\\.C$"   (".H"  ".hh" ".h"))
          ("\\.H$"   (".C"  ".CC"))
          ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
          ("\\.HH$"  (".CC"))
          ("\\.cxx$" (".hh" ".h"))
          ("\\.cpp$" (".hpp" ".hh" ".h"))
          ("\\.hpp$" (".cpp" ".c"))))

(use-package cc-mode
  :commands (c-mode c++-mode objc-mode)
  :mode ("\\.inl\\'" . c++-mode)
  :magic (("\\(.\\|\n\\)*\n@implementation" . objc-mode)
          ("\\(.\\|\n\\)*\n@interface" . objc-mode)
          ("\\(.\\|\n\\)*\n@protocol" . objc-mode)
          )
  :hook ((c-mode c++-mode objc-mode) .
         (lambda()
           (if (not (eq 'windows-nt system-type)) (lsp))
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
           (add-to-list (make-local-variable 'company-backends) #'company-tabnine)
           )
         )
  :config
  (when (equal system-type 'darwin)
    (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"))
  )

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda()
           (require `ccls)
           (set (make-local-variable 'lsp-enable-on-type-formatting) nil)
           ))
  :config
  (eval-after-load "projectile"
    '(add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))
  :ensure t)

;;; }
;;; { personal java mode settings

(defvar java-bin nil "java executabale")
(cond
 ((eq system-type 'windows-nt)
  (setq java-bin (expand-file-name "bin/java.exe" (getenv "JAVA_HOME"))))
 (t
  (setq java-bin "java")
  ))

(defvar mvn-bin nil "mvn executabale")
(cond
 ((eq system-type 'windows-nt)
  (setq mvn-bin (expand-file-name "bin/mvn.cmd" (getenv "MVN_HOME"))))
 (t
  (setq mvn-bin "mvn")))

;(use-package treemacs :ensure t)
(if (not (eq 'windows-nt system-type))
    (use-package lsp-java
                                        ;:requires (lsp-ui-flycheck lsp-ui-sideline)
      :init
      (add-hook 'java-mode-hook 'lsp)
                                        ;(java-mode . (lambda () (lsp-ui-flycheck-enable t)))
                                        ;(java-mode . lsp-ui-sideline-mode)
      :config
      (setq lsp-java-save-action-organize-imports nil)
      :ensure t
      )
  )

;; (use-package meghanada
;;   :commands meghanada-mode
;;   :init
;;   (add-hook 'java-mode-hook
;;             (lambda ()
;;               (meghanada-mode t)
;;               (setq c-basic-offset 4)
;;               ))
;;   :config
;;   (setq meghanada-java-path java-bin)
;;   (setq meghanada-maven-path mvn-bin)
;;   :ensure t
;;   )

;;; }
;;; { personal kotlin mode settings

(eval-after-load "compile"
  '(progn
     (add-to-list 'compilation-error-regexp-alist 'gradle-kotlin-1)
     (add-to-list
      'compilation-error-regexp-alist-alist
      '(gradle-kotlin-1
        "^[we]: \\(.+\\): (\\([0-9]+\\), \\([0-9]+\\)): .*" 1 2 3))
     (add-to-list 'compilation-error-regexp-alist 'gradle-kotlin-2)
     (add-to-list
      'compilation-error-regexp-alist-alist
      '(gradle-kotlin-2
        "^[we]: \\(.+\\):\\([0-9]+\\): .*" 1 2))
     ))

(use-package kotlin-mode
  :mode "\\.kt\\'"
  :hook ((kotlin-mode . lsp)
         )
  :config
  (define-key kotlin-mode-map (kbd "C-c C-c") 'comment-region)
  :ensure t)

;;; }
;;; { personal dart mode settings

(use-package dart-mode
  :mode "\\.dart\\'"
  :hook ((dart-mode . lsp)
         )
  :init
  ;; (with-eval-after-load "projectile"
  ;;   (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  ;;   (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
  :config
  (define-key dart-mode-map (kbd "C-c C-c") 'comment-region)
  :ensure t)

;;; }
;;; { personal go model

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . lsp))
  :bind
  (:map go-mode-map
   ("C-c C-g" . go-goto-map)
   ("C-c C-r" . go-remove-unused-imports)
   ("C-c C-f" . gofmt)
   ("C-c C-k" . godoc)
   ("M-." . godef-jump)
   )
  :config
  (defun personal-go-setup()
    (add-hook 'before-save-hook #'gofmt-before-save)
    (setq tab-width 4)
    )
  (add-hook 'go-mode-hook 'personal-go-setup)
  (define-key go-mode-map (kbd "C-c C-c") 'comment-region)
  :ensure t
  )

;;; }
;;; { personal make setup

(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))

;;; }
;;; { personal csharp mode setup

;; (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;; (setq auto-mode-alist (cons '("\\.cs$" . csharp-mode) auto-mode-alist))

;; ;; Patterns for finding Microsoft C# compiler error messages:
;; (require 'compile)
;; (push '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): error" 1 2 3 2) compilation-error-regexp-alist)
;; (push '("^\\(.*\\)(\\([0-9]+\\),\\([0-9]+\\)): warning" 1 2 3 1) compilation-error-regexp-alist)

;; ;; Patterns for defining blocks to hide/show:
;; (push '(csharp-mode
;; 	"\\(^\\s *#\\s *region\\b\\)\\|{"
;; 	"\\(^\\s *#\\s *endregion\\b\\)\\|}"
;; 	"/[*/]"
;; 	nil
;; 	hs-c-like-adjust-block-beginning)
;;       hs-special-modes-alist)

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

;;; }
;;; { personal python mode settings

(use-package python-mode
  :mode "\\.py\\'"
  :hook ((python-mode . lsp))
  :config
  :ensure t)

;; (use-package lsp-python
;;   :hook (python-mode . (lambda ()
;;                                         ;(require 'lsp-python-ms)
;;                          (lsp))
;;                      )  ; or lsp-deferred
;;   :ensure t)

;;; }
;;; { personal php mode

(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-to-list 'mmm-mode-ext-classes-alist '(html-mode "\\.php[s345t]?\\'" html-php))
  (add-to-list 'auto-mode-alist '("\\.php[s345t]?\\'" . html-mode))
  :ensure t)

;;; }
;;; { personal json mode

(use-package json-mode
  :ensure t)

;;; }
;;; { personal js mode

(use-package js2-mode
  :mode "\\.js\\'"
  :hook ((js2-mode . (lambda () (flycheck-mode 1)))
         (js2-mode . lsp)
         )
  :bind (:map js2-mode-map
              ("C-c C-c" . comment-region)
              )
  :config
  (setq-default js2-allow-rhino-new-expr-initializer nil)
  (setq-default js2-auto-indent-p nil)
  (setq-default js2-enter-indents-newline nil)
  (setq-default js2-global-externs
                '("module" "require" "buster" "sinon" "assert" "refute"
                  "setTimeout" "clearTimeout" "setInterval" "clearInterval"
                  "location" "__dirname" "console" "JSON" "process"))
  (setq-default js2-idle-timer-delay 0.1)
  (setq-default js2-indent-on-enter-key nil)
  (setq-default js2-mirror-mode nil)
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default js2-auto-indent-p t)
  (setq-default js2-include-rhino-externs nil)
  (setq-default js2-include-gears-externs nil)
  (setq-default js2-concat-multiline-strings 'eol)
  (setq-default js2-rebind-eol-bol-keys nil)
  (setq-default js2-basic-offset 2)

  ;; Let flycheck handle parse errors
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

  :ensure t)

(use-package js2-refactor
  :commands (js2-refactor-mode js2r-rename-current-buffer-file js2r-delete-current-buffer-file)
  :hook (js2-mode-hook . js2-refactor-mode)
  :bind
  (:map js2-mode-map
        ("C-x C-r" . js2r-rename-current-buffer-file)
        ("C-x C-k" . js2r-delete-current-buffer-file)
        )
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  :ensure t)

(eval-after-load "js-mode"
  '(progn
     (define-key js-mode-map (kbd "C-c C-c") 'comment-region)
     )
  )

(eval-after-load "mmm-mode"
  '(progn
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
    )
)

;;; }
;;; { personal typescript mode

;; (use-package typescript-mode
;;   :ensure t
;;   :hook ((typescript-mode . lsp)
;;          )
;;   )

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;;; }
;;; { personal web mode (html, jsp)

(use-package web-mode
  :mode ("\\.jsp\\'" "\\.html?\\'")
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    )
  :ensure t
  )

(use-package emmet-mode
  :hook (web-mode)
  :ensure t)

;;; }
;;; { personal css mode

(eval-after-load "css-mode"
  '(progn
     (setq cssm-indent-function 'cssm-c-style-indenter)
     (setq cssm-indent-level '2)
     ))

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-css))

;;; }
;;; { personal android develop settings

(use-package android-mode
  :commands (android-mode android-logcat android-gradle)
  :config
  (when (eq 'darwin system-type)
    (set-alist 'android-mode-build-command-alist 'gradle "gradle")
    )
  :ensure t)

;;; }
;;; { personal groovy(gradel) mode

(use-package groovy-mode
  :ensure t
  :mode ("\\.groovy$" "\\.gradle$")
  :interpreter ("gradle" "groovy")
  :config
  (autoload 'run-groovy "inf-groovy" "Run an inferior Groovy process")
  (autoload 'inf-groovy-keys "inf-groovy" "Set local key defs for inf-groovy in groovy-mode")

  ;; Some keys for
  (add-hook 'groovy-mode-hook '(lambda () (inf-groovy-keys))))
  ;(define-key groovy-mode-map (kbd "C-c C-c") 'comment-region)

;; Subpackages
(use-package groovy-imports :ensure t)

(use-package flycheck-gradle
  :ensure t
  :defer t)

;;; }
;;; { personal cmake mode

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  (setq cmake-tab-width 2)
  :ensure t
  )

(use-package cmake-font-lock
  :requires cmake-mode
  :commands (cmake-font-lock-activate)
  :hook (cmake-mode-hook . cmake-font-lock-activate)
  :ensure t
  )

;;; }
;;; { personal solidity mode

(use-package solidity-mode
  :mode "\\.sol\\'"
  :ensure t)

;;; }
;;; { personal swift mode

(use-package company-sourcekit
  :after company
  :ensure t)

(use-package swift-mode
  :after (lsp company-lsp)
  :mode "\\.swift\\'"
  :hook (;(swift-mode . lsp)
         (swift-mode . (lambda () (add-to-list (make-local-variable 'company-backends) 'company-sourcekit)))
         (swift-mode . company-mode)
         )
  :custom
  (swift-mode:multiline-statement-offset 4)
  (swift-mode:parenthesized-expression-offset 4)
  :config
  (eval-after-load "compile"
    '(progn
       (add-to-list 'compilation-error-regexp-alist 'swift)
       (add-to-list
        'compilation-error-regexp-alist-alist
        '(swift
          "⚠?[ ❌]+\\([^ ❌].+\\..+\\):\\([0-9]+\\):\\([0-9]+\\): .*" 1 2 3))))
  (define-key swift-mode-map (kbd "C-c C-c") 'comment-region)
  :ensure t
  )

;; (use-package lsp-sourcekit
;;   :after lsp-mode
;;   :config
;;   (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain")
;;   (setq lsp-sourcekit-executable "sourcekit-lsp")
;;   :ensure t)

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
;;; { auto-upate

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  :ensure t)

;;; }
