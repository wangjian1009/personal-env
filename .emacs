;; -*- origami-fold-style: triple-braces -*-

;; globl setting {{{ 
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 100000)

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

(setq gc-cons-threshold 100000000)
(when (>= emacs-major-version 27)
  (setq read-process-output-max (* 1024 1024)))

(global-set-key (kbd "ESC C-l") 'revert-buffer)
(global-set-key (kbd "ESC C-M-l") 'revert-buffer-with-coding-system)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c f") 'ff-find-other-file)
(setq vc-follow-symlinks t)

;; }}}
;; package {{{

(require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ))

(when (< emacs-major-version 27)
  (package-initialize))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; }}}
;; theme {{{

(setq custom-theme-directory (expand-file-name "themes" user-emacs-directory))

;; (use-package apropospriate-theme
;;   :ensure t
;;   :config 
;;   (load-theme 'apropospriate-dark t)
;;   ;; or (load-theme 'apropospriate-light t)
;;   )

(use-package zenburn-theme
  :ensure t
  :config
  (setq zenburn-use-variable-pitch t)
  (setq zenburn-scale-org-headlines t)
  (setq zenburn-scale-outline-headlines t)
  (load-theme 'zenburn t)
  )

;; (use-package dracula-theme
;;   :ensure t
;;   :config
;;   (load-theme 'dracula t)
;;   )

;; (load-theme 'classic)

;; }}}
;; font setting... {{{

;; ** OS X **
;; Monaco               -- OS X 之前的默认字体就是它，它的风格特殊，有种苹果味
;; Menlo                -- Xcode默认字体 (老版本)
;; San Francisco Mono   -- Xcode默认字体 (macOS High Sierra以后版本)

;; ** Windows **
;; Consolas             --  Microsoft Visual Studio 的默认字

;; ** 其他 **
;; Monospace
;; Courier New
;; DejaVu Sans Mono
;; Anonymous Pro        --  古典打字机的感觉
;; Space Mono           --  
;; IBM 3270             --  基于 IBM 在 1971 年制造的 IBM 3270 终端中使用的字体，有种复古编程的味道
;; Droid Sans Mono      --  这是为 Android 设计的一种字体，很漂亮，但是0和O并没有区分

;; Microsoft Yahei

;; (qiang-set-font
;;  '("Consolas" "Menlo")
;;  (concat ":pixelsize=" (number-to-string global-font-size))
;;  '("Microsoft Yahei"))

(use-package cnfonts
  :ensure t
  :init
  (setq cnfonts-profiles
        '("program" "org-mode" "read-book"))
  
  (setq cnfonts--custom-set-fontnames
        '(("program" "Consolas" "Microsoft Yahei")
          ))

  (setq cnfonts--custom-set-fontsizes
        '((9    9.0  9.5 )
          (10   11.0 11.0)
          (11.5 12.5 12.5)
          (12.5 13.5 13.5)
          (14   15.0 15.0)
          (16   17.0 17.0)
          (18   18.0 18.0)
          (20   21.0 21.0)
          (22   23.0 23.0)
          (24   25.5 25.5)
          (26   27.0 27.0)
          (28   29.0 29.0)
          (30   32.0 32.0)
          (32   33.0 33.0)))

  (cnfonts-enable)
  )

;; (defun my-dpi ()
;;   (let* ((attrs (car (display-monitor-attributes-list)))
;;          (size (assoc 'mm-size attrs))
;;          (sizex (cadr size))
;;          (res (cdr (assoc 'geometry attrs)))
;;          (resx (- (caddr res) (car res)))
;;          dpi)
;;     (catch 'exit
;;       ;; in terminal
;;       (unless sizex
;;         (throw 'exit 10))
;;       ;; on big screen
;;       (when (> sizex 1000)
;;         (throw 'exit 10))
;;       ;; DPI
;;       (* (/ (float resx) sizex) 25.4))))

;; (defun my-preferred-font-size ()
;;   (let ( (dpi (my-dpi)) )
;;   (cond
;;     ((< dpi 110) 14)
;;     ((< dpi 130) 16)
;;     ((< dpi 160) 18)
;;     ((< dpi 220) 20)
;;     ((< dpi 260) 28)
;;     ((< dpi 320) 36)
;;     (t 36))))

;; }}}
;; support functions or macros {{{

(defun validate-dir-p (directory)
  "Return DIRECTORY if DIRECTORY is a readable directory, nil otherwise."
  (and (stringp directory)  (file-directory-p directory)  (file-readable-p directory)  directory))

;; shorthand for interactive lambdas
(defmacro λ (&rest body)
  `(lambda ()
     (interactive)
     ,@body))

;; }}}
;; apple support {{{

(when (eq 'darwin system-type)
  (setq mac-command-modifier 'meta) ;映射苹果键  
  (setq mac-control-modifier 'control) ;映射Ctrl键
  (setq mac-option-modifier 'control) ;映射Alt键 

  (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH" (concat "/usr/local/lib:" (getenv "LD_LIBRARY_PATH")))
  (setenv "INFOPATH" (concat "/usr/local/info:/usr/local/share/info:/usr/info:/usr/share/info:" (getenv "INFOPATH")))
  (setenv "MANPATH" (concat "/usr/local/man:/usr/local/share/man:/usr/man:/usr/share/man:" (getenv "MANPATH")))
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path "/usr/local/sbin")

  (let ((home-dir (getenv "HOME")))
    (if home-dir
        (progn
          (setenv "PATH" (concat home-dir "/.local/bin:" (getenv "PATH")))
          (setenv "LD_LIBRARY_PATH" (concat home-dir "/.local/lib:" (getenv "LD_LIBRARY_PATH")))
          (add-to-list 'exec-path (concat home-dir "/.local/bin"))
          )
      ))

  ;; (setq delete-by-moving-to-trash t
  ;;       trash-directory "~/.Trash/emacs")
  (setq dired-use-ls-dired nil)
  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)
)

;; }}}
;; msys support {{{

(when (eq 'windows-nt system-type)
  (use-package w32-browser
    :ensure t)
    
  (let ((msys-root (or (validate-dir-p "C:/tools/msys64/")))
        )
    (if msys-root
        (progn
          (add-to-list 'Info-default-directory-list (expand-file-name "usr/share/info" msys-root) 'APPEND)
          ;(setenv "INFOPATH" (concat (expand-file-name "usr/share/info" msys-root) ":" (getenv "INFOPATH")))
          (add-to-list 'exec-path (expand-file-name "usr/bin" msys-root))
          (setenv "PATH" (concat (expand-file-name "usr/bin" msys-root) ";" (getenv "PATH")))

          (add-to-list 'Info-default-directory-list (expand-file-name "mingw64/share/info" msys-root) 'APPEND)
          ;(setenv "INFOPATH" (concat (expand-file-name "mingw64/share/info" msys-root) ":" (getenv "INFOPATH")))
          (add-to-list 'exec-path (expand-file-name "mingw64/bin" msys-root))
          (setenv "PATH" (concat (expand-file-name "mingw64/bin" msys-root) ";" (getenv "PATH")))
          
          (setq shell-file-name  (expand-file-name "usr/bin/bash.exe" msys-root))
          (setenv "SHELL" shell-file-name)
          (setq explicit-shell-file-name  shell-file-name) ; Interactive shell
          (setq ediff-shell               shell-file-name)    ; Ediff shell
          (setq explicit-shell-args       '("--login" "-i"))
          ;;;;; (setq shell-command-switch "-ic") ; SHOULD THIS BE "-c" or "-ic"?
          (setq w32-quote-process-args ?\")

          )
      )
    )

  (let ((home-dir (getenv "HOME")))
    (if home-dir
        (progn
          (cd home-dir)
          (setenv "PATH" (concat home-dir "/bin;" (getenv "PATH")))
          (add-to-list 'exec-path (concat home-dir "/bin"))
          )
      ))
  )

;; }}}
;; coding-system {{{

;(add-to-list `process-coding-system-alist `("bash" . gb2312))
;(add-to-list `process-coding-system-alist `("git*" . gb2312))
;(add-to-list 'file-coding-system-alist '("\\.[hc]\\(pp\\)?\\'" .gb2312 ))
(setq system-time-locale "C")

;; }}}
;; backup settings {{{

(setq-default make-backup-files nil)
(setq backup-directory-alist '(("" . "~/emacs.d/autosave")))
(auto-save-mode nil)
(setq delete-auto-save-files t)

;; }}}
;; iedit {{{
(use-package iedit
  :ensure t)
;; }}}
;; hydra {{{

(use-package hydra :ensure t)

;; }}}
;; format-all {{{

(use-package format-all
  :ensure t)

;; }}}
;; multi-term {{{

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

;; }}}
;; dirent {{{

;; Show git info in dired
(use-package dired-git-info
  :commands (dired-git-info-mode)
  :after dired
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Allow rsync from dired buffers
(use-package dired-rsync
  :commands (dired-rsync)
  :after dired
  :ensure t
  :bind (:map dired-mode-map
              ("C-c C-r" . dired-rsync)))

(use-package dired-x
  :config
  (progn
    (setq dired-omit-verbose nil)
    ;; toggle `dired-omit-mode' with C-x M-o
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$"))))
;; Colourful dired
;; (use-package diredfl
;;   :ensure t
;;   :init (diredfl-global-mode 1))

;; }}}
;; compilation-mode {{{

(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook ((comint-mode . ansi-color-for-comint-mode-on)
         (compilation-filter . my-colorize-compilation-buffer))
  )

;; }}}
;; browse-url {{{

;; (if (featurep 'xwidget-internal)
;;     (setq browse-url-browser-function 'xwidget-webkit-browse-url))

;(setq browse-url-browser-function 'eww-browse-url)

;; }}}
;; xwidget {{{

(use-package xwidgete
  :if (featurep 'xwidget-internal)
  :ensure t)

;; }}}
;; ibuffer settings {{{

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; (global-set-key (kbd "C-x C-f") 'ido-find-file)
  ;; (define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file)
  )

;; (use-package ibuffer-projectile
;;   :ensure t
;;   :hook (ibuffer . (lambda ()
;;                      (ibuffer-projectile-set-filter-groups)
;;                      ;; (unless (eq ibuffer-sorting-mode 'alphabetic) (ibuffer-do-sort-by-alphabetic))
;;                      ))
;;   )

;; }}}
;; diminish {{{
(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode)
  )
;; }}} 
;; ace-window {{{

(use-package ace-window
  :ensure t
  ;;   :config
  ;;   (setq aw-scope 'global) ;; was frame
  ;;   (global-set-key (kbd "C-x O") 'other-frame)
  ;;   (global-set-key [remap other-window] 'ace-window)
  )

;; }}}
;; ivy {{{

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :bind
  (("C-c C-r" . ivy-resume)
   (:map ivy-minibuffer-map
         ("RET" . ivy-alt-done)
         )
   )
  :init
  (setq
   ivy-use-virtual-buffers t
   ivy-count-format "%d/%d "
   ivy-wrap t
   ivy-height 10
   )
  :config
  (add-to-list 'ivy-re-builders-alist
           '(read-file-name-internal . ivy--regex-fuzzy))

  )

;; }}} 
;; swiper {{{

(use-package swiper
  :ensure t
  :bind
  (
   ("\C-s" . swiper-isearch)
   )
  )

;; }}} 
;; counsel {{{

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :hook (after-init . counsel-mode)
  :bind (("M-y" . counsel-yank-pop)
         )
  )

;; }}}
;; folding {{{

(use-package origami
  :ensure t
  :hook (((sh-mode emacs-lisp-mode) . origami-mode)
         )
  :config
  (setq origami-mode-prefix-map (make-sparse-keymap))
  (define-key origami-mode-prefix-map (kbd "y") 'origami-open-node) ;Open a fold node.
  (define-key origami-mode-prefix-map (kbd "o") 'origami-show-node) ;Like origami-open-node but also opens parent fold nodes recursively so as to ensure the position where point is is visible.
  (define-key origami-mode-prefix-map (kbd "z") 'origami-close-node-recursively) ;Close a fold node and all of its children.

  (define-key origami-mode-prefix-map (kbd "#") 'origami-toggle-node) ;Toggle open or closed a fold node.
  (define-key origami-mode-prefix-map (kbd ".") 'origami-recursively-toggle-node) ;Acts like org-mode header collapsing. Cycle a fold between open, recursively open, closed.
  (define-key origami-mode-prefix-map (kbd "M-.") 'origami-toggle-all-nodes) ;Move to the start of the next fold.

  ;; (define-key origami-mode-prefix-map (kbd "\C-o") 'origami-open-all-nodes) ;Open every fold in the buffer.

  (define-key origami-mode-prefix-map (kbd "w") 'origami-reset) ;Remove all folds from the buffer and reset all origami state. Useful if origami messes up!

  (define-key origami-mode-prefix-map (kbd "/") 'origami-undo) ;Undo the last folding operation.

  (define-key origami-mode-prefix-map (kbd "n") 'origami-forward-fold-same-level) ;Move to the previous fold.
  (define-key origami-mode-prefix-map (kbd "p") 'origami-backward-fold-same-level) ;Move to the start of the next fold.
  (define-key origami-mode-prefix-map (kbd "M-n") 'origami-previous-fold) ;Move to the previous fold.
  (define-key origami-mode-prefix-map (kbd "M-p") 'origami-previous-fold) ;Move to the start of the next fold.

  (define-key origami-mode-map (kbd "C-c @") origami-mode-prefix-map)
  
;; origami-close-node	Close a fold node.
;; origami-forward-toggle-node	Search forward on this line for a node and toggle it open or closed. This makes toggling nodes much more convenient.
;; origami-close-all-nodes	Close every fold in the buffer.
;; origami-show-only-node	Close everything but the folds necessary to see the point. Very useful for concentrating on an area of code.
;; origami-next-fold	Move to the end of the next fold.
;; origami-forward-fold	Move to the start of the next fold.
;; origami-redo	Redo the last undone folding operation.
  
)
  
;; }}}
;; all {{{

(use-package all :ensure t)

;; }}}
;; git {{{

(use-package magit
  :ensure t
  :bind(("C-x g" . magit-status))
  :config
  (setq magit-ediff-dwim-show-on-hunks t)
  )

(use-package git-timemachine
  :ensure t
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook (before-revert . (lambda ()
                           (when (bound-and-true-p git-timemachine-mode)
                             (user-error "Cannot revert the timemachine buffer")))))

(use-package browse-at-remote
  :ensure t
  :bind (:map vc-prefix-map
              ("B" . browse-at-remote))
  :config
  (add-to-list 'browse-at-remote-remote-type-domains '("gitlab.adups.com" . "gitlab"))
  )

;; (use-package magit-todos
;;   :after magit
;;   :ensure t
;;   :commands (magit-todos-mode)
;;   :hook (magit-mode . magit-todos-mode)
;;   :config (setq magit-todos-recursive t
;;                 magit-todos-depth 100)
;;   :custom
;;   (magit-todos-keywords (list "TODO" "FIXME"))
;;   :ensure t)

(use-package smerge-mode
  :after hydra
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package forge
  :ensure t
  :after magit)

;; }}}
;; projectile {{{

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  ;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  )

;; }}}
;; flymake {{{

(use-package flymake-proc
  :ensure t
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flycheck-goto-previous-error)
              ("C-c ! l" . flymake-show-diagnostics-buffer)
              )
  :config
  ;(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  )

;; }}}
;; company-mode {{{

(use-package company
  :commands
  (company-mode company-complete-common)
  :diminish company-mode
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
  :ensure t
  :init
  (defun company-tabnine-buffer-enable()
    "在当前buffer开启tabnine"
    (add-to-list (make-local-variable 'company-backends) #'company-tabnine))
  )

;; }}}
;; mmm-mode {{{

(use-package mmm-mode
  :config
  (require 'mmm-auto)
  (setq mmm-global-mode 'maybe)
  :ensure t)

;; }}}
;; osx {{{

(when (eq 'darwin system-type)
  (use-package osx-trash
    :ensure t
    :config
    (osx-trash-setup)
    (setq delete-by-moving-to-trash t)
    )

  (use-package osx-dictionary
    :if (eq 'darwin system-type)
    :commands (osx-dictionary-search-word-at-point)
    :ensure t
    :bind (("\C-c\C-d" . osx-dictionary-search-word-at-point))
    )

  (use-package dash-at-point
    :if (eq 'darwin system-type)
    :ensure t
    :commands (dash-at-point dash-at-point-with-docset)
    :bind (("\C-cd" . dash-at-point)
           ("\C-ce" . dash-at-point-with-docset)
           )
    )
  )

;; }}} 
;; Icons {{{

(use-package all-the-icons
  :ensure t
  :config
  ;(all-the-icons-install-fonts)
  )

(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup)
  )

;; (use-package all-the-icons-ivy-rich
;;   :ensure t
;;   :init (all-the-icons-ivy-rich-mode 1))

(use-package all-the-icons-dired
  :ensure t
  :after (dired)
  :diminish all-the-icons-dired-mode
  :hook ((dired-mode . all-the-icons-dired-mode))
  )

(use-package all-the-icons-ibuffer
  :ensure t
  :diminish all-the-icons-ibuffer-mode
  :init (all-the-icons-ibuffer-mode 1))

;; }}}
;; x509 {{{

(use-package x509-mode
  :ensure t)

;; }}}
;; pyim {{{

(use-package pyim
  :ensure t
  :config
  (setq default-input-method "pyim")

  ;; 金手指设置，可以将光标处的编码，比如：拼音字符串，转换为中文。
  ;; (global-set-key (kbd "M-j") 'pyim-convert-string-at-point)

  ;; 我使用全拼
  (pyim-default-scheme 'quanpin)
  ;; (pyim-default-scheme 'wubi)
  ;; (pyim-default-scheme 'cangjie)

  (pyim-isearch-mode -1)

  ;; 设置选词框的绘制方式
  (if (posframe-workable-p)
      (setq pyim-page-tooltip 'posframe)
    (setq pyim-page-tooltip 'popup))

  ;; 显示5个候选词。
  (setq pyim-page-length 5)
  )

(use-package pyim-basedict
  :ensure t
  :init
  (pyim-basedict-enable))

;; }}}
;; personal Eshell mode {{{

(use-package eshell
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "e" "find-file $1")
              (eshell/alias "emacs" "find-file $1")

              (eshell/alias "gd" "magit-diff-unstaged")
              (eshell/alias "gds" "magit-diff-staged")

              ;; The 'ls' executable requires the Gnu version on the Mac
              (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                            "/usr/local/bin/gls"
                          "/bin/ls")))
                (eshell/alias "ll" (concat ls " -AlohG --color=always"))))
            )
  )

;; }}}
;; personal ORG mode {{{

(use-package org
  :hook ((org-mode . yas-minor-mode-on)
         (org-mode . turn-on-org-cdlatex)
         )
  :config
  (setq org-preview-latex-default-process 'imagemagick) ;使用 imagemagick 来生成图片
  (setq org-confirm-babel-evaluate nil)   ;不用每次确认
  (setq org-reverse-note-order t)
  (setq org-edit-src-content-indentation 0)
  (setq org-refile-targets
        '((org-agenda-files :tag . "REFINE")
          ))

  (global-set-key (kbd "C-c o a") 'org-agenda)
  (global-set-key (kbd "C-c o t") 'org-capture)
          
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("INPROGRESS" . "yellow")
          ("WAITTING" . (:foreground "gray" :weight bold))
          ("ABORT" . (:foreground "gray" :weight bold))
          ("CANCELED" . (:foreground "gray" :weight bold))
          ("DONE" . (:foreground "gray" :weight bold))
          ))
  
  (add-to-list 'org-tags-exclude-from-inheritance "PROJECT")
  (add-to-list 'org-tags-exclude-from-inheritance "REQUIREMENT")
  (add-to-list 'org-tags-exclude-from-inheritance "BUG")
  (add-to-list 'org-tags-exclude-from-inheritance "VERSION")
  (add-to-list 'org-tags-exclude-from-inheritance "REFINE")

  (add-to-list 'org-src-lang-modes '("perl" . "cperl"))
               
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ledger . t)
     (hledger . t)
     (R . t)
     (sql . t)
     (lisp . t)
     (ditaa . t)
     (shell . t)
     (perl . t)
     (js . t)
     (dot . t)
     (python . t)
     (latex . t)
     ))
  )

(use-package ob-async
  :ensure t)

(use-package ob-http
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages
           '(http . t)
     ))
)

(use-package ob-typescript
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages
           '(typescript . t)
     ))
  )

(use-package ob-mermaid
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages
           '(mermaid . t)
     ))
  )

(use-package org-superstar
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode t))))

(use-package org-mime
  :after org
  :ensure t
  :bind ((:map org-mode-map
               ("C-c o m" . org-mime-org-buffer-htmlize)
               )
         )
  :config
  (setq org-mime-library 'semi)
  )

(use-package ob-sql-mode
  :ensure t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages '(sql-mode . t)))
  )

(use-package ox-latex
  :load-path "lisp/org-mode/lisp"
  :ensure nil
  :demand
  :after org
  :custom
  (org-latex-compiler "xelatex")
  :config
  (setq org-highlight-latex-and-related '(latex script entities))
  (add-to-list 'org-latex-classes
               '("exam"
                 "\\documentclass{exam}
                 [NO-DEFAULT-PACKAGES]\\usepackage{hyperref}
                 [PACKAGES]
                 [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}\\begin{questions}" "\\end{questions}")
                 ("\\question %%%s" . "\\question %%%s")
                 )
               )
  )

(use-package ox-gfm :ensure t)

;; (use-package org-bookmark-heading
;;   :ensure t
;;   )

;; (use-package org-babel
;;   :load-path "lisp/org-mode/lisp"
;;   :ensure t
;;   :after org
;;   :config
;;   (add-to-list org-babel-load-languages '(ledger . t))
;;   )

;; }}}
;; personal EBDB {{{

;; (use-package bbdb
;;   :ensure t
;;   :config
;;   (setq bbdb-mua-pop-up-window-size 0.2)
;;   ; (setq bbdb-use-pop-up t)
;;   ; (setq bbdb/mail-auto-create-p t)
;;   ;; exceptional folders against auto collection
;;   ; (setq signature-use-bbdb t)
;;   ; (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
;;   ; (setq bbdb-north-american-phone-numbers-p nil)
;;   )

;; (use-package bbdb-vcard
;;   :ensure t)

(use-package ebdb
  :ensure t
  :config
  (setq ebdb-default-window-size 0.2)
  (setq ebdb-mua-auto-update-p t)

  ;;(eval-after-load "org" '(require 'ebdb-org))
  )

(use-package ebdb-i18n-chn
  :after ebdb
  :ensure t)

;; }}}
;; personal mail {{{

(use-package wanderlust
  :ensure t
  :commands (wl-user-agent
             wl-user-agent-compose
             wl-draft-send
             wl-draft-kill
             )
  :init
  (if (boundp 'mail-user-agent)
      (setq mail-user-agent 'wl-user-agent))
  (if (fboundp 'define-mail-user-agent)
      (define-mail-user-agent
        'wl-user-agent
        'wl-user-agent-compose
        'wl-draft-send
        'wl-draft-kill
        'mail-send-hook))
  )

;; }}}
;; personal elfeed {{{

(use-package elfeed
  :ensure t
  :bind(("C-x w" . elfeed)
        (:map elfeed-show-mode-map
              ("v" . elfeed-show-visit-eww))
        )
  :config

  ;; (define-key elfeed-show-mode-map (kbd "v")
  ;;   '(lambda ()
  (defun elfeed-show-visit-eww()
    "通过EWW打开当前内容"
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (message "Sent to browser: %s" link)
        (eww-browse-url link))))
  
  ;;functions to support syncing .elfeed between machines
  ;;makes sure elfeed reads index from disk before launching
  (defun personal/elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening"
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;;write to disk when quiting
  (defun personal/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))
  )

;; use an org file to organise feeds
(use-package elfeed-org
  :ensure t
  :after elfeed
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files '("~/.emacs.d/elfeed.org"))
  )

;; }}}
;; personal ledger mode {{{

;; (use-package ledger-mode
;;   :ensure t
;;   :hook ((ledger-mode .
;;           (lambda ()
;;             (setq-local tab-always-indent 'complete)
;;             (setq-local completion-cycle-threshold t)
;;             (setq-local ledger-complete-in-steps t)))
;;          )
;;   :config
;;   (setq ledger-reconcile-default-commodity "¥"
;;         )
;;   )

(use-package htmlize
  :ensure t)

(use-package hledger-mode
  :after htmlize
  :ensure t
  :mode ("\\.journal\\'" "\\.hledger\\'" "\\.ledger\\'")
  :commands hledger-enable-reporting
  :preface
  (defun hledger/next-entry ()
    "Move to next entry and pulse."
    (interactive)
    (hledger-next-or-new-entry)
    (hledger-pulse-momentary-current-entry))

  (defface hledger-warning-face
    '((((background dark))
       :background "Red" :foreground "White")
      (((background light))
       :background "Red" :foreground "White")
      (t :inverse-video t))
    "Face for warning"
    :group 'hledger)

  (defun hledger/prev-entry ()
    "Move to last entry and pulse."
    (interactive)
    (hledger-backward-entry)
    (hledger-pulse-momentary-current-entry))

  :bind (("C-c j" . hledger-run-command)
         :map hledger-mode-map
         ("C-c e" . hledger-jentry)
         ("M-p" . hledger/prev-entry)
         ("M-n" . hledger/next-entry))
  :init
  (setq hledger-jfile
        (expand-file-name "~/miscellany/personal/finance/accounting.journal")
        ;hledger-email-secrets-file (expand-file-name "secrets.el" emacs-assets-directory)
        )
  ;; Expanded account balances in the overall monthly report are
  ;; mostly noise for me and do not convey any meaningful information.
  (setq hledger-show-expanded-report nil)

  (when (boundp 'my-hledger-service-fetch-url)
    (setq hledger-service-fetch-url
          my-hledger-service-fetch-url))

  :config
  (add-hook 'hledger-view-mode-hook #'hl-line-mode)
  (add-hook 'hledger-view-mode-hook #'center-text-for-reading)

  (add-hook 'hledger-view-mode-hook
            (lambda ()
              (run-with-timer 1
                              nil
                              (lambda ()
                                (when (equal hledger-last-run-command
                                             "balancesheet")
                                  ;; highlight frequently changing accounts
                                  (highlight-regexp "^.*\\(savings\\|cash\\).*$")
                                  (highlight-regexp "^.*信用卡.*$"
                                                    'hledger-warning-face))))))

  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company))))

;; (use-package hledger-input
;;   :bind (("C-c e" . hledger-capture)
;;          :map hledger-input-mode-map
;;          ("C-c C-b" . popup-balance-at-point))
;;   :preface
;;   (defun popup-balance-at-point ()
;;     "Show balance for account at point in a popup."
;;     (interactive)
;;     (if-let ((account (thing-at-point 'hledger-account)))
;;         (message (hledger-shell-command-to-string (format " balance -N %s "
;;                                                           account)))
;;       (message "No account at point")))

;;   :config
;;   (setq hledger-input-buffer-height 20)
;;   (add-hook 'hledger-input-post-commit-hook #'hledger-show-new-balances)
;;   (add-hook 'hledger-input-mode-hook #'auto-fill-mode)
;;   (add-hook 'hledger-input-mode-hook
;;             (lambda ()
;;               (make-local-variable 'company-idle-delay)
;;               (setq-local company-idle-delay 0.1))))

;; }}}.
;; personal csv mode {{{

(use-package csv-mode
  :ensure t)

;; }}}
;; personal markdown mode {{{

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :hook ((markdown-mode . yas-minor-mode-on))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t

        markdown-content-type "application/xhtml+xml"
        markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
                             "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css")
        markdown-xhtml-header-content "
<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
<style>
body {
  box-sizing: border-box;
  max-width: 740px;
  width: 100%;
  margin: 40px auto;
  padding: 0 10px;
}
</style>

<link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>
<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
<script>
document.addEventListener('DOMContentLoaded', () => {
  document.body.classList.add('markdown-body');
  document.querySelectorAll('pre code').forEach((code) => {
    if (code.className != 'mermaid') {
      hljs.highlightBlock(code);
    }
  });
});
</script>

<script src='https://unpkg.com/mermaid@8.4.8/dist/mermaid.min.js'></script>
<script>
mermaid.initialize({
  theme: 'default',  // default, forest, dark, neutral
  startOnLoad: true
});
</script>
"
        markdown-gfm-additional-languages "Mermaid")

  ;; `multimarkdown' is necessary for `highlight.js' and `mermaid.js'
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))

  ;; Preview with built-in webkit
  (with-no-warnings
    (defun my-markdown-export-and-preview (fn)
      "Preview with `xwidget' if applicable, otherwise with the default browser."
      (if (featurep 'xwidget-internal)
          (progn
            (xwidget-webkit-browse-url (concat "file://" (markdown-export)))
            (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
              (when (buffer-live-p buf)
                (and (eq buf (current-buffer)) (quit-window))
                (pop-to-buffer buf))))
        (funcall fn)))
    (advice-add #'markdown-export-and-preview :around #'my-markdown-export-and-preview))
  )

;; (use-package markdown-mode+
;;   :ensure t
;;   :after markdown-mode
;;   :defer t)

(use-package poly-markdown
  :ensure t
  :after markdown-mode)

;; (use-package pandoc-mode :ensure t
;;   :hook (markdown-mode . pandoc-mode)
;;   :bind
;;   (:map markdown-mode-map
;;         ("C-c j" . pandoc-jump-to-reference)
;;         )
;;   )

(use-package mermaid-mode
  :ensure t)

;; }}}
;; personal grip mode {{{

;; 预览Markdown和Org
;; (use-package grip-mode
;;   :ensure t
;;   :diminish (grip-mode)
;;   :bind ((:map org-mode-map
;;                ("C-c C-v" . grip-mode))
;;          (:map markdown-mode-map
;;                ("C-c C-v" . grip-mode)))
;;   :config
;;   (require 'auth-source)
;;   (let ((credential (auth-source-user-and-password "emacs-grip.api.github.com")))
;;     (setq grip-github-user (car credential)
;;           grip-github-password (cadr credential)))

;;   ;; (setq grip-preview-use-webkit nil)
;;   )

;; }}} 
;; personal lsp mode {{{

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-l")
  :custom
  (lsp-xml-jar-file (expand-file-name (locate-user-emacs-file "org.eclipse.lemminx-uber.jar")))
  :hook ((sh-mode . lsp-deferred))
  :config
  ;(setq lsp-print-io t)
  (setq lsp-auto-guess-root t
        lsp-enable-xref t
        lsp-enable-snippet t
        lsp-eldoc-render-all nil
        lsp-enable-file-watchers nil
        lsp-enable-semantic-highlighting t
        lsp-modeline-diagnostics-scope :workspace
        lsp-diagnostics-provider :flymake)
  :ensure t)

;(use-package lsp-ui :ensure t)
(use-package lsp-ivy
  :ensure t
  :after (lsp)
  :bind (
         (:map lsp-command-map
               ("g s". lsp-ivy-workspace-symbol) )
         )
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-origami
  :ensure t
  :after (lsp origami)
  :hook ((lsp-after-open . lsp-origami-try-enable)))

;; }}}
;; personal dap mode {{{

(use-package dap-mode
  :commands (dap-mode dap-ui-mode)
  :bind ((:map dap-mode-map
               ("C-x C-d c" . dap-hydra)
               ("C-x C-d r" . dap-debug-last)
               ("C-x C-d C-r" . dap-debug)
               ("C-x C-d o" . dap-go-to-output-buffer)
               ("C-x C-d s" . dap-switch-session)
               ("C-x C-d f" . dap-switch-stack-frame)
               ("C-x C-d t" . dap-switch-thread)
               ("C-x C-d S" . dap-ui-sessions)
               ("C-x C-d L" . dap-ui-locals)
               ("C-x C-d E" . dap-ui-repl)
               )
         )
  :hook((dap-mode . dap-ui-mode)
        (dap-session-created . (lambda (_args) (dap-hydra)))
        (dap-stopped . (lambda (_args) (dap-hydra)))

        (python-mode . (lambda () (require 'dap-python)))
        (ruby-mode . (lambda () (require 'dap-ruby)))
        (go-mode . (lambda () (require 'dap-go)))
        (java-mode . (lambda () (require 'dap-java)))
        ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
        (php-mode . (lambda () (require 'dap-php)))
        (elixir-mode . (lambda () (require 'dap-elixir)))
        ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))
        (powershell-mode . (lambda () (require 'dap-pwsh)))
        )
  :ensure t
  )

;; }}}
;; personal yaml model {{{

(use-package yaml-mode
  :mode ("\\.yml$" "\\.yaml$")
  :hook ((yaml-mode . lsp-deferred)
         (yaml-mode . yas-minor-mode-on)
         )
  :config
  (define-key yaml-mode-map (kbd "C-c C-c") 'comment-region)
  :ensure t
  )

;; }}}
;; personal powershell model {{{
(use-package powershell
  :ensure t
  )
;; }}}
;; personal dockerfile model {{{

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
  :ensure t)

(use-package docker-tramp
  :ensure t)

;; }}}
;; personal nxml momdel {{{

(use-package nxml-mode
  :hook ((nxml-mode . lsp))
  :config
  (setq nxml-child-indent 4)
  )

;; }}}
;; personal lisp mode settings {{{

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'comment-region)

;; }}}
;; personal c cpp and objc mode settings {{{

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
           (lsp-deferred)
           (dap-mode t)
           (yas-minor-mode-on)
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
           ;(company-tabnine-buffer-enable)
           )
         )
  )

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda()
           (require `ccls)
           (set (make-local-variable 'lsp-enable-on-type-formatting) nil)
           ))
  :custom
  ;;(ccls-sem-highlight-method 'font-lock)
  (ccls-sem-highlight-method 'overlay)
  :config
  (eval-after-load "projectile"
    '(add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))
  :ensure t)

;; }}}
;; personal java mode settings {{{

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

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp-deferred)
  :init
  ;; (java-mode . (lambda () (lsp-ui-flycheck-enable t)))
  ;; (java-mode . lsp-ui-sideline-mode)
  ;; :config
  ;; (setq lsp-java-save-action-organize-imports nil)
  )

;(use-package treemacs :ensure t)
;; (if (not (eq 'windows-nt system-type))
;;     (use-package lsp-java
;;       :init
;;       ; (add-hook 'java-mode-hook 'lsp-deferred)
;;                                         ;(java-mode . (lambda () (lsp-ui-flycheck-enable t)))
;;                                         ;(java-mode . lsp-ui-sideline-mode)
;;       :config
;;       (setq lsp-java-save-action-organize-imports nil)
;;       :ensure t
;;       )
;;   )

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

;; }}}
;; personal kotlin mode settings {{{

;; (eval-after-load "compile"
;;   '(progn
;;      (add-to-list 'compilation-error-regexp-alist 'gradle-kotlin-1)
;;      (add-to-list
;;       'compilation-error-regexp-alist-alist
;;       '(gradle-kotlin-1
;;         "^[we]: \\(.+\\): (\\([0-9]+\\), \\([0-9]+\\)): .*" 1 2 3))
;;      (add-to-list 'compilation-error-regexp-alist 'gradle-kotlin-2)
;;      (add-to-list
;;       'compilation-error-regexp-alist-alist
;;       '(gradle-kotlin-2
;;         "^[we]: \\(.+\\):\\([0-9]+\\): .*" 1 2))
;;      ))

;; (use-package lsp-intellij
;;   :load-path "~/.emacs.d/lsp-intellij"
;;   )

;; (use-package eglot
;;   :ensure t)
  
(use-package kotlin-mode
  :mode "\\.kt\\'"
  :hook ((kotlin-mode . lsp-deferred) ;lsp-intellij-enable lsp-deferred 
         (kotlin-mode . yas-minor-mode-on)
         )
  :custom
  (lsp-kotlin-trace-server "verbose")
  (lsp-kotlin-trace-server t)
  :config
  (define-key kotlin-mode-map (kbd "C-c C-c") 'comment-region)
  :ensure t)

;; }}}
;; personal dart mode settings {{{

(use-package dart-mode
  :mode "\\.dart\\'"
  :hook ((dart-mode . yas-minor-mode-on))
  :init
  ;; (with-eval-after-load "projectile"
  ;;   (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  ;;   (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
  :custom
  (dart-format-on-save t)
  :config
  (define-key dart-mode-map (kbd "C-c C-c") 'comment-region)
  :ensure t)

(use-package lsp-dart 
  :ensure t 
  :hook (dart-mode . lsp-deferred))

(use-package hover :ensure t)

;; }}}
;; personal flutter {{{

(use-package flutter
  :ensure t
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path (expand-file-name "~/workspace/flutter"))
  )

;; }}}
;; personal go model {{{

(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (go-mode . yas-minor-mode-on)
         ;(go-mode . (lambda () (setq tab-width 4)))
         (go-mode . lsp-go-install-save-hooks)
         (go-mode . company-tabnine-buffer-enable)
         )
  :bind
  (:map go-mode-map
   ;; ("C-c C-g" . go-goto-map)
   ;; ("C-c C-r" . go-remove-unused-imports)
   ;; ("C-c C-f" . gofmt)
   ;; ("C-c C-k" . godoc)
   ("C-c C-c" . comment-region)
   )
  :ensure t
  :init
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))
   )
  )

;; }}}
;; personal make setup {{{

(use-package make-mode
  :mode (("\\.mk\\'" . makefile-gmake-mode)
         ("[Mm]akefile\\'" . makefile-gmake-mode)
         )
  :hook ((makefile-mode . yas-minor-mode-on)
         (makefile-mode . origami-mode)
         )
  )

;; }}}
;; personal C# mode {{{

(use-package csharp-mode
  :ensure t
  :hook ((csharp-mode . lsp)
         (csharp-mode . yas-minor-mode-on)
         )
  )

(use-package csproj-mode
  :ensure t)

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

;; }}}
;; personal perl mode settings {{{

;; (defvar perl-syntax-bin "perl" "The perl binary used to check syntax.")
;; (defun perl-syntax-check-only ()
;;   "Return a either nill or t depending on whether the current buffer passws perl`s syntax check."
;;   (interactive)
;;   (let ((buf (get-buffer-create "*Perl syntax check*")))
;;     (let ((syntax-ok (= 0 (save-excursion
;;                            (widen)
;;                            (call-process-region
;;                             (point-min) (point-max) perl-syntax-bin nil buf nil "-c")))))
;;       (if syntax-ok (kill-buffer buf)
;;         (display-buffer buf)))))
 
;; (defvar perl-syntax-mode nil "Check perl syntax before saving.")
;; (make-variable-buffer-local 'perl-syntax-mode)
;; (defun perl-syntax-write-hook ()
;;   "Check perl syntax during 'write-file-hooks' for 'perl-syntax-mode'"
;;   (if perl-syntax-mode
;;       (save-excursion
;;        (widen)
;;        (mark-whole-buffer)
;;        (not (perl-syntax-check-only)))
;;     nil))
 
;; (defun perl-syntax-mode (&optional arg)
;;   "Perl syntax checking minor mode."
;;   (interactive "p")
;;   (setq perl-syntax-mode
;;         (if (null arg)
;;             (not perl-syntax-mode)
;;           (> (prefix-numeric-value arg) 0)))
;;   (make-local-variable 'write-file-hooks)
;;   (if perl-syntax-mode
;;       (add-hook 'write-file-hooks 'perl-syntax-write-hook)
;;     (remove-hook 'write-file-hooks 'perl-syntax-write-hook)))
 
;; (if (not (assq 'perl-syntax-mode minor-mode-alist))
;;     (setq minor-mode-alist
;;           (cons '(perl-syntax-mode " Perl Syntax")
;;                 minor-mode-alist)))

(use-package cperl-mode
  :mode "\\.\\(pl\\|pm\\|pod\\)\\'"
  :bind ((:map cperl-mode-map
               ("C-x C-e" . perl-eval)
               ("C-c f" . perldoc-find-module)
               ("M-p" . cperl-perldoc)
               ("C-c C-c" . comment-region)
               ))
  :hook ((cperl-mode . yas-minor-mode-on)
         (cperl-mode . lsp)
         )
  :init
  (setq interpreter-mode-alist
      (cons '("perl" . cperl-mode)
            (cons '("perl5" . cperl-mode) interpreter-mode-alist)))
  :config
  (setq cperl-auto-newline nil)
  (setq cperl-auto-newline-after-colon nil)
  (setq cperl-electric-parens nil)
  )

;; }}}
;; personal python mode settings {{{

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (setq elpy-test-runner 'elpy-test-green-runner)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
  (setq dired-omit-files
        (concat dired-omit-files "\\|^__pycache__$"))
  :hook
  ((elpy-mode . company-tabnine-buffer-enable)
   )
  )

(use-package lsp-python-ms
  :ensure t
  :after (elpy)
  :init
  (setq lsp-python-ms-auto-install-server t)
  ;; (add-to-list 'pyvenv-post-activate-hooks
  ;;              (lambda () (setq lsp-python-ms-python-executable (concat pyvenv-virtual-env "bin/python")))
  ;;              'APPEND)
  :hook (python-mode . (lambda ()
                         (setq lsp-python-ms-python-executable "/usr/local/bin/python3")
                         (require 'lsp-python-ms)
                         (lsp-deferred)
                         (add-hook 'before-save-hook 'elpy-autopep8-fix-code nil t)))
  )

;; }}}
;; personal php mode {{{

(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-to-list 'mmm-mode-ext-classes-alist '(html-mode "\\.php[s345t]?\\'" html-php))
  (add-to-list 'auto-mode-alist '("\\.php[s345t]?\\'" . html-mode))
  :ensure t)

;; }}}
;; personal json mode {{{

(use-package json-mode
  :ensure t
  :hook ((json-mode . lsp-deferred))
  )

;; }}}
;; personal js mode {{{

(use-package js2-mode
  :mode "\\.js\\'"
  :hook ((js2-mode . lsp-deferred)
         (js2-mode . dap-mode)
         (js2-mode . yas-minor-mode-on)
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

;; }}}
;; personal rust mode {{{

(use-package rustic
  :ensure t
  :init
  (add-to-list 'exec-path (concat (getenv "HOME") "/.cargo/bin"))
  (setenv "PATH" (concat (getenv "HOME") "/.cargo/bin:" (getenv "PATH")))
  )

;; }}}
;; personal typescript mode {{{

(use-package typescript-mode
  :ensure t
  :bind (:map typescript-mode-map
              ("C-c C-c" . comment-region)
              )
  :init
  (defun lsp-typescript-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t))
  :hook ((typescript-mode . company-mode)
         (typescript-mode . yas-minor-mode-on)
         (typescript-mode . lsp-deferred)
         (typescript-mode . lsp-typescript-install-save-hooks)
         )
  :config
  (setq typescript-indent-level 2)
  )

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (typescript-mode . company-tabnine-buffer-enable)
;;          (before-save . tide-format-before-save)
;;          (js2-mode . tide-setup))
;;   )

(use-package jest-test-mode
  :ensure t
  :defer t
  :diminish nil
  :hook ((typescript-mode . jest-test-mode)
         (js2-mode . jest-test-mode)
         (vue-mode . jest-test-mode)
         ))

(use-package flymake-eslint
  :ensure t
  :hook ((typescript-mode . flymake-eslint-enable)
         (js2-mode . flymake-eslint-enable))
  )

;; }}}
;; personal Vue mode {{{

(use-package vue-mode
  :ensure t
  :hook ((vue-mode . lsp-deferred)
         (vue-mode . yas-minor-mode-on)
         )
  )

;; }}}
;; personal web mode (html, jsp) {{{

(use-package web-mode
  :mode ("\\.jsp\\'" "\\.html?\\'")
  :hook ((web-mode . yas-minor-mode-on)
         )
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    )
  :ensure t
  )

(use-package emmet-mode
  :hook (web-mode)
  :ensure t)

;; }}}
;; personal css mode {{{

(eval-after-load "css-mode"
  '(progn
     (setq cssm-indent-function 'cssm-c-style-indenter)
     (setq cssm-indent-level '2)
     ))

(add-to-list 'mmm-mode-ext-classes-alist '(html-mode nil html-css))

;; }}}
;; personal android develop settings {{{

(use-package android-mode
  :commands (android-mode android-logcat android-gradle)
  :config
  :ensure t)

;; }}}
;; personal groovy(gradel) mode {{{

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

;; (use-package flycheck-gradle
;;   :ensure t
;;   :defer t)

;; }}}
;; personal cmake mode {{{

; pip3 install cmake-language-server
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook ((cmake-mode . lsp-deferred)
         (cmake-mode . yas-minor-mode-on)
         (cmake-mode . company-tabnine-buffer-enable)
         )
  :config
  (setq cmake-tab-width 2)
  :ensure t
  )

(use-package cmake-font-lock
  :requires cmake-mode
  :commands (cmake-font-lock-activate)
  :hook ((cmake-mode-hook . cmake-font-lock-activate))
  :ensure t
  )

;; }}}
;; personal solidity mode {{{

(use-package solidity-mode
  :mode "\\.sol\\'"
  :ensure t)

;; }}}
;; personal swift mode {{{

;; (use-package company-sourcekit
;;   :after company
;;   :ensure t)

(use-package swift-mode
  :after (lsp)
  :mode "\\.swift\\'"
  :hook ((swift-mode . lsp-deferred)
         ;; (swift-mode . (lambda () (add-to-list (make-local-variable 'company-backends) 'company-sourcekit)))
         ;; (swift-mode . company-mode)
         (swift-mode . yas-minor-mode-on)
         )
  :bind ((:map swift-mode-map
              ("C-c C-c" . comment-region)
              ))
  :custom
  (swift-mode:multiline-statement-offset 4)
  (swift-mode:parenthesized-expression-offset 4)
  :ensure t
  )

(when (eq 'darwin system-type)
  (use-package lsp-sourcekit
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
    :ensure t))

;; }}}
;; personal R mode {{{

(use-package ess
  :ensure t
  :defer t
  :mode
  (("\\.sp\\'"           . S-mode)
   ("/R/.*\\.q\\'"       . R-mode)
   ("\\.[qsS]\\'"        . S-mode)
   ("\\.ssc\\'"          . S-mode)
   ("\\.SSC\\'"          . S-mode)
   ("\\.[rR]\\'"         . R-mode)
   ("\\.[rR]nw\\'"       . Rnw-mode)
   ("\\.[sS]nw\\'"       . Snw-mode)
   ("\\.[rR]profile\\'"  . R-mode)
   ("NAMESPACE\\'"       . R-mode)
   ("CITATION\\'"        . R-mode)
   ("\\.omg\\'"          . omegahat-mode)
   ("\\.hat\\'"          . omegahat-mode)
   ("\\.lsp\\'"          . XLS-mode)
   ("\\.do\\'"           . STA-mode)
   ("\\.ado\\'"          . STA-mode)
   ("\\.[Ss][Aa][Ss]\\'" . SAS-mode)
   ("\\.jl\\'"           . ess-julia-mode)
   ("\\.[Ss]t\\'"        . S-transcript-mode)
   ("\\.Sout"            . S-transcript-mode)
   ("\\.[Rr]out"         . R-transcript-mode)
   ("\\.Rd\\'"           . Rd-mode)
   ("\\.[Bb][Uu][Gg]\\'" . ess-bugs-mode)
   ("\\.[Bb][Oo][Gg]\\'" . ess-bugs-mode)
   ("\\.[Bb][Mm][Dd]\\'" . ess-bugs-mode)
   ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
   ("\\.[Jj][Oo][Gg]\\'" . ess-jags-mode)
   ("\\.[Jj][Mm][Dd]\\'" . ess-jags-mode))
  :commands R
  ;; :hook ((ess-mode-hook . yas-minor-mode)
  ;;        (ess-mode-hook . company-mode)
  ;;        (inferior-ess-mode-hook . company-mode)
  ;;        )
  :bind (
         ;; (:map inferior-ess-mode-map
         ;;       ("C-c C-c" . comment-region)
         ;;       ("C-j" . comint-next-input)
         ;;       ("C-k" . comint-previous-input)
         ;;       )
         ;; (:map ess-mode-map
         ;;       ("C-c C-c" . comment-region)
         ;;       )
         )
  :config
  (setq ess-first-continued-statement-offset 2
        ess-indent-level 2
        ess-continued-statement-offset 2
        ess-brace-offset 0
        ess-arg-function-offset 4
        ess-expression-offset 2
        ess-else-offset 0
        ess-close-brace-offset 0
        ess-nuke-trailing-whitespace-p t
        ess-default-style 'DEFAULT
        ess-ask-for-ess-directory nil
        ess-eval-visibly nil
        ;; ess-directory user-project-directory
        ;; Keep global .Rhistory file.
        ess-history-directory "~/.R/"
        inferior-R-args "-q" ; I donnot want to print startup message
        ess-use-company nil ; don't auto-insert ess backends
        )
  ;; (defun my-ess-config ()
  ;;   (make-variable-buffer-local 'company-backends)
  ;;   (add-to-list 'company-backends
  ;;                '(company-tabnine company-R-args company-R-objects company-dabbrev-code :separate)))
  ;; (add-hook 'ess-mode-hook #'my-ess-config)
  )

;; (use-package poly-R :ensure t)

;; (use-package polymode :ensure t :after (poly-R)
;;   :mode (("\\.[SR]nw\\'" . poly-noweb+r-mode)
;;          ("\\.Rmd\\'" . Rmd-mode))
;;   :init
;;   (progn
;;     (defun Rmd-mode ()
;;       "ESS Markdown mode for Rmd files."
;;       (interactive)
;;       (require 'poly-R)
;;       (require 'poly-markdown)
;;       (R-mode)
;;       (yaml-mode)
;;       (poly-markdown+r-mode))

;;     ;; do this in R process
;;     ;; library (rmarkdown); render ("file_name.Rmd")
;;     (defun ess-rmarkdown ()
;;       (interactive)
;;       "Compile R markdown (.Rmd). Should work for any output type."
;;       "http://roughtheory.com/posts/ess-rmarkdown.html"
;;       ;; Check if attached R-session
;;       (condition-case nil
;;           (ess-get-process)
;;         (error
;;          (ess-switch-process)))
;;       (save-excursion
;;         (let* ((sprocess (ess-get-process ess-current-process-name))
;;                (sbuffer (process-buffer sprocess))
;;                (buf-coding (symbol-name buffer-file-coding-system))
;;                (R-cmd
;;                 (format "library (rmarkdown); rmarkdown::render(\"%s\")"
;;                         buffer-file-name))
;;                (message "Running rmarkdown on %s" buffer-file-name)
;;                (ess-execute R-cmd 'buffer nil nil)
;;                (switch-to-buffer rmd-buf)
;;                (ess-show-buffer (buffer-name-sbuffer) nil)))))
;;     )
;;   )

;; }}}
;; personal Tex {{{

(use-package auctex
  :defer t
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode-on)
         (LaTeX-mode . lsp-deferred)
         )
  )

(use-package latex-extra
  :ensure t
  :hook (LaTeX-mode . latex-extra-mode)
  )

(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . cdlatex-mode)
         (LaTeX-mode . (lambda ()
                         (LaTeX-math-mode t)
                         (tex-fold-mode t)
                         (turn-on-reftex)
                         (auto-fill-mode t)
                        (TeX-engine-set 'xetex)
                         )
                     )
         )
  :config
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    (setq-default TeX-engine 'xetex)

    (setq reftex-plug-into-AUCTeX t)
    (setq TeX-PDF-mode t)
    )
  )

;; }}}
;; personal Ragel mode {{{
(define-generic-mode 'ragel-mode
  '(?#) ;; comments
  '( 
    ;; keywords
    "machine" "action" "access" "context" "include" "import" "export" "prepush" "postpop"
    "when" "inwhen" "outwhen" "err" "lerr" "eof" "from" "to" 
    "alphtype" "getkey" "write" 
    ;; rules
    "any" "ascii" "extend" "alpha" "digit" "alnum" "lower" "upper" 
    "xdigit" "cntrl" "graph" "print" "punct" "space" "zlen" "empty" 
    ;; inline code matching
    "fpc" "fc" "fcurs" "fbuf" "fblen" "ftargs" "fstack"
    "fhold" "fgoto" "fcall" "fret" "fentry" "fnext" "fexec" "fbreak"
    )
  '(
    ;; literals
    ;("\\([^\\)]*\\)" . font-lock-constant-face)
    ;("\\[[[^\\]]*\\]" . font-lock-constant-face)
    ("\(\"\\?'\"\'|\\?\"'\|'[^']*'\|\"[^\"]*\"\)" . font-lock-constant-face)

    ;; Numbers
    ("[0-9][0-9]*" . font-lock-constant-face)
    ("0x[0-9a-fA-F][0-9a-fA-F]*" . font-lock-constant-face)
    
    ;; Operators
    ("[>$%@]" . font-lock-constant-face)
    ("<>\|<" . font-lock-constant-face)
                                        ;                    ("[>\<$%@][!\^/*~]" . font-lock-constant-face)
                                        ;                    ("[>$%]?" . font-lock-constant-face)
                                        ;                    ("<>[!\^/*~]" . font-lock-constant-face)
    ("=>" . font-lock-constant-face)
    ("->" . font-lock-constant-face)

    (":>" . font-lock-constant-face)
    (":>>" . font-lock-constant-face)
    ("<:" . font-lock-constant-face)
    )
  '(".rl\\'") 
  nil
  "Generic mode for mmm-mode editing .rl files.")

(mmm-add-classes
 '((embedded-ragel
    :submode ragel-mode
    :save-matches 1
    :case-fold-search 1
    :face mmm-declaration-submode-face
    :front "%%{"
    :include-front t
    ;; :front-offset (end-of-line 1)
    :back "}%%"
    :include-back t
    ;; :back-offset (end-of-line 1)
    ;; line match? "%%[^{]" "%%$"
    )))
(mmm-add-mode-ext-class 'c++-mode nil 'embedded-ragel)
(mmm-add-mode-ext-class 'cperl-mode nil 'embedded-ragel)
(mmm-add-mode-ext-class 'c-mode nil 'embedded-ragel)

(add-to-list 'mmm-c-derived-modes 'embedded-ragel)

;; }}}
;; personal sql mode {{{

(use-package sql
  :bind ((:map sql-mode-map
               ("C-c C-c" . comment-region)
               ))
  )

(use-package sqlformat
  :ensure t
  :bind ((:map sql-mode-map
               ("C-c C-f" . sqlformat)
               ))
  )

;; }}}
;; utility match-parten {{{

(global-set-key "%" 'match-paren)
        
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; }}}
;; utility ascii table {{{

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

;; }}}
;; run server {{{

(require 'server)
(unless (server-running-p) (server-start))

;; }}}
;; emacs local {{{

(if (file-exists-p "~/.emacs.local")
    (load-file "~/.emacs.local"))

;; }}}
;; desktop {{{

(setq desktop-load-locked-desktop t)
(desktop-save-mode)
(desktop-read)

;; }}}
;; auto-upate {{{

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  :ensure t)

;; }}}
