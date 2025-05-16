;; Anton Rybakov init.el file

;; Common configuration functions
(defun ar-read-whole-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun ar-read-elisp-file (path)
  (read (ar-read-whole-file path)))

(defun ar-user-home-dir ()
  (if (eq system-type 'windows-nt)
    (getenv "USERPROFILE")
    (getenv "HOME")))

(defun ar-org-roam-directory ()
  (file-name-concat (ar-user-home-dir) ".org_roam"))

(defun ar-org-roam-capture-templates ()
  (ar-read-elisp-file (file-name-concat (ar-org-roam-directory) ".capture_templates.el")))

(defun ar-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(defun ar-dired-find-file ()
  "Find a file in the current dired directory"
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (find-file (read-file-name "Find file:"))))

;; Global hotkeys
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 4)))
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 4)))
(global-set-key (kbd "C-c f i") (lambda () (interactive) (find-file user-init-file)))

;; Emacs built-in variables & configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(desktop-save-mode 1)

(setq inhibit-startup-screen t)
(setq frame-resize-pixelwise t)

(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist
             '(font . "Sarasa Mono J-18")) ;; TODO: Make this font size host dependent

(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.local/state/emacs/backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(let ((save-files-directory "~/.local/state/emacs/autosave/"))
  (make-directory save-files-directory :parents)
  (setq auto-save-file-name-transforms
    `((".*" ,save-files-directory t))))

(let ((lock-files-directory "~/.local/state/emacs/locks/"))
  (make-directory lock-files-directory :parents)
  (setq lock-file-name-transforms
    `((".*" ,lock-files-directory t))))

(if (equal system-type 'darwin)
    (progn (setq insert-directory-program "gls"))) ;; TODO: Check that gls is installed
(setq dired-listing-switches "-D -alv --group-directories-first")

(if (equal system-type 'windows-nt)
  (progn (require 'ls-lisp)
         (setq ls-lisp-dirs-first t)
         (setq ls-lisp-use-string-collate nil)
         (setq ls-lisp-verbosity nil)))

(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))

(setq org-confirm-babel-evaluate nil)

(defun ar-org-babel-to-buffer ()
  "A function to efficiently feed babel code block result to a separate buffer"
  (interactive)
  (save-selected-window
    (org-open-at-point)
    (org-babel-remove-result)))

(defun ar-org-mode-config ()
  "To use with `org-mode-hook'"
  (local-set-key (kbd "C-c C-`") 'ar-org-babel-to-buffer))

(defun ar-dired-mode-config ()
  "To use with `dired-mode-hook'"
  (local-set-key (kbd "C-x M-f") 'ar-dired-find-file))

(add-hook 'org-mode-hook 'ar-org-mode-config)
(add-hook 'dired-mode-hook 'ar-dired-mode-config)

;; Bootstraping straight.el
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package
(setq use-package-always-defer t)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; Used packages
(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package multiple-cursors)

(use-package paredit)

(use-package racket-mode)

(use-package sicp)

(use-package seoul256-theme
  :init
  (setq seoul256-background 256)
  (load-theme 'seoul256 t))

(use-package org-roam
  :custom
  (org-roam-directory (ar-org-roam-directory))
  (org-roam-capture-templates (ar-org-roam-capture-templates))
  (org-roam-node-display-template "${title}")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))

(use-package org-pomodoro)

(use-package ob-racket
  :after org
  :config
  (add-hook 'ob-racket-pre-runtime-library-load-hook
	      #'ob-racket-raco-make-runtime-library)
  :straight (ob-racket
	       :type git :host github :repo "hasu/emacs-ob-racket"
	       :files ("*.el" "*.rkt")))

(use-package anki-editor
  :defer t
  :straight (:repo "anki-editor/anki-editor"))

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package magit)

(use-package writeroom-mode)

(use-package focus)

(use-package telega
  :custom
  (telega-use-docker t))

(use-package perfect-margin)
