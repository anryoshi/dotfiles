(setq straight-repository-branch "develop")

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
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

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package paredit)

(use-package racket-mode)

(use-package sicp)

(use-package seoul256-theme
  :init
  (setq seoul256-background 256)
  (load-theme 'seoul256 t))

(use-package anki-editor
  :defer t
  :straight (:repo "anki-editor/anki-editor"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(desktop-save-mode 1)

(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist
             '(font . "Sarasa Mono J-16"))

;; Backup, autosave and lock files configuration
(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.local/state/emacs/backups"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)

(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.local/state/emacs/autosave/\\1" t)))

(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.local/state/emacs/locks/\\1" t)))

(require 'ls-lisp)

(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-string-collate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ruby . t)))