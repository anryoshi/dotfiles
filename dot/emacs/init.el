;; This is temporary way to do local parameterization
;; of the configuration file. Until better way invented
(let ((local-config "~/.config/emacs/local.el"))
  (if (file-exists-p local-config)
      (load-file local-config)))

;; Switching from `master` to `develop` branch
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

;; Used packages

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package paredit)

(use-package racket-mode)

(use-package sicp)

(use-package seoul256-theme
  :init
  (setq seoul256-background 256)
  (load-theme 'seoul256 t))

(use-package org-roam
  :custom
  (org-roam-directory
   (if (boundp 'anryoshi-org-roam-directory)
       (file-truename anryoshi-org-roam-directory)))
  (org-roam-capture-templates
   (if (boundp 'anryoshi-org-roam-capture-templates)
       anryoshi-org-roam-capture-templates))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))

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

(use-package magit)

(use-package writeroom-mode)

(use-package telega
  :custom
  (telega-use-docker t))

;; Configuration

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(desktop-save-mode 1)

(setq inhibit-startup-screen t)

(setq-default indent-tabs-mode nil)

(add-to-list 'default-frame-alist
             '(font . "Sarasa Mono J-18"))

;; Backup, autosave and lock files configuration
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

(setq dired-listing-switches "-D -alv --group-directories-first")

;; ls-lisp configuration for Windows

(require 'ls-lisp)

(setq ls-lisp-dirs-first t)
(setq ls-lisp-use-string-collate nil)
(setq ls-lisp-verbosity nil)

;; Org-roam
(setq org-roam-node-display-template "${title}")

;; Sandbox section

(defun delete-file-and-buffer ()
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

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (mapcar #'org-roam-node-file (org-roam-node-list))))
