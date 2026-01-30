;; Anton Rybakov init.el file

;; Misc configuration function
(defun ar/read-whole-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun ar/read-elisp-file (path)
  (read (ar/read-whole-file path)))

(defun ar/user-home-dir ()
  (if (eq system-type 'windows-nt)
    (getenv "USERPROFILE")
    (getenv "HOME")))

(defun ar/org-roam-directory ()
  (file-name-concat (ar/user-home-dir) ".org-roam"))

(defun ar/org-roam-capture-templates ()
  (ar/read-elisp-file (file-name-concat (ar/org-roam-directory) ".capture_templates.el")))

(defun ar/delete-file-and-buffer ()
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

(defun ar/kill-nonexistent-file-buffers ()
  "Kill all file-visiting buffers whose files no longer exist."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      (when (and filename (not (file-exists-p filename)))
        (kill-buffer buf)))))

(defun ar/dired-find-file ()
  "Find a file in the current dired directory"
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (find-file (read-file-name "Find file:"))))

;; Global hotkeys
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 4)))
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 4)))
(global-set-key (kbd "C-c f i") (lambda () (interactive) (find-file user-init-file)))

;; UI configuration
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(add-to-list 'default-frame-alist
             '(font . "Sarasa Mono J-15")) 

;; State files handling
(defmacro ar/define-state-directory (dirname)
  `(defun ,(intern (format "ar/%s-state-directory" dirname)) ()
     (let ((destination-directory
	     (if (eq system-type 'windows-nt)
	       (concat user-emacs-directory ,(format "state/%s" dirname))
	       ,(format "~/.local/state/emacs/%s" dirname))))
       (make-directory destination-directory :parents)
       destination-directory)))
(ar/define-state-directory backups)
(ar/define-state-directory autosave)
(ar/define-state-directory locks)

(setq
  ;; Custom variables file
  custom-file (concat user-emacs-directory "custom.el")
  ;; Backup configuration section
  backup-directory-alist '(("." . (ar/backups-state-directory)))
  backup-by-copying t
  delete-old-versions t
  kept-new-versions 2
  kept-old-versions 2
  version-control t
  ;; Autosave location
  auto-save-file-name-transforms `((".*" ,(ar/autosave-state-directory) t))
  ;; Lock files location
  lock-file-name-transforms `((".*" ,(ar/locks-state-directory) t)))
(load custom-file)

;; Windows specific
(if (equal system-type 'windows-nt)
  (progn (require 'ls-lisp)
         (setq ls-lisp-dirs-first t)
         (setq ls-lisp-use-string-collate nil)
         (setq ls-lisp-verbosity nil)))

;; macOS specific
(if (equal system-type 'darwin)
    (progn (setq insert-directory-program "gls"))) ;; TODO: Check that gls is installed
(setq dired-listing-switches "-D -alv --group-directories-first")

(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))


;; Packages
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 1)
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;; TODO: Explore how multiple-cursors and meow can be combined
;; (use-package multiple-cursors)

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

(use-package consult)

; (use-package embark)

(use-package embark-consult)

(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

(use-package cape)

(use-package perfect-margin)

(use-package modus-themes
  :custom
  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     (fg-heading-2 magenta-cooler)))
  :init
  (load-theme 'modus-operandi t))

(use-package magit)

(use-package org-roam
  :custom
  (org-roam-directory (ar/org-roam-directory))
  (org-roam-capture-templates (ar/org-roam-capture-templates))
  (org-roam-node-display-template "${title}")
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))
