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

(use-package meow
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :config
  (meow-motion-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
'("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("M" . meow-page-up)
   '("n" . meow-search)
   '("N" . meow-page-down)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))
   (meow-global-mode 1))

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
   '((fg-heading-2 magenta-cooler)))
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
