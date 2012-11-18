;;
;; emacs starter kit v2, http://technomancy.us/153
;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; defined before (package-initialize) is called so it's available for
;; use within username.el scripts.
(defun ensure-packages (ps)
  "install any missing packages in ps"
  (dolist (p ps)
    (when (not (package-installed-p p))
      (package-install p))))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell
                                  starter-kit-bindings starter-kit-js
                                  starter-kit-ruby scpaste gist
                                  clojure-mode clojure-test-mode
                                  markdown-mode yaml-mode paredit
                                  magit color-theme color-theme-solarized
                                  ess
                                  auto-complete ac-slime
                                  projectile
                                  midje-mode
                                  yasnippet))

(ensure-packages my-packages)

;; friendly colors
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; Emacs launched through Spotlight isn't run via a shell and thus
;; isn't in an environment where ~/.bash* have run.
(setenv "PATH"
        (concat (concat (getenv "HOME") "/bin" ":")
                "/usr/local/bin" ":"
                "/usr/local/sbin" ":"
                "/usr/local/share/python" ":"
                "/opt/local/bin/" ":"
                "/usr/texbin" ":"
                (getenv "PATH")))

;; Add brew paths to exec-path so things like aspell and markdown can
;; be found. ~/bin too, for good measure.
(push "/usr/local/share/python" exec-path)
(push "/usr/local/sbin" exec-path)
(push "/usr/local/bin" exec-path)
(push (concat (getenv "HOME") "/bin") exec-path)
(push "/opt/local/bin/" exec-path)
(push "/usr/texbin/" exec-path)

;; teach magit where to find projects
(setq magit-repo-dirs
      (list
       (concat (getenv "HOME") "/repos")
       (concat (getenv "HOME") "/dev")
       (concat (getenv "HOME") "/workspace")))

(require 'magit)

;; misc
(put 'upcase-region 'disabled nil)
(push '("Rakefile" . ruby-mode) auto-mode-alist)
(push '("\\.md$" . markdown-mode) auto-mode-alist)
(push '("\\.markdown$" . markdown-mode) auto-mode-alist)
(push '("\\.yml$" . yaml-mode) auto-mode-alist)
(push '("\\.cljs$" . clojure-mode) auto-mode-alist)
(column-number-mode t)

;; no more tabs
(setq indent-tabs-mode nil)
(setq c-basic-offset 2)
(setq tab-width 4)

;; auto complete setup
(require 'auto-complete)
(global-auto-complete-mode t)

;; clojure env tweaks
(require 'clojure-mode)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
(add-hook 'slime-repl-mode-hook 'esk-turn-on-paredit)
(define-key clojure-mode-map (kbd "C-c v") 'slime-eval-buffer)
(global-set-key (kbd "C-c C-j") 'clojure-jack-in)
;; setup autocomplete for Clojure slime
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(require 'midje-mode)
(add-hook 'clojure-mode-hook 'midje-mode)

(add-hook 'prog-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; configure gist
;; don't forget `git config --global github.user`, &c.
;; (require 'gist)
;; (setq gist-view-gist t)
;; (setq gist-use-curl t)
;; (push '(slime-repl-mode . "clj") gist-supported-modes-alist)

(defun lein-deps ()
  (interactive)
  (let ((proj-dir (locate-dominating-file default-directory "project.clj")))
    (when (not proj-dir)
      (error "cannot find project.clj"))
    (shell-command (format "cd %s && lein clean, deps &" proj-dir)
                   "*lein-deps*")))

(defun lein-new (path)
  (interactive "FNew project directory: ")
  (let ((parent (file-name-directory (file-truename path)))
        (target (file-name-nondirectory (file-truename path))))
    (shell-command
     (format "cd %s && lein new %s &" parent target)
     "*lein-new*")))

;; orgy-goodness
;;(add-to-list 'load-path "~/.emacs.d/org-7.8.11/lisp")
(require 'org-install)
(require 'org)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(setq org-hide-leading-stars t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; babel-foo
(require 'ob-clojure)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (css . t)
   (emacs-lisp . t)
   (js . t)
   (R . t)
   (perl . t)
   (python . t)
   (R . t)
   (sh . t)))

;; set OS X specific variables
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(set-face-attribute 'default nil :height 100)

(add-to-list 'load-path "~/.emacs.d/")
(require 'hacks)
(put 'ido-exit-minibuffer 'disabled nil)

(require 'ess)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-font-lock-symbols t t)
 '(org-agenda-files (quote ("~/structures/typhoon/typhoon-volatility.org")))
 '(org-export-babel-evaluate nil)
 '(org-src-fontify-natively t)
 '(ido-max-work-directory-list 150)
 '(ido-max-work-file-list 500)
 '(org-src-fontify-natively t)
 '(haskell-font-lock-symbols t)
 '(org-src-fontify-natively t)
 '(yas/root-directory "~/.emacs.d/snippets/" nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;;(set-face-attribute 'default nil :family "Anonymous Pro" :height 120)
(set-face-attribute 'default nil :family "Monaco" :height 90)

(require 'projectile)
(projectile-global-mode) ;; to enable in all buffers

;; tab width should be 4 in most languages
(setq tab-width 4)

(add-to-list 'load-path "~/.emacs.d/google-maps/")
(require 'google-maps)

(setq haskell-font-lock-symbols t)

(set-face-attribute 'default nil
                    :family "Inconsolata" :height (case system-type
                                                    ('gnu/linux 95)
                                                    ('darwin 125)) :weight 'normal)

;; these additions from https://github.com/magnars/ of EmacsRocks fame
;;
;; git submodule add https://github.com/magnars/mark-multiple.el.git vendor/mark-multiple
;; git submodule add https://github.com/magnars/expand-region.el.git vendor/expand-region
;; git submodule add https://github.com/magnars/annoying-arrows-mode.el.git vendor/annoying-arrows
;; git submodule add https://github.com/winterTTr/ace-jump-mode vendor/ace-jump-mode

(add-to-list 'load-path "~/.emacs.d/vendor/mark-multiple/")
(require 'mark-more-like-this)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(add-to-list 'load-path "~/.emacs.d/vendor/expand-region/")
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Annoying arrows mode
(add-to-list 'load-path "~/.emacs.d/vendor/annoying-arrows/")
(require 'annoying-arrows-mode)
(global-annoying-arrows-mode)

;; Push mark when using ido-imenu
(defvar push-mark-before-goto-char nil)

(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push-mark)))

(defun ido-imenu-push-mark ()
  (interactive)
  (let ((push-mark-before-goto-char t))
    (ido-imenu)))

;; ace jump mode
(add-to-list 'load-path "~/.emacs.d/vendor/ace-jump-mode/")
(require 'ace-jump-mode)
;; Quickly jump in document with ace-jump-mode
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode)

;; additional submodules
;; git submodule add https://github.com/mbunkus/simple-rtm.git vendor/simple-rtm
(add-to-list 'load-path "~/.emacs.d/vendor/simple-rtm/lisp")
(autoload 'simple-rtm-mode "simple-rtm" "Interactive mode for Remember The Milk" t)
