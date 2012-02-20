;;; color-theme-library.el --- The real color theme functions

;; Copyright (C) 2005, 2006  Xavier Maillard <zedek@gnu.org>
;; Copyright (C) 2005, 2006  Brian Palmer <bpalmer@gmail.com>

;; Version: 0.0.9
;; Keywords: faces
;; Author: Brian Palmer, Xavier Maillard
;; Maintainer: Xavier Maillard <zedek@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ColorTheme

;; This file is not (YET) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;; Code:

(add-to-list 'load-path "~/.emacs.d/")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(cua-mode t nil (cua-base))
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(safe-local-variable-values (quote ((c-file-offsets (innamespace . 0) (substatement-open . 0)) (c-brace-offset . 0) (c-file-offsets (innamespace . 0) (inline-open . 0) (case-label . +)))))
 '(scroll-bar-mode (quote right))
 '(show-paren-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(auto-fill-mode -1)
(setq ediff-split-window-function 'split-window-horizontally)
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/color-theme-mibrahim.el")
(color-theme-mibrahim)
;(color-theme-clarity)
;;(color-theme-lethe)
;(global-font-lock-mode t)
(require 'actionscript-mode)
(require 'php-mode)
(set-default-font "Monospace-8")
;;(set-default-font "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-1")
(setq auto-mode-alist (cons '("\\.as$" . actionscript-mode) auto-mode-alist))
(global-set-key [f12] 'compile)
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;;(autoload 'javascript-mode "javascript" nil t)
;;(add-hook 'window-configuration-change-hook
;;	  (lambda ()
;;	    (setq frame-title-format
;;		  (concat
;;		   "" ;;invocation-name ":" ;;"@" system-name ": "
;;		   (replace-regexp-in-string
;;		    (concat "/home/" user-login-name) "~"
;;		    (or buffer-file-name "%b"))))))
(xterm-mouse-mode)
(mouse-wheel-mode)
(setq visible-bell nil)
(setq-default fill-column 99999)
(setq fill-column 99999)

;; Transparency stuff
;(defun djcb-opacity-modify (&optional dec)
;  "modify the transparency of the emacs frame; if DEC is t,
;    decrease the transparency, otherwise increase it in 10%-steps"
;  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
;          (oldalpha (if alpha-or-nil alpha-or-nil 100))
;          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
;    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
;      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))
;
; ;; C-8 will increase opacity (== decrease transparency)
; ;; C-9 will decrease opacity (== increase transparency
; ;; C-0 will returns the state to normal
;(global-set-key (kbd "C-8") '(lambda()(interactive)(djcb-opacity-modify)))
;(global-set-key (kbd "C-9") '(lambda()(interactive)(djcb-opacity-modify t)))
;(global-set-key (kbd "C-0") '(lambda()(interactive)
;                               (modify-frame-parameters nil `((alpha . 100)))))
(setq term-default-bg-color nil)
