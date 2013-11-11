;;; helm-css-scss.el --- CSS/SCSS selector with helm interface -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2013 by Shingo Fukuyama

;; Version: 1.1
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/helm-css-scss
;; Created: Oct 18 2013
;; Keywords: scss css mode helm
;; Package-Requires: ((helm "1.0") (emacs "24"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:

;; Example config

;; ----------------------------------------------------
;; (require 'helm-config)

;; (add-to-list 'load-path "~/.emacs.d/elisp/helm-css-scss")
;; (require 'helm-css-scss)

;; ;; Allow comment inserting depth at each end of a brace
;; (setq helm-css-scss-insert-close-comment-depth 2)

;; ;; nil is horizontally. t is vertically
;; (setq helm-css-scss-split-window-vertically nil)

;; ;; Set local keybind map for css-mode / scss-mode
;; (dolist ($hook '(css-mode-hook scss-mode-hook))
;;   (add-hook
;;    $hook
;;    (lambda ()
;;      (local-set-key (kbd "s-i") ;; [command + i]
;;                     'helm-css-scss)
;;      (local-set-key (kbd "s-b") ;; [command + b]
;;                     'helm-css-scss-back-to-last-point)
;;      (local-set-key (kbd "s-c") ;; [command + c]
;;                     'helm-css-scss-insert-close-comment)
;;      (local-set-key (kbd "s-n") ;; [command + n]
;;                     'helm-css-scss-move-and-echo-next-selector)
;;      (local-set-key (kbd "s-p") ;; [command + p]
;;                     'helm-css-scss-move-and-echo-previous-selector)
;;            )))
;; ----------------------------------------------------

;; This program has two main functions

;; (helm-css-scss)
;;   Easily jumping between CSS/SCSS selectors powerd by helm.el

;; (helm-css-scss-insert-close-comment &optional $depth)
;;   Insert inline comment like " //__ comment" at the next of
;;   a close brace "}". If it's aleardy there, update it.
;;   You can also specify a nest $depth of selector.
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'helm)

(defgroup helm-css-scss nil
  "Open helm-swoop."
  :prefix "helm-swoop-" :group 'helm)

(defface helm-css-scss-target-line-face
  '((t (:background "#e3e300" :foreground "#333333")))
  "Face for helm-css-scss target line"
  :group 'helm-css-scss)

;;; config -----------------------------

(defvar helm-css-scss-insert-close-comment-depth 3
  "Set SCSS style nest depth")

(defvar helm-css-scss-split-window-vertically nil
  "If it's nil helm window will appear horizontally")

(defvar helm-css-scss-include-commented-selector nil
  "Don't list selectors which is commented")

(defvar helm-css-scss-split-window-function
  (lambda ($buf)
    (if helm-css-scss-split-window-vertically
        (split-window-vertically)
      (split-window-horizontally))
    (other-window 1)
    (switch-to-buffer $buf))
  "Change the way to split window only when `helm-css-scss' is calling")

;; Avoide compile error for apply buffer local variable
(defvar helm-css-scss-cache)
(defvar helm-css-scss-last-point)

(defvar helm-css-scss-first-time nil
  "For keep line position when `helm-css-scss' calls")

(defvar helm-css-scss-overlay nil
  "Store overlay object")

(defvar helm-css-scss-target-buffer nil
  "For overlay")

(defvar helm-css-scss-at-screen-top helm-display-source-at-screen-top
  "For enable scrolling margin")

(defun helm-css-scss-target-overlay ()
  "Add color to target line"
  (overlay-put (setq helm-css-scss-overlay
                     (make-overlay (point-at-bol) (point)))
               'face 'helm-css-scss-target-line-face))

;;; common parts -----------------------------

(defun helm-css-scss-nthcar ($i $l)
  "Return n($i) of values from the head of a list($l)"
  (loop for $k from 1 to $i
        collect (nth (- $k 1) $l) into $res
        finally return (delq nil $res)))

(defun helm-css-scss-substruct-last-string ($text $key)
  "Return the tail of $text without $key strings"
  (while (string-match $key $text)
      (setq $text (substring $text (1+ (string-match $key $text)))))
  $text)

(defun helm-css-scss-trim-whitespace ($s)
  "Return string without whitespace at the both beginning and end"
  (if (string-match "\\`\\(?:\\s-+\\)?\\(.+?\\)\\(?:\\s-+\\)?\\'" $s)
      (match-string 1 $s)
    $s))

(defun helm-css-scss-delete-all-matches-in-buffer ($regexp)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward $regexp nil t)
      (delete-region (match-beginning 0) (match-end 0)))))

;;; scan selector -----------------------------

(defun helm-css-scss-comment-p (&optional $point)
  (or $point (setq $point (point)))
  (nth 4 (parse-partial-sexp (point-min) $point)))

(defun helm-css-scss-selector-to-hash ()
  "Collect all selectors and make hash table"
  (let ($s $beg ($end nil) $hash $dep ($max nil) ($sl nil))
    (setq $hash (make-hash-table :test 'equal))
    (save-excursion
      (goto-char (point-min))
      (while (setq $s (helm-css-scss-selector-next))
        (setq $beg (point))
        (setq $end (scan-sexps $beg 1))
        (setq $max (cons $end $max))
        (setq $max (mapcar (lambda (x)
                             (if (< x $beg)
                                 nil
                               x))
                           $max))
        (setq $max (delq nil $max))
        (setq $dep (length $max))
        (if (<= $dep (length $sl))
            (loop repeat (- (1+ (length $sl)) $dep) do (pop $sl)))
        (setq $sl (cons $s $sl))
        (puthash (mapconcat 'identity (reverse $sl) " ") (list $beg $end $dep) $hash)
        ))
    $hash))

(defun helm-css-scss-selector-hash-to-list ()
  "Collected selector hash table to list"
  (let (($hash (helm-css-scss-selector-to-hash)))
    (loop for $k being hash-key in $hash using (hash-values $v)
          collect (cons $k $v))))

;;; core -----------------------------

(defun helm-css-scss--extract-selector ()
  (let (($multi "") $s $po1 $po2 $str $commentp)
    ;; Collect multiple selector across previous lines
    ;; (i.e. "div, \n p, \n span {...}")
    (save-excursion
      (while (string-match ",[ ]*$"
                           (setq $s (helm-css-scss-fetch-previous-line)))
        ;; Skip commented selector (i.e. " // .blue,")
        (save-excursion
          (move-beginning-of-line 1)
          (setq $commentp (helm-css-scss-comment-p (search-forward ","))))
        (unless $commentp
          (setq $multi (format "%s %s"
                               (helm-css-scss-trim-whitespace $s)
                               $multi)))))
    ;; Extract selector include one-line-nesting (i.e. "div { p {...} }")
    (skip-chars-backward "^{;\n")
    (setq $po1 (point))
    (skip-chars-forward "^{")
    (setq $po2 (point))
    (setq $str (buffer-substring-no-properties $po1 $po2))
    ;; i.e. "div { .box { p"  ->  " p"
    (setq $str (helm-css-scss-substruct-last-string $str "{\\|}"))
    (setq $str (helm-css-scss-trim-whitespace $str))
    ;; Return selector
    (if (equal $multi "")
        (format "%s" $str)
      (format "%s %s" (helm-css-scss-trim-whitespace $multi) $str))))

(defun* helm-css-scss-selector-next (&optional $bound)
  "Return and goto next selector."
  (unless (helm-css-scss-open-brace-forward $bound)
    (return-from helm-css-scss-selector-next nil))
  (helm-css-scss--extract-selector))

(defun* helm-css-scss-selector-previous (&optional $bound)
  "Return and goto previous selector."
  (unless (helm-css-scss-open-brace-backward $bound)
    (return-from helm-css-scss-selector-next nil))
  (helm-css-scss--extract-selector))

(defun* helm-css-scss-fetch-previous-line (&optional $prev $noexcursion)
  "Return previous nth ($prev) line strings.
If $noexcursion is not-nil cursor doesn't move."
  ;; In compressed Css without this return, it takes long time
  (if (eq 1 (line-number-at-pos))
      (return-from helm-css-scss-fetch-previous-line ""))
  (or $prev (setq $prev 1))
  (if $noexcursion (setq $noexcursion (point)))
  (move-beginning-of-line (- 1 $prev))
  (let (($po (point)) $res)
    (move-end-of-line 1)
    (setq $res (buffer-substring-no-properties $po (point)))
    (if $noexcursion (goto-char $noexcursion))
    $res))

(defun* helm-css-scss-open-brace-forward (&optional $bound)
  (interactive)
  "Move to next open brace, skip commented brace"
  (let ($ret)
    (setq $ret (re-search-forward "[^#]{" $bound t))
    (unless $ret (return-from helm-css-scss-open-brace-forward nil))
    (backward-char)
    (if (helm-css-scss-comment-p (point))
        (helm-css-scss-open-brace-forward $bound)
      $ret)))

(defun* helm-css-scss-open-brace-backward (&optional $bound)
  (interactive)
  "Move to previous open brace, skip commented brace"
  (let ($ret)
    (setq $ret (re-search-backward "[^#]{" $bound t))
    (unless $ret (return-from helm-css-scss-open-brace-backward nil))
    (forward-char)
    (if (helm-css-scss-comment-p (point))
        (helm-css-scss-open-brace-backward $bound)
      $ret)))

;;;###autoload
(defun* helm-css-scss-insert-close-comment ($depth)
  (interactive (list (read-number
                      "Nest Depth: "
                      helm-css-scss-insert-close-comment-depth)))
  ;; Delete original comment for update comments
  (helm-css-scss-delete-all-matches-in-buffer "[ \t]?\\/\\*__.*\\*\\/")
  (if (<= $depth 0) (return-from helm-css-scss-insert-close-comment nil))
  (let (($list (helm-css-scss-selector-to-hash))
        $r1 $r2 $ordered)
    (save-excursion
      ;; Extract selector and close-brace point
      (loop for $k being hash-key in $list using (hash-values $v)
            if (<= (caddr $v) $depth)
            do (let (($v2 (cadr $v)))
                 (setq $r1 (cons (cons $v2 $k) $r1))
                 (setq $r2 (cons $v2 $r2))))
      ;;(setq $hash (sort* $hash '> :key 'car))
      (setq $r2 (sort $r2 '<))
      (dolist ($x $r2)
        (setq $ordered (cons (assoc $x $r1) $ordered)))
      (loop for ($end . $sel) in $ordered
            do (progn
                 (goto-char $end)
                 (insert (format " /*__ %s */" $sel)))))))

(defun helm-css-scss-current-selector (&optional $list $point)
  (interactive)
  "Return selector that $point is in"
  (unless $list (setq $list (helm-css-scss-selector-hash-to-list)))
  (or $point (setq $point (point)))
  (let ($r1 $r2)
    (loop for ($sel $beg $end $dep) in $list
          if (and (< $point $end) (>= $point $beg))
          do (progn (setq $r1 (cons (cons $dep $sel) $r1))
                    (setq $r2 (cons $dep $r2))))
    ;; Get the deepest selector
    (assoc-default (car (sort $r2 '>)) $r1)))

;;;###autoload
(defun helm-css-scss-move-and-echo-next-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (helm-css-scss-selector-next))
               $s
             (goto-char (point-max))
             "No more exist the next target from here"))))

;;;###autoload
(defun helm-css-scss-move-and-echo-previous-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (helm-css-scss-selector-previous))
               $s
             (goto-char (point-min))
             "No more exist the previous target from here"))))

;;; helm -----------------------------

(defun helm-c-source-helm-css-scss ($list)
  `((name . "CSS/Scss Selectors")
    (candidates . ,$list)
    (action ("Goto open brace"  . (lambda ($po)
                                    (goto-char (car $po))
                                    (recenter)))
            ("Goto close brace" . (lambda ($po)
                                    (goto-char (nth 1 $po))
                                    (recenter))))))

(defun helm-css-scss-back-to-last-point ()
  (interactive)
  (when helm-css-scss-last-point
    (let (($po (point)))
      (goto-char helm-css-scss-last-point)
      (setq helm-css-scss-last-point $po))))

(defvar helm-css-scss-synchronizing-window nil
  "Store window identity for synchronizing")
(defun helm-css-scss-synchronizing-position ()
  (with-helm-window
    (let* (($key (helm-css-scss-trim-whitespace
                  (buffer-substring-no-properties
                   (point-at-bol) (point-at-eol))))
           ($cand (assoc-default 'candidates (helm-get-current-source)))
           ($prop (assoc-default $key $cand)))
      ;; Synchronizing selecter list to buffer
      (with-selected-window helm-css-scss-synchronizing-window
        (if helm-css-scss-first-time
            (progn
              (goto-char (car $prop))
              (with-current-buffer helm-css-scss-target-buffer
                (delete-overlay helm-css-scss-overlay)
                (helm-css-scss-target-overlay))
              (recenter))
          (helm-css-scss-target-overlay)
          (recenter)
          (setq helm-css-scss-first-time t))))))

;; Store function to restore later
(defvar helm-css-scss-tmp helm-display-function
  "To restore helm window display function")

(defun helm-css-scss-clear-cache ()
  "Clear cache when buffer saved"
  (if (boundp 'helm-css-scss-cache) (setq helm-css-scss-cache nil)))
(add-hook 'after-save-hook 'helm-css-scss-clear-cache)

;;;###autoload
(defun helm-css-scss ()
  (interactive)
  (setq helm-css-scss-synchronizing-window (selected-window))
  (if (boundp 'helm-css-scss-last-point)
      (setq helm-css-scss-last-point (point))
    (set (make-local-variable 'helm-css-scss-last-point) nil)
    (setq helm-css-scss-last-point (point)))
  (setq helm-css-scss-target-buffer (current-buffer))
  (setq helm-css-scss-overlay (make-overlay (point-at-bol) (point-at-eol)))
  ;; Enable scrolling margin
  (when helm-display-source-at-screen-top
    (setq helm-display-source-at-screen-top nil))
  ;; Cache
  (cond ((not (boundp 'helm-css-scss-cache))
         (set (make-local-variable 'helm-css-scss-cache)
              (helm-css-scss-selector-hash-to-list)))
        ((not helm-css-scss-cache)
         (setq helm-css-scss-cache (helm-css-scss-selector-hash-to-list)))
        ((buffer-modified-p)
         (setq helm-css-scss-cache (helm-css-scss-selector-hash-to-list))))
  (unwind-protect
      (let (($list helm-css-scss-cache))
        ;; Modify window split function temporary
        (setq helm-display-function helm-css-scss-split-window-function)
        ;; For synchronizing position
        (add-hook 'helm-move-selection-after-hook
                  'helm-css-scss-synchronizing-position)
        ;; Execute helm
        (helm :sources (helm-c-source-helm-css-scss $list)
              :buffer "*Helm Css SCSS*"
              :preselect (helm-css-scss-current-selector $list)
              :candidate-number-limit 999)
        ;; Restore helm's hook and window function
        (progn
          (remove-hook 'helm-move-selection-after-hook
                       'helm-css-scss-synchronizing-position)
          (setq helm-display-function helm-css-scss-tmp)
          (setq helm-css-scss-first-time nil)
          (delete-overlay helm-css-scss-overlay)
          (setq helm-display-source-at-screen-top
                helm-css-scss-at-screen-top)))))

(provide 'helm-css-scss)
;;; helm-css-scss.el ends here
