;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (C) 2013 by Shingo Fukuyama
;;
;; helm-css-scss-mode.el --- SCSS Selector with helm interface
;;
;; Version: 1.0
;; Author: Shingo Fukuyama - http://fukuyama.co
;; Repository: https://github.com/ShingoFukuyama/helm-css-scss
;; Created: Oct 18 2013
;; Keywords: scss css mode helm
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;;; Commentary:
;;
;; This program has two main functions
;;
;; (helm-css-scss)
;;   Easily jumping between SCSS selectors powerd by helm.el
;;
;; (helm-css-scss-insert-close-comment &optional $depth)
;;   Insert inline comment like " //__ comment" at the next of
;;   a close brace "}". If it's aleardy there, update it.
;;   You can also specify a nest $depth of selector.
;;
;; TODO:
;;   Jump back latest position
;;
;;

(eval-when-compile (require 'cl))

(require 'helm)

;;; config -----------------------------
(defvar helm-css-scss-insert-close-comment-depth nil)

(defvar helm-css-scss-split-window-vertically nil)

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
    (while
        (re-search-forward $regexp nil t)
      (delete-region (match-beginning 0) (match-end 0))
      )))

;;; scan selector -----------------------------
(defun* helm-css-scss-asterisk-comment-p (&optional $point)
  "Check whether $point within /* */ or not."
  (or $point (setq $point (point)))
  (save-excursion
    (goto-char $point)
    (let ($ret)
      (save-excursion
        (when (re-search-backward "\\*\\/\\|\\/\\*" nil t)
         (if (equal (match-string 0) "/*")
              (setq $ret t))
          ))
      $ret)))

(defun* helm-css-scss-slash-comment-p (&optional $point)
  (or $point (setq $point (point)))
  (save-excursion
    (goto-char $point)
    (let ($beg $end $slash)
      (save-excursion
        (setq $beg (re-search-backward "^" nil t))
        (setq $end (re-search-forward "$" nil t)))
      (unless (setq $slash (re-search-backward "\\/\\/" $beg t))
        (return-from helm-css-scss-slash-comment-p nil))
      (if (and (<= $point $end) (> $point $slash))
          t
        nil)
      )))

(defun helm-css-scss-comment-p (&optional $point)
  (or $point (setq $point (point)))
  (or (helm-css-scss-slash-comment-p $point)
      (helm-css-scss-asterisk-comment-p $point)))

(defun helm-css-scss-selector-to-hash ()
  (interactive)
  (let ($s $beg ($end nil)
        $h $n ($max nil) ($sl nil))
    (setq $h (make-hash-table :test 'equal))
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
        (setq $n (length $max))
        (if (<= $n (length $sl))
            (loop repeat (- (1+ (length $sl)) $n) do (pop $sl)))
        (setq $sl (cons $s $sl))
        (puthash (reverse $sl) (list $beg $end $n) $h)
        ))
    $h))

(defun helm-css-scss-selector-hash-to-list ()
  (let ($hash)
    (setq $hash (helm-css-scss-selector-to-hash))
    (loop for $k being hash-key in $hash using (hash-values $v)
          collect (cons $k $v))))

;;; core -----------------------------
(defun helm-css-scss--extract-selector ()
  (let (($multi "") $s $po1 $po2 $str $commentp)
    ;; Collect multiple selector across previous lines
    ;; (i.e. "div, \n p, \n span {...}")
    (save-excursion
      (while (string-match ",[ ]*$" (setq $s (helm-css-scss-fetch-previous-line)))
        ;; Skip commented selector (i.e. " // .blue,")
        (save-excursion
          (move-beginning-of-line 1)
          (setq $commentp (helm-css-scss-comment-p (search-forward ","))))
        (unless $commentp
          (setq $multi (format "%s %s" (helm-css-scss-trim-whitespace $s) $multi)))
        ))
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
      (format "%s %s" (helm-css-scss-trim-whitespace $multi) $str))
    ))

(defun* helm-css-scss-selector-next (&optional $bound)
  "Return and goto next selector."
  (or $bound (setq $bound nil))
  (unless (helm-css-scss-open-brace-forward $bound)
    (return-from helm-css-scss-selector-next nil))
  (helm-css-scss--extract-selector))

(defun* helm-css-scss-selector-previous (&optional $bound)
  "Return and goto previous selector."
  (or $bound (setq $bound nil))
  (unless (helm-css-scss-open-brace-backward $bound)
    (return-from helm-css-scss-selector-next nil))
  (helm-css-scss--extract-selector))

(defun helm-css-scss-fetch-previous-line (&optional $prev $noexcursion)
  (interactive)
  "Return previous nth ($prev) line strings.
If $noexcursion is not-nil cursor doesn't move."
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
      $ret)
    ))

(defun* helm-css-scss-open-brace-backward (&optional $bound)
  (interactive)
  "Move to previous open brace, skip commented brace"
  (let ($ret)
    (setq $ret (re-search-backward "[^#]{" $bound t))
    (unless $ret (return-from helm-css-scss-open-brace-backward nil))
    (forward-char)
    (if (helm-css-scss-comment-p (point))
        (helm-css-scss-open-brace-backward $bound)
      $ret)
    ))

(defun* helm-css-scss-insert-close-comment (&optional $depth)
  (interactive "P")
  (setq $depth (or $depth
                   helm-css-scss-insert-close-comment-depth
                   21))
  ;; Delete original comment for update comments
  (helm-css-scss-delete-all-matches-in-buffer "[ \t]?\\/\\/__.*$")
  (if (<= $depth 0) (return-from helm-css-scss-insert-close-comment nil))
  (let (($list (helm-css-scss-selector-hash-to-list))
        $slist)
    (save-excursion
      ;; Extract selector and close-brace point
      (setq $slist (loop for ($sel $beg $end $dep) in $list
                         if (<= $dep $depth)
                         collect (list $end (mapconcat 'identity $sel " ")) into $res
                         finally return $res))
      (setq $slist (sort* $slist '> :key 'car))
      (loop for ($end $sel) in $slist
            do (progn
                 (goto-char $end)
                 (insert (format " //__ %s" $sel))))
      )))

(defun helm-css-scss-current-selector (&optional $list $point)
  "Return selector that $point is in"
  (unless $list (setq $list (helm-css-scss-selector-hash-to-list)))
  (or $point (setq $point (point)))
  (let ($s)
    (setq $s (loop for ($sel $beg $end $dep) in $list
                   if (and (< $point $end) (>= $point $beg))
                   collect (list $dep $sel) into $res
                   finally return $res))
    ;; Get the deepest selector
    (setq $s (cdr (car (sort* $s '> :key 'car))))
    ;; (mapconcat 'identity $s " ")
    (mapconcat 'identity (car $s) " ")
    ))

(defun helm-css-scss-move-and-echo-previous-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (helm-css-scss-selector-previous))
               $s
             (goto-char (point-min))
             "No more exist the previous target from here"))))

;;; option -----------------------------
(defun helm-css-scss-move-and-echo-next-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (helm-css-scss-selector-next))
               $s
             (goto-char (point-max))
             "No more exist the next target from here"))))

(defun helm-css-scss-move-and-echo-previous-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (helm-css-scss-selector-previous))
               $s
             (goto-char (point-min))
             "No more exist the previous target from here"))))

;;; helm -----------------------------

(defun helm-c-source-helm-css-scss ($list)
  `((name . "SCSS Selectors")
    (candidates . ,$list)
    (action ("Goto open brace"  . (lambda ($po) (goto-char (car $po))))
            ("Goto close brace" . (lambda ($po) (goto-char (nth 1 $po)))))
    ))

(defvar helm-css-scss-last-point nil
  "For jump back once")

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
    (let* (($key (helm-css-scss-trim-whitespace (thing-at-point 'line)))
           ($cand (assoc-default 'candidates (helm-get-current-source)))
           ($prop (assoc-default $key $cand)))
      ;; Synchronizing selecter list to buffer
      (with-selected-window helm-css-scss-synchronizing-window
        (goto-char (car $prop)))
      )))

;; Store function to restore later
(setq helm-css-scss-tmp helm-display-function)


(defun helm-css-scss ()
  (interactive)
  (setq helm-css-scss-synchronizing-window (selected-window))
  (setq helm-css-scss-last-point (point))
  (unwind-protect
      (let (($list
             (loop for ($sel $beg $end $dep)
                   in (helm-css-scss-selector-hash-to-list)
                   collect (list (mapconcat 'identity $sel " ") $beg $end $dep) into $res
                   finally return $res
                   )))
        ;; Modify window split function temporary
        (setq helm-display-function
              (lambda (buf)
                (if helm-css-scss-split-window-vertically
                    (split-window-vertically)
                  (split-window-horizontally))
                (other-window 1)
                (switch-to-buffer buf)))
        ;; For synchronizing position
        (add-hook 'helm-move-selection-after-hook
                  'helm-css-scss-synchronizing-position)
        ;; Execute helm
        (helm :sources (helm-c-source-helm-css-scss $list)
              :buffer "*Helm Css SCSS*"
              :preselect (helm-css-scss-current-selector)
              :candidate-number-limit 999)
        ;; Restore helm's hook and window function
        (progn
          (remove-hook 'helm-move-selection-after-hook
                       'helm-css-scss-synchronizing-position)
          (setq helm-display-function helm-css-scss-tmp))
        )))

(provide 'helm-css-scss)
