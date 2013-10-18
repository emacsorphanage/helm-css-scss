;;; -*- coding: utf-8; lexical-binding: t -*-
;;
;; Copyright (C) 2013 by Shingo Fukuyama
;;
;; anything-css-scss-mode.el --- SCSS Selector with anything interface
;;
;; Version: 1.0
;; Author: Shingo Fukuyama <xxxx@gmail.com> - http://fukuyama.co
;; Repository: http://github.com/fxbois/web-mode
;; Created: Oct 18 2013
;; Keywords: scss css mode anything
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
;; (anything-css-scss)
;;   Easily jumping between SCSS selectors powerd by anything.el
;;
;; (anything-css-scss-insert-close-comment &optional $depth)
;;   Insert inline comment like " //__ comment" at the next of
;;   a close brace "}". If it's aleardy there, update it.
;;   You can also specify a nest $depth of selector.

(eval-when-compile
  (require 'cl))
(require 'anything)

;;; config -----------------------------
(defvar anything-css-scss-insert-close-comment-depth nil)

;;; common parts -----------------------------
(defun anything-css-scss-nthcar ($i $l)
  "Return n($i) of values from the head of a list($l)"
  (loop for $k from 1 to $i
        collect (nth (- $k 1) $l) into $res
        finally return (delq nil $res)))

(defun anything-css-scss-substruct-last-string ($text $key)
  "Return the tail of $text without $key strings"
  (while (string-match $key $text)
      (setq $text (substring $text (1+ (string-match $key $text)))))
  $text)

(defun anything-css-scss-trim-whitespace ($s)
  "Return string without whitespace at the both beginning and end"
  (if (string-match "\\`\\(?:\\s-+\\)?\\(.+?\\)\\(?:\\s-+\\)?\\'" $s)
      (match-string 1 $s)
    $s))

(defun anything-css-scss-delete-all-matches-in-buffer ($regexp)
  (save-excursion
    (goto-char (point-min))
    (while
        (re-search-forward $regexp nil t)
      (delete-region (match-beginning 0) (match-end 0))
      )))

;;; comment-p -----------------------------
(defun* anything-css-scss-asterisk-comment-p (&optional $point)
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

(defun* anything-css-scss-slash-comment-p (&optional $point)
  (or $point (setq $point (point)))
  (save-excursion
    (goto-char $point)
    (let ($beg $end $slash)
      (save-excursion
        (setq $beg (re-search-backward "^" nil t))
        (setq $end (re-search-forward "$" nil t)))
      (unless (setq $slash (re-search-backward "\\/\\/" $beg t))
        (return-from anything-css-scss-slash-comment-p nil))
      (if (and (<= $point $end) (> $point $slash))
          t
        nil)
      )))

(defun anything-css-scss-comment-p (&optional $point)
  (or $point (setq $point (point)))
  (or (anything-css-scss-slash-comment-p $point)
      (anything-css-scss-asterisk-comment-p $point)))

;;; core -----------------------------
(defun anything-css-scss--extract-selector ()
  (let (($multi "") $s $po1 $po2 $str $commentp)
    ;; Collect multiple selector across previous lines
    ;; (i.e. "div, \n p, \n span {...}")
    (save-excursion
      (while (string-match ",[ ]*$" (setq $s (anything-css-scss-fetch-previous-line)))
        ;; Skip commented selector (i.e. " // .blue,")
        (save-excursion
          (move-beginning-of-line 1)
          (setq $commentp (anything-css-scss-comment-p (search-forward ","))))
        (unless $commentp
          (setq $multi (format "%s %s" (anything-css-scss-trim-whitespace $s) $multi)))
        ))
    ;; Extract selector include one-line-nesting (i.e. "div { p {...} }")
    (skip-chars-backward "^{;\n")
    (setq $po1 (point))
    (skip-chars-forward "^{")
    (setq $po2 (point))
    (setq $str (buffer-substring-no-properties $po1 $po2))
    ;; i.e. "div { .box { p"  ->  " p"
    (setq $str (anything-css-scss-substruct-last-string $str "{\\|}"))
    (setq $str (anything-css-scss-trim-whitespace $str))
    ;; Return selector
    (if (equal $multi "")
        (format "%s" $str)
      (format "%s %s" (anything-css-scss-trim-whitespace $multi) $str))
    ))

(defun* anything-css-scss-selector-next (&optional $bound)
  "Return and goto next selector."
  (or $bound (setq $bound nil))
  (unless (anything-css-scss-open-brace-forward $bound)
    (return-from anything-css-scss-selector-next nil))
  (anything-css-scss--extract-selector))

(defun* anything-css-scss-selector-previous (&optional $bound)
  "Return and goto previous selector."
  (or $bound (setq $bound nil))
  (unless (anything-css-scss-open-brace-backward $bound)
    (return-from anything-css-scss-selector-next nil))
  (anything-css-scss--extract-selector))

(defun anything-css-scss-fetch-previous-line (&optional $prev $noexcursion)
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

(defun* anything-css-scss-open-brace-forward (&optional $bound)
  (interactive)
  "Move to next open brace, skip commented brace"
  (let ($ret)
    (setq $ret (re-search-forward "[^#]{" $bound t))
    (unless $ret (return-from anything-css-scss-open-brace-forward nil))
    (backward-char)
    (if (anything-css-scss-comment-p (point))
        (anything-css-scss-open-brace-forward $bound)
      $ret)
    ))

(defun* anything-css-scss-open-brace-backward (&optional $bound)
  (interactive)
  "Move to previous open brace, skip commented brace"
  (let ($ret)
    (setq $ret (re-search-backward "[^#]{" $bound t))
    (unless $ret (return-from anything-css-scss-open-brace-backward nil))
    (forward-char)
    (if (anything-css-scss-comment-p (point))
        (anything-css-scss-open-brace-backward $bound)
      $ret)
    ))

(defun anything-css-scss-selector-to-hash ()
  (interactive)
  (let ($s $beg ($end nil)
        $h $n ($max nil) ($sl nil))
    (setq $h (make-hash-table :test 'equal))
    (save-excursion
      (goto-char (point-min))
      (while (setq $s (anything-css-scss-selector-next))
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

(defun anything-css-scss-selector-hash-to-list ()
  (let ($hash)
    (setq $hash (anything-css-scss-selector-to-hash))
    (loop for $k being hash-key in $hash using (hash-values $v)
          collect (cons $k $v))))

(defun* anything-css-scss-insert-close-comment (&optional $depth)
  (interactive "P")
  (setq $depth (or $depth
                   anything-css-scss-insert-close-comment-depth
                   21))
  ;; Delete original comment for update comments
  (anything-css-scss-delete-all-matches-in-buffer "[ \t]?\\/\\/__.*$")
  (if (<= $depth 0) (return-from anything-css-scss-insert-close-comment nil))
  (let (($list (anything-css-scss-selector-hash-to-list))
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

;;; option -----------------------------
(defun anything-css-scss-move-and-echo-next-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (anything-css-scss-selector-next))
               $s
             (goto-char (point-max))
             "No more exist the next target from here"))))

(defun anything-css-scss-move-and-echo-previous-selector ()
  (interactive)
  (let ($s)
    (message (if (setq $s (anything-css-scss-selector-previous))
               $s
             (goto-char (point-min))
             "No more exist the previous target from here"))))

;;; anything -----------------------------
(defun anything-c-source-anything-css-scss ($list)
  `((name . "SCSS Selectors")
    (candidates . ,$list)
    ;(candidates-in-buffer)
    (action ("Goto open brace"  . (lambda (po) (goto-char (car po))))
            ("Goto close brace" . (lambda (po) (goto-char (nth 1 po)))))
    ))

(defun anything-css-scss ()
  (interactive)
  (let (($list
         (loop for ($sel $beg $end $dep) in (anything-css-scss-selector-hash-to-list)
               collect
               (list (mapconcat 'identity $sel " ") $beg $end) into $res
               finally return $res
               )))
    (anything :sources (anything-c-source-anything-css-scss $list)
          :buffer "*Anything Css SCSS*"
          :candidate-number-limit 999)
    ))

(provide 'anything-css-scss)
