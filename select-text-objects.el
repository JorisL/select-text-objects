;;; select-text-objects.el --- Functions for selecting specific sections of texts near the point -*- lexical-binding: t -*-

;; Copyright (C) 2022--present Joris Laurenssen
;; MIT License

;; Author: Joris Laurenssen <JorisL@users.noreply.github.com>
;; URL: https://github.com/JorisL/select-text-objects
;; Version: 0.1.2
;; Package-Requires: ((emacs "27.2"))
;; Keywords: convenience

;; This file is not part of GNU Emacs

;;; Commentary:

;; select-text-objects provides various functions to quickly select pieces of text near the point/cursor.
;; Examples include selecting:
;; - everything within the surrounding brackets
;; - the complete current line
;; - everythin within the surrounding parentheses, including the parentheses themselve
;; - every continuous line with at least the same indentation level as the current line
;; This is similar to VIM's text objects (e.g. i] to select everything within the surrounding brackets and similar commands).
;; These functions are intended to reduce the motion commands needed for programming,
;; where many actions can be performed by selecting a piece of text and performing an edit action (e.g. replacing, copying, deleting, etc.)

;; These functions all follow the convention where the point (cursor) is placed ad the beginning of the selection, and the mark at the end.

;; Several functions also have been added to add a few additional functionalities for use after making a selection.
;; These include:
;; - placing the cursor at the end of the selection and disabling the selection (shorthand for "C-x C-x C-<space>")
;; - adding a line above the selection, disabling the selection, and placing the cursor at that line
;; - adding a line below the selection, disabling the selection, and placing the cursor at that line
;; These last functions can, for example, be chained together with the text selection commands to perform actions such as:
;; - adding a line at the end of the current python indentation level (e.g. select current indentation level and create a line below the current selection)

;; Recommended is to place these functions behind an unused or unnessecary prefix keybinding.
;; Personally I place them behind the "C-v" prefix, and can then use "C-v i" to select the current indentation level or "C-v (" to select within surrounding parentheses.

;;; Code:

;; * Requirements
(require 'cl-lib)
(require 's)


;; Helper functions
(defun sto--deselect-leading-trailing-whitespace ()
  "Deselect leading and trailing whitespace from the current selection."
  (when (> (point) (mark))
    (exchange-point-and-mark))
  (while (and (< (point) (mark))
              (or (char-equal (char-after) 9)
                  (char-equal (char-after) 10)
                  (char-equal (char-after) 32)))
    (right-char))
  (exchange-point-and-mark)
  (while (and (> (point) (mark))
              (or (char-equal (char-before) 9)
                  (char-equal (char-before) 10)
                  (char-equal (char-before) 32)))
    (left-char))
  (exchange-point-and-mark))



;; Selecting words, lines, paragraphs, etc.
(defun sto/select-word ()
  "Put point at beginning of word and mark at end of word."
  (interactive)
  (forward-word)
  (set-mark-command nil)
  (backward-word))


(defun sto/select-within-whitespace ()
  "Put point at first non-whitespace character and mark at last non-whitespace character relative to current point."
  (interactive)
  (search-backward-regexp (rx (or whitespace "\n")))
  (right-char)
  (set-mark-command nil)
  (search-forward-regexp (rx (or whitespace "\n")))
  (left-char)
  (exchange-point-and-mark)
  )


(defun sto/select-line ()
  "Put point at first non-whitespace character of line and mark at end of line."
  (interactive)
  (progn
    (end-of-line)
    (set-mark-command nil)
    (back-to-indentation)))


(defun sto/select-inc-newline ()
  "Put point at first character of the line and mark at first character of the next line."
  (interactive)
  (progn
    (beginning-of-line)
    (set-mark-command nil)
    (next-line)
    (beginning-of-line)
    (exchange-point-and-mark)))


(defun sto/select-sentence ()
  "Put point at beginning of sentence and mark at end of sentence."
  (interactive)
  (progn
    (forward-sentence)
    (set-mark-command nil)
    (backward-sentence))
  (sto--deselect-leading-trailing-whitespace))


(defun sto/select-paragraph ()
  "Put point at beginning of paragraph and mark at end of paragraph."
  (interactive)
  (mark-paragraph)
  (sto--deselect-leading-trailing-whitespace))


(defun sto/select-buffer ()
  "Put point at beginning of buffer and mark at end of buffer."
  (interactive)
  (mark-whole-buffer))


;; Selecting programming elements (function, argument, indentation, ...)

(defun sto/select-defun ()
  "Put point at beginning of function and mark at end of function."
  (interactive)
  (mark-defun)
  (sto--deselect-leading-trailing-whitespace))


(defun sto--get-current-line-indentation ()
  (save-excursion
    (beginning-of-line)
    (set-mark-command nil)
    (back-to-indentation)
    (let ((indentation-text (buffer-substring-no-properties (mark) (point))))
      (deactivate-mark)
      indentation-text)))

(defun sto--line-starts-with? (prefix)
  (s-starts-with? prefix (thing-at-point 'line)))

(defun sto/select-indent ()
  "Select every contignuous line at the current indentation level or deeper."
  (interactive)
  (let ((indent (sto--get-current-line-indentation)))
    (if (string-equal indent "")
        (mark-whole-buffer)
      (progn
        (while (sto--line-starts-with? indent)
          (next-line))
        (previous-line)
        (end-of-line)
        (set-mark-command nil)
        (while (sto--line-starts-with? indent)
          (previous-line))
        (next-line)
        (beginning-of-line)))))


;; TODO: improve robustness, for example checking for balanced parentheses pairs, detect lisp or c-like...
(defun sto/select-argument ()
  "Put point at beginning of function argument and mark at end of function argument."
  (interactive)
  (search-backward-regexp (rx (or "," "(" "[" "{")))
  (right-char)
  (set-mark-command nil)
  (search-forward-regexp (rx (or "," ")" "]" "}")))
  (left-char)
  (exchange-point-and-mark)
  (sto--deselect-leading-trailing-whitespace)
  )


;; Selecting within pairs (brackets, braces, parens, angle-brackets)
;; TODO: check for when point is not in between a character-pair

(defun sto--goto-opening-charpair (opening-char closing-char)
  (unless (char-equal (char-after) opening-char)
    (let ((counter 0))
      (while (or (not (char-equal (char-after) opening-char))
                 (>= counter 0))
        (left-char)
        (cond ((char-equal (char-after) closing-char) (cl-incf counter))
              ((char-equal (char-after) opening-char) (cl-decf counter)))))))


(defun sto/select-outer-paren ()
  "Select within, and including, the surrounding parentheses."
  (interactive)
  (sto--goto-opening-charpair (string-to-char "(") (string-to-char ")"))
  (mark-sexp))

(defun sto/select-outer-bracket ()
  "Select within, and including, the surrounding brackets."
  (interactive)
  (sto--goto-opening-charpair (string-to-char "[") (string-to-char "]"))
  (mark-sexp))

(defun sto/select-outer-brace ()
  "Select within, and including, the surrounding braces"
  (interactive)
  (sto--goto-opening-charpair (string-to-char "{") (string-to-char "}"))
  (mark-sexp))

(defun sto/select-outer-angle-bracket ()
  "Select within, and including, the surrounding angle brackets."
  (interactive)
  (sto--goto-opening-charpair (string-to-char "<") (string-to-char ">"))
  (mark-sexp))

(defun sto/select-inner-paren ()
  "Select within the surrounding parentheses."
  (interactive)
  (sto/select-outer-paren)
  (right-char)
  (exchange-point-and-mark)
  (left-char)
  (exchange-point-and-mark))

(defun sto/select-inner-bracket ()
  "Select withing the surrounding brackets."
  (interactive)
  (sto/select-outer-bracket)
  (right-char)
  (exchange-point-and-mark)
  (left-char)
  (exchange-point-and-mark))

(defun sto/select-inner-brace ()
  "Select within the surrounding braces."
  (interactive)
  (sto/select-outer-brace)
  (right-char)
  (exchange-point-and-mark)
  (left-char)
  (exchange-point-and-mark))

(defun sto/select-inner-angle-bracket ()
  "Select within the surrounding angle brackets."
  (interactive)
  (sto/select-outer-angle-bracket)
  (right-char)
  (exchange-point-and-mark)
  (left-char)
  (exchange-point-and-mark))


;;; Selecting strings
(defun sto--current-string-delimiter ()
  (nth 3 (syntax-ppss)))

(defun sto--move-to-front-of-string ()
  (goto-char (nth 8 (syntax-ppss))))

(defun sto/select-outer-string ()
  "Select the current string, including the string delimiters."
  (interactive)
  (cond ((sto--current-string-delimiter) (progn (sto--move-to-front-of-string) (mark-sexp)))
        ((char-equal (char-after) 34) (mark-sexp)) ; double quote
        ((char-equal (char-after) 39) (mark-sexp)) ; single quote
        ))

(defun sto/select-inner-string ()
  "Select the current string, excluding the string delimiters."
  (interactive)
  (sto/mark-outer-string)
  (right-char)
  (exchange-point-and-mark)
  (left-char)
  (exchange-point-and-mark))


;; Actions to perform after selecting a text object
(defun sto--region-active-p ()
  "Return t if Transient Mark mode is enabled and the mark is active."
  (and transient-mark-mode mark-active))


(defun sto/move-cursor-selection-front ()
  "Move the cursor to the front of the selection (if active) and disable the selection"
  (interactive)
  (when mark-active
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (deactivate-mark)))

(defun sto/move-cursor-selection-back ()
  "Move the cursor to the back of the selection (if active) and disable the selection"
  (interactive)
  (when mark-active
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (deactivate-mark)))


;; TODO: change to use specialized commands of current major mode (e.g. org-newline)?
(defun sto/add-line-above-selection-or-point ()
  "Add a line on the row above the point (similar to 'O' in vi) or above the selection when active"
  (interactive)
  (sto/move-cursor-selection-front)
  (back-to-indentation)
  (newline 1 t)
  (previous-line)
  (move-end-of-line nil))

(defun sto/add-line-below-selection-or-point ()
  "Add a line on the row above the point (similar to 'o' in vi) or above the selection when active"
  (interactive)
  (sto/move-cursor-selection-back)
  (move-end-of-line nil)
  (newline 1 t))


(provide 'select-text-objects)

;;; select-text-objects.el ends here
