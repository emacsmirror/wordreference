;;; wordreference.el --- Interface for wordreference.com -*- lexical-binding:t -*-
;;
;; Copyright (C) 2022 Marty Hiatt <martianhiatus AT riseup.net>
;;
;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Created: 21 Oct 2020
;;
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, translate
;; URL: https://codeberg.org/martianh/wordreference
;; Version: 0.1
;; Prefix: wordreference
;; Separator: -

;;; Commentary:
;;
;; A simple interface for wordreference.com.

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; nb: wr requires a user agent to return a request

;;; Code:
(require 'dom)
(require 'shr)
(require 'browse-url)
(require 'text-property-search)

(defvar wordreference-source-lang "fr"
  "Default source language.")

(defvar wordreference-target-lang "en"
  "Default target language.")


;; REQUESTING AND PARSING

(defun wordreference--retrieve-parse-html (word)
  "Query wordreference.com for WORD, and parse the HTML response."
  (let* ((url (format "https://www.wordreference.com/%s%s/%s"
                      wordreference-source-lang
                      wordreference-target-lang
                      word))
	 (html-buffer (url-retrieve-synchronously url)))
    (with-current-buffer html-buffer
      (goto-char (point-min))
      (libxml-parse-html-region
       (search-forward "\n\n") (point-max)))));))

(defun wordreference--get-tables (dom)
  "Get tables from parsed HTML response DOM."
  (dom-by-tag dom 'table))

(defun wordreference--get-word-tables (tables)
  (let ((word-tables))
    (mapcar (lambda (x)
              (when (equal (dom-attr x 'class) "WRD")
                (push x word-tables)))
            tables)))

(defun wordreference--get-trs (word-table)
  (dom-children word-table))

(defun wordreference--build-tds-text-list (tr)
  "Return a list of results of both source and target langs from TR."
  (let ((tds (dom-by-tag tr 'td)))
    (setq wr-single-td (caddr tds))
    (mapcar (lambda (x)
              (wr-build-single-td-list x))
            tds)))

(defun wr-build-single-td-list (td)
  ""
  (cond ((or (dom-by-class td "ToWrd")
             (dom-by-class td "FrWrd"))
         (let* ((to-or-from (if (dom-by-class td "ToWrd")
                                :to
                              :from))
                (em (dom-by-class td "tooltip POS2"))
                (pos (dom-text em))
                (tooltip (dom-by-tag em 'span))
                (tooltip-text (dom-texts tooltip))
                (term-text
                 (if (dom-by-class td "FrWrd")
                     (dom-text (dom-by-tag td 'strong))
                   (dom-text td))))
           `(,to-or-from ,term-text :pos ,pos :tooltip ,tooltip-text)))
        ((or (dom-by-class td "ToEx")
             (dom-by-class td "FrEx"))
         `(:example ,(dom-texts td)))
        (t
         `(:other ,(dom-texts td)))))

(defun wr-collect-trs-results-list (trs)
  ""
  (mapcar (lambda (tr)
            (wordreference--build-tds-text-list tr))
          trs))

(defun wordreference-search (word)
  ""
  (interactive "MWordreference search: ")
  (with-current-buffer (get-buffer-create "*wordreference*")
    (let* ((inhibit-read-only t)
           (html-parsed (wordreference--retrieve-parse-html word))
           (tables (wordreference--get-tables html-parsed))
           (word-tables (wordreference--get-word-tables tables))
           (pr-table (cadr word-tables))
           (pr-trs (wordreference--get-trs pr-table))
           (pr-trs-results-list (wr-collect-trs-results-list pr-trs))
           (sup-trs-table (cadddr word-tables))
           (sup-trs (wordreference--get-trs sup-trs-table))
           (sup-trs-results-list (wr-collect-trs-results-list sup-trs)))
      ;; (setq wr-word-tables word-tables)
      ;; (setq wr-full-html html-parsed)
      ;; (setq wr-full-tables word-tables)
      ;; (setq wr-single-tr (caddr (wordreference--get-trs pr-table)))
      ;; (setq wr-full-pr-trs-list pr-trs-results-list)
      ;; (setq wr-full-sup-trs-list sup-trs-results-list)
      (erase-buffer)
      ;; (special-mode)
      (wordreference-mode)
      (switch-to-buffer-other-window (current-buffer))
      (wr-print-trs-results pr-trs-results-list)
      (insert "\n")
      (when sup-trs-table
        (wr-print-trs-results sup-trs-results-list))
      (setq-local header-line-format
                  (propertize
                   (format "Wordreference results for \"%s\" from %s to %s:"
                           word
                           wordreference-source-lang
                           wordreference-target-lang)
                   'face font-lock-comment-face))
      (wr--make-buttons)
      (wr-prop-query-in-results word)
      (goto-char (point-min)))))


;; PRINTING:

(defun wr-prop-query-in-results (word)
  "Propertize query WORD in results buffer."
  (let ((word-spl (split-string word)))
    (save-excursion
      (goto-char (point-min))
      (mapc (lambda (x)
              (while (search-forward-regexp (concat "\\b" x "\\b")
                                            nil 'noerror)
                (add-text-properties (- (point) (length x)) (point)
                                     '(face '((t :inherit success :weight bold)))))
              (goto-char (point-min)))
            word-spl))))

(defun wr-print-heading (heading)
  ""
  (insert (propertize heading
                      'heading t
                      'face '((t :inherit font-lock-function-name-face
                                 :weight bold)))))

(defun wr-print-trs-results (trs)
  ""
  (let* ((table-name (plist-get (caar trs) :other))
         (langs (cadr trs))
         (source (caar langs))
         (target (caaddr langs))
         (definitions (cddr trs)))
    (when table-name
      (wr-print-heading table-name))
    (if definitions
        (wr-print-definitions definitions)
      (insert "looks like wr returned nada."))))

(defun wr-print-definitions (defs)
  ""
  (mapc (lambda (def)
            (wr-print-single-definition def))
          defs)
  (insert "\n"))

(defun wr--cull-double-spaces (result)
  "Remove any double spaces from RESULT."
  (save-match-data
    (while (string-match "[[:blank:]]\\{2\\}"
                         result)
      (setq result (replace-match
                    " "
                    t nil result))))
  result)

(defun wr--cull-single-spaces-in-brackets (result)
  "Remove any spaces inside brackets from RESULT."
  (save-match-data
    (while (string-match
            ;; ( + SPC + any number of chars + SPC + ):
            "([[:blank:]]\\(.*\\)[[:blank:]])"
            result)
      (setq result (replace-match
                    "(\\1)"
                    t nil result))))
  result)

(defun wr--cull-space-between-brackets (result)
  "Remove any spaces between closing brackets from RESULT."
  (save-match-data
    (while (string-match
            ;; ] + SPC + ) :
            "][[:blank:]])"
            result)
      (setq result (replace-match
                    "])"
                    t nil result))))
  result)

(defun wr-print-single-definition (def)
  "Print a single definition DEF in the buffer.
\nFor now a definition can be a set of source term, context term,
and target term, or an example sentence."
  (let* ((source (car def))
         (source-term-untrimmed (or (plist-get source :from) ; new term
                                    (plist-get source :other))) ; repeat term
         (source-term (wr--cull-double-spaces
                       (string-trim source-term-untrimmed)))
         (source-pos (plist-get source :pos))
         (context (cadr def))
         (context-term-untrimmed (plist-get context :other))
         (context-term (when context-term-untrimmed
                         (wr--cull-double-spaces
                          (wr--cull-single-spaces-in-brackets
                           (wr--cull-space-between-brackets
                            (string-trim context-term-untrimmed))))))
         (target (caddr def))
         (target-term
          (when target (wr--cull-double-spaces
                        (string-trim
                         (plist-get target :to)))))
         (target-pos (plist-get target :pos))
         (eg (cond ((string= (plist-get source :other) " ")
                    (plist-get context :example))
                   ;; process "Note" as eg, not as source-term:
                   ((string-prefix-p "Note :" (plist-get source :other))
                    (plist-get source :other)))))
    (if eg
        (insert
         (concat "\n -- "
                 (propertize eg
                             'face '((t :height 0.8)))))
      (progn
        (insert
         (concat
          (when source-term
            (if (string= source-term " ")
                (propertize "\n\""
                            'face font-lock-comment-face)
              (progn
                (concat
                 "\n\n"
                 (propertize
                  source-term
                  'button t
                  'face '((t :inherit warning)))))))
          " "
          (propertize (or source-pos
                          "")
                      'face font-lock-comment-face)
          " "
          (when (and context-term
                     (not (string= context-term " ")))
            (propertize context-term
                        'face '((t :inherit font-lock-comment-face :slant italic))))
          "\n           "
          (propertize "--> "
                      'face font-lock-comment-face)
          (when target-term
            (propertize target-term
                        'button t
                        'face '((t :inherit warning))))
          " "
          (propertize (or target-pos
                          "")
                      'face font-lock-comment-face)))))))


;; NAVIGATION etc.

(defun wordreference-next-heading ()
  "Move point to next heading."
  (interactive)
  (save-match-data
    (let ((match
           (save-excursion
             (text-property-search-forward 'heading ;NB 27.1!
                                           t t t))))
      (if match
          (progn
            (goto-char (prop-match-beginning match))
            (recenter-top-bottom 3))
        (message "No more headings.")))))

(defun wordreference-previous-heading ()
  "Move point to previous heading."
  (interactive)
  (save-match-data
    (let ((match
           (save-excursion
             (text-property-search-backward 'heading ;NB 27.1!
                                            t t t))))
      (if match
          (progn
            (goto-char (prop-match-beginning match))
            (recenter-top-bottom 3))
        (message "No more headings.")))))

(defun wr--make-buttons ()
  "Make all property ranges with button property into buttons."
  (with-current-buffer (get-buffer "*wordreference*")
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (next-single-property-change (point) 'button)
          (make-text-button
           (goto-char (next-single-property-change (point) 'button))
           (goto-char (next-single-property-change (point) 'button))))))))

(defvar wordreference-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "w") #'wordreference-search)
    ;; (define-key map (kbd "b") #'wordreference-browse-url-results)
    (define-key map (kbd ",") #'wordreference-previous-heading)
    (define-key map (kbd ".") #'wordreference-next-heading)
    ;; (define-key map (kbd "f") #'wordreference-jump-to-forum-results)
    ;; (define-key map (kbd "<") #'wordreference-translate-left-side-only)
    ;; (define-key map (kbd ">") #'wordreference-translate-right-side-only)
    ;; (when (require 'dictcc nil :noerror)
      ;; (define-key map (kbd "c") #'wordreference--search-term-with-dictcc))
    map)
  "Keymap for wordreference mode.")

(define-derived-mode wordreference-mode special-mode "wordreference"
  :group 'wordreference
  (read-only-mode 1))

(provide 'wordreference)
;;; wordreference.el ends here
