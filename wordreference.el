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
(require 'xml)
(require 'dom)
(require 'shr)
(require 'browse-url)
(require 'text-property-search)

(defgroup wordreference nil
  "Wordreference dictionary interface."
  :group 'wordreference)

(defcustom wordreference-browse-url-function nil
  "The browser that is used to access online dictionaries."
  :group 'wordreference
  :type '(choice
          (const         :tag "Default" :value nil)
          (function-item :tag "Emacs W3" :value  browse-url-w3)
          (function-item :tag "W3 in another Emacs via `gnudoit'"
                         :value  browse-url-w3-gnudoit)
          (function-item :tag "Mozilla" :value  browse-url-mozilla)
          (function-item :tag "Firefox" :value browse-url-firefox)
          (function-item :tag "Chromium" :value browse-url-chromium)
          (function-item :tag "Gawordreferencen" :value  browse-url-gawordreferencen)
          (function-item :tag "Epiphany" :value  browse-url-epiphany)
          (function-item :tag "Netscape" :value  browse-url-netscape)
          (function-item :tag "eww" :value  eww-browse-url)
          (function-item :tag "Text browser in an xterm window"
                         :value browse-url-text-xterm)
          (function-item :tag "Text browser in an Emacs window"
                         :value browse-url-text-emacs)
          (function-item :tag "KDE" :value browse-url-kde)
          (function-item :tag "Elinks" :value browse-url-elinks)
          (function-item :tag "Specified by `Browse Url Generic Program'"
                         :value browse-url-generic)
          (function-item :tag "Default Windows browser"
                         :value browse-url-default-windows-browser)
          (function-item :tag "Default Mac OS X browser"
                         :value browse-url-default-macosx-browser)
          (function-item :tag "GNOME invoking Mozilla"
                         :value browse-url-gnome-moz)
          (function-item :tag "Default browser"
                         :value browse-url-default-browser)
          (function      :tag "Your own function")
          (alist         :tag "Regexp/function association list"
                         :key-type regexp :value-type function)))

(defvar wordreference-source-lang "fr"
  "Default source language.")

(defvar wordreference-target-lang "en"
  "Default target language.")

(defvar wordreference-base-url
  "https://www.wordreference.com"
  "Base wordreference URL.")

(defvar wordreference-results-info nil
  "Information about the current results from a word reference search.
Used to store search term for `wordreference-leo-browse-url-results'.")
(make-variable-buffer-local 'wordreference-results-info)

(defvar wordreference-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "s") #'wordreference-search)
    (define-key map (kbd "w") #'wordreference-search)
    (define-key map (kbd "b") #'wordreference-browse-url-results)
    (define-key map (kbd "c") #'wordreference-copy-search-term)
    (define-key map (kbd ",") #'wordreference-previous-heading)
    (define-key map (kbd ".") #'wordreference-next-heading)
    (define-key map (kbd "RET") #'wordreference--return-search-word)
    (define-key map (kbd "S") #'wordreference-switch-source-target-and-search)
    ;; (define-key map (kbd "f") #'wordreference-jump-to-forum-results)
    map)
  "Keymap for wordreference mode.")

(defvar wordreference-result-search-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [mouse-2] #'wordreference--click-search-word)
    (define-key map (kbd "RET") #'wordreference--return-search-word)
    map))


;; REQUESTING AND PARSING

(defun wordreference--retrieve-parse-html (word &optional source target)
  "Query wordreference.com for WORD, and parse the HTML response."
  (let* ((url (concat wordreference-base-url
                      (format "/%s%s/%s"
                              (or source
                                  wordreference-source-lang)
                              (or target
                                  wordreference-target-lang)
                              word)))
	 (html-buffer (url-retrieve-synchronously url)))
    (with-current-buffer html-buffer
      (goto-char (point-min))
      (libxml-parse-html-region
       (search-forward "\n\n") (point-max)))))

(defun wordreference--get-tables (dom)
  "Get tables from parsed HTML response DOM."
  (dom-by-tag dom 'table))

(defun wordreference--get-word-tables (tables)
  (let ((word-tables))
    (mapcar (lambda (x)
              (when (equal (dom-attr x 'class) "WRD")
                (push x word-tables)))
            tables)
    (reverse word-tables)))

(defun wordreference--get-trs (word-table)
  (dom-children word-table))

(defun wordreference-extract-lang-code-from-td (td)
  ""
  ;; format is "sLang_en", but is it always?
  (when td
  (substring-no-properties
   (dom-attr
    (dom-by-tag td 'span)
    'data-ph)
   -2))) ;last two chars

(defun wordreference-collect-trs-results-list (trs)
  ""
  (let* ((title-tr (car trs))
         (langs-tr (cadr trs))
         (source-td (dom-by-class langs-tr "FrWrd"))
         (source-word (dom-texts source-td))
         (source-abbrev (wordreference-extract-lang-code-from-td source-td))
         (target-td (dom-by-class langs-tr "ToWrd"))
         (target-word (dom-texts target-td))
         (target-abbrev (wordreference-extract-lang-code-from-td target-td))
         (words-trs (cddr trs)))
    (list
     `(:title ,(dom-texts title-tr) :source ,source-abbrev :target ,target-abbrev)
     (mapcar (lambda (tr)
               (wordreference--build-tds-text-list tr))
             words-trs))))

(defun wordreference--build-tds-text-list (tr)
  "Return a list of results of both source and target langs from TR."
  (let ((tds (dom-by-tag tr 'td)))
    ;; (setq wr-single-td (caddr tds))
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
                     (dom-texts (dom-by-tag td 'strong))
                   (dom-text td)))
                (conj (dom-by-tag
                       (dom-by-tag td 'strong)
                       'a))
                (conj-link-suffix (dom-attr conj 'href)))
           `(,to-or-from ,term-text :pos ,pos :tooltip ,tooltip-text :conj ,conj-link-suffix)))
        ((or (dom-by-class td "ToEx")
             (dom-by-class td "FrEx"))
         `(:example ,(dom-texts td)))
        (t
         `(:other ,(dom-texts td)))))

;;;###autoload
(defun wordreference-search (word &optional source target)
  ""
  (interactive "MWordreference search: ")
  (with-current-buffer (get-buffer-create "*wordreference*")
    (let* ((inhibit-read-only t)
           (html-parsed (wordreference--retrieve-parse-html word source target))
           (tables (wordreference--get-tables html-parsed))
           (word-tables (wordreference--get-word-tables tables))
           (pr-table (car word-tables))
           (pr-trs (wordreference--get-trs pr-table))
           (pr-trs-results-list (wordreference-collect-trs-results-list pr-trs))
           ;; (sup-trs-table (cdr word-tables))
           ;; (sup-trs (wordreference--get-trs sup-trs-table))
           ;; (sup-trs-results-list (wordreference-collect-trs-results-list sup-trs))
           ;; (comp-table (dom-by-id word-tables "compound_forms"))
           ;; (comp-trs (wordreference--get-trs comp-table))
           ;; (comp-trs-results-list (wordreference-collect-trs-results-list comp-trs))
           (source-lang (plist-get (car pr-trs-results-list) :source))
           (target-lang (plist-get (car pr-trs-results-list) :target)))
      ;; Debugging:
      ;; (setq wordreference-word-tables word-tables)
      ;; (setq wordreference-full-html html-parsed)
      ;; (setq wordreference-full-tables word-tables)
      ;; (setq wordreference-single-tr (caddr (wordreference--get-trs pr-table)))
      ;; (setq wordreference-full-pr-trs-list pr-trs-results-list)
      ;; (setq wordreference-full-sup-trs-list sup-trs-results-list)
      (erase-buffer)
      (wordreference-mode)
      ;; print principle, supplementary, and compound tables:
      (mapcar (lambda (x)
                (wordreference-print-trs-results
                 (wordreference-collect-trs-results-list
                  (wordreference--get-trs x))))
              word-tables)
      (setq-local header-line-format
                  (propertize
                   (format "Wordreference results for \"%s\" from %s to %s:"
                           word
                           source-lang
                           target-lang)
                   'face font-lock-comment-face))
      (setq-local wordreference-results-info
                  `(term ,word
                         source ,source-lang
                         target ,target-lang))
      (wordreference--make-buttons)
      (wordreference-prop-query-in-results word)
      (goto-char (point-min))))
  ;; handle searching again from wr:
  (when (not (equal (buffer-name (current-buffer)) "*wordreference*"))
    (switch-to-buffer-other-window (get-buffer "*wordreference*")))
  (message "w/s: search again, ./,: next/prev heading, b: view in browser, TAB: jump to terms, c: copy search term, S: switch langs and search."))


;; PRINTING:

(defun wordreference-prop-query-in-results (word)
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

(defun wordreference-print-heading (heading)
  ""
  (insert (propertize heading
                      'heading t
                      'face '((t :inherit font-lock-function-name-face
                                 :weight bold)))))

(defun wordreference-print-trs-results (trs)
  ""
  (let* ((info (car trs))
         (table-name (plist-get info :title))
         (source (plist-get info :source))
         (target (plist-get info :target))
         (definitions (cadr trs)))
    (when table-name
      (wordreference-print-heading table-name))
    (if definitions
        (wordreference-print-definitions definitions)
      (insert "looks like wordreference returned nada."))
    (insert "\n")))

(defun wordreference-print-definitions (defs)
  ""
  (mapc (lambda (def)
            (wordreference-print-single-definition def))
          defs)
  (insert "\n"))

(defun wordreference--cull-double-spaces (result)
  "Remove any double spaces from RESULT."
  (save-match-data
    (while (string-match "[[:blank:]]\\{2\\}"
                         result)
      (setq result (replace-match
                    " "
                    t nil result))))
  result)

(defun wordreference--cull-single-spaces-in-brackets (result)
  "Remove any spaces inside brackets from RESULT."
  ;;TODO: rewrite to handle single spaces also
  (save-match-data
    (while (string-match
            ;; ( + SPC + any number of chars + SPC + ):
            "([[:blank:]]\\(.*\\)[[:blank:]])"
            result)
      (setq result (replace-match
                    "(\\1)"
                    t nil result))))
  result)

(defun wordreference--cull-space-between-brackets (result)
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

(defun wordreference-print-single-definition (def)
  "Print a single definition DEF in the buffer.
\nFor now a definition can be a set of source term, context term,
and target term, or an example sentence."
  (let* ((source (car def))
         (source-term-untrimmed (or (plist-get source :from) ; new term
                                    (plist-get source :other))) ; repeat term
         (source-term (wordreference--cull-double-spaces
                       (string-trim source-term-untrimmed)))
         (source-pos (plist-get source :pos))
         (source-conj (plist-get source :conj))
         (context (cadr def))
         (context-term-untrimmed (plist-get context :other))
         (context-term (when context-term-untrimmed
                         (wordreference--cull-double-spaces
                          (wordreference--cull-single-spaces-in-brackets
                           (wordreference--cull-space-between-brackets
                            (string-trim context-term-untrimmed))))))
         (target (caddr def))
         (target-term
          (when target (wordreference--cull-double-spaces
                        (string-trim
                         (plist-get target :to)))))
         (target-pos (plist-get target :pos))
         (target-conj (plist-get target :conj))
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
                  'type 'source
                  'keymap wordreference-result-search-map
                  'help-echo "RET to search wordreference for this term"
                  'face '((t :inherit warning)))))))
          " "
          (when source-conj
            (concat
             (propertize
              (if (fontp (char-displayable-p #10r9638))
                  "▦"
                "#")
              'button t
              'follow-link t
              'shr-url (concat wordreference-base-url source-conj)
              'keymap leo-inflexion-table-map
              'fontified t
              'face 'leo-auxiliary-face
              'mouse-face 'highlight
              'help-echo (concat "Browse inflexion table for '"
                                 source-term "'"))
             " "))
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
                        'type 'target
                        'keymap wordreference-result-search-map
                        'help-echo "RET to search wordreference for this term"
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

(defun wordreference--make-buttons ()
  "Make all property ranges with button property into buttons."
  (with-current-buffer (get-buffer "*wordreference*")
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (next-single-property-change (point) 'button)
          (make-text-button
           (goto-char (next-single-property-change (point) 'button))
           (goto-char (next-single-property-change (point) 'button))))))))

(defun wordreference-get-results-info-item (item)
  ""
  (plist-get wordreference-results-info item))

(defun wordreference--return-search-word ()
  "Translate word or phrase at point.
Word or phrase at point is determined by button text property."
  ;;TODO: make this work with word at point
  (interactive)
  (let* ((text (buffer-substring-no-properties
                (progn
                  (if (looking-back "[ \t\n]" nil) ; enter range if we tabbed here
                      (forward-char))
                  (previous-single-property-change (point) 'button)) ; range start
                (next-single-property-change (point) 'button)))
         (text-type (get-text-property (point) 'type))
         (text-lang (if (equal text-type 'source)
                        (wordreference-get-results-info-item 'source)
                      (wordreference-get-results-info-item 'target)))
         (other-lang (if (equal text-type 'source)
                         (wordreference-get-results-info-item 'target)
                       (wordreference-get-results-info-item 'source))))
    (wordreference-search text text-lang other-lang)))

(defun wordreference-browse-url-results ()
  "Open the current results in external browser.
Uses `wordreference-browse-url-function' to decide which browser to use."
  (interactive)
  (let* ((url (concat wordreference-base-url
                      (format "/%s%s/"
                      wordreference-source-lang
                      wordreference-target-lang)))
         (word (plist-get wordreference-results-info 'term))
         (search-url (concat url word))
         (browse-url-browser-function (or wordreference-browse-url-function
                                          (when (browse-url-can-use-xdg-open)
                                            '(browse-url-xdg-open))
                                          browse-url-secondary-browser-function
                                          browse-url-browser-function)))
    (browse-url search-url)))

(defun wordreference-copy-search-term ()
  "Copy current search term to the kill ring."
  (interactive)
  (let ((term (plist-get wordreference-results-info 'term)))
    (kill-new term)
    (message (concat "\"" term "\" copied to clipboard."))))

(defun wordreference-switch-source-target-and-search ()
  ""
  (interactive)
  (let ((word-reference-target-lang (plist-get wordreference-results-info 'source))
        (wordreference-source-lang (plist-get wordreference-results-info 'target))
        (term (plist-get wordreference-results-info 'term)))
    (wordreference-search term)))

(defun wordreference-browse-conjugation-for-term ()
  ""
  (interactive)
  (let* ((term (word-at-point)) ;FIXME: this wont work when point on a conj icon!
         ;;FIXME: if we search in our target not source lang,
         ;; WR still works and flips the langs, leaving our vars backwards
         ;; can we store real langs somehow?
         (lang (if (equal (get-text-property (point) 'type) 'source)
                   wordreference-source-lang
                 wordreference-target-lang))
         (url (concat wordreference-conj-base-url
                      lang
                      wordreference-conj-url-query
                      term))
         (browse-url-browser-function (or wordreference-browse-url-function
                                          (when (browse-url-can-use-xdg-open)
                                            'browse-url-xdg-open)
                                          browse-url-secondary-browser-function
                                          browse-url-browser-function)))
    (browse-url url)))

(define-derived-mode wordreference-mode special-mode "wordreference"
  :group 'wordreference
  (read-only-mode 1))

(provide 'wordreference)
;;; wordreference.el ends here
