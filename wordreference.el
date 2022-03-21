;;; wordreference.el --- Interface for wordreference.com -*- lexical-binding:t -*-
;;
;; Author: Marty Hiatt <martianhiatus AT riseup.net>
;; Copyright (C) 2022 Marty Hiatt <martianhiatus AT riseup.net>
;;
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, translate
;; URL: https://codeberg.org/martianh/wordreference
;; Version: 0.2
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
(require 'thingatpt)
(require 'text-property-search)

(when (require 'pdf-tools nil :no-error)
  (declare-function pdf-view-active-region-text "pdf-view"))

(when (require 'helm-dictionary nil :noerror)
  (declare-function helm-dictionary "helm-dictionary")
  (defvar helm-dictionary-database)
  (defvar wordreference-helm-dictionary-name "fr-en"
    "The name of the dictionary to use for `helm-dictionary' queries.\
It must match the key of one of the dictionaries in `helm-dictionary-database'."))

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

(defcustom wordreference-source-lang "fr"
  "Default source language."
  :group 'wordreference
  :type 'string)

(defcustom wordreference-target-lang "en"
  "Default target language."
  :group 'wordreference
  :type 'string)

(defvar wordreference-languages-full
  '("en" "fr" "it" "es" "pt" "de" "ru" "tr" "gr" "cz" "pl" "zh" "ja" "ko" "ar" "is" "nl" "sv")
  "List of all wordreference languages.")

(defvar wordreference-base-url
  "https://www.wordreference.com"
  "Base wordreference URL.")

(defvar wordreference-results-info nil
  "Information about the current results from a word reference search.
Used to store search term for `wordreference-leo-browse-url-results'.")
(make-variable-buffer-local 'wordreference-results-info)

(defvar-local wordreference-nearby-entries nil)

(defvar wordreference-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'backward-button)
    (define-key map (kbd "s") #'wordreference-search)
    (define-key map (kbd "w") #'wordreference-search)
    (define-key map (kbd "b") #'wordreference-browse-url-results)
    (define-key map (kbd "C") #'wordreference-copy-search-term)
    (define-key map (kbd "d") #'wordreference-helm-dict-search)
    (define-key map (kbd "c") #'wordreference-browse-term-cntrl)
    (define-key map (kbd "l") #'wordreference-browse-term-linguee)
    (define-key map (kbd "n") #'wordreference-nearby-entries-search)
    (define-key map (kbd ",") #'wordreference-previous-heading)
    (define-key map (kbd ".") #'wordreference-next-heading)
    (define-key map (kbd "RET") #'wordreference--return-search-word)
    (define-key map (kbd "S") #'wordreference-switch-source-target-and-search)
    map)
  "Keymap for wordreference mode.")

(defvar wordreference-result-search-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map [mouse-2] #'wordreference--click-search-word)
    (define-key map (kbd "RET") #'wordreference--return-search-word)
    map))

(defvar wordreference-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'shr-browse-url)
    (define-key map (kbd "RET") #'shr-browse-url)
    map))


;; REQUESTING AND PARSING

(defun wordreference--retrieve-parse-html (word &optional source target)
  "Query wordreference.com for WORD, and parse the HTML response.
Optionally specify SOURCE and TARGET languages."
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

(defun wordreference--get-nearby-entries (dom)
  "Fetch list of nearby entries from HTML response DOM."
  (let* ((entries-table (dom-by-id
                         (wordreference--get-tables dom)
                         "contenttable"))
         (entries-tr-ul (dom-by-id (car entries-table) "left"))
         (entries-tr-ul-li-ul (dom-by-tag (car entries-tr-ul) 'ul))
         ;; (entries-heading (dom-text
         ;; (car entries-tr-ul-li-ul)))
         (entries-rest (cdr entries-tr-ul-li-ul))
         (entries-link-list (dom-by-tag (cdr entries-rest) 'a)))
    (mapcar (lambda (x)
              (dom-texts x))
            entries-link-list)))

(defun wordreference--get-word-tables (tables)
  "Get all word tables from list of TABLES."
  (let ((word-tables))
    (mapc (lambda (x)
            (when (equal (dom-attr x 'class) "WRD")
              (push x word-tables)))
          tables)
    (reverse word-tables)))

(defun wordreference--get-trs (word-table)
  "Get all table rows from a WORD-TABLE."
  (dom-children word-table))

(defun wordreference-extract-lang-code-from-td (td)
  "Extract a two letter language code from TD."
  ;; format is "sLang_en", but is it always?
  (when td
    (substring-no-properties
     (dom-attr
      (dom-by-tag td 'span)
      'data-ph)
     -2))) ;last two chars

(defun wordreference-collect-trs-results-list (trs)
  "Process the results found in TRS.
\nReturns a nested list first containing information about the
heading title, followed by source and target languages. This is
followed by a list of textual results returned by
`wordreference--build-tds-text-list'."
  (let* ((title-tr (car trs))
         (langs-tr (cadr trs))
         (source-td (dom-by-class langs-tr "FrWrd"))
         ;; (source-word (dom-texts source-td))
         (source-abbrev (wordreference-extract-lang-code-from-td source-td))
         (target-td (dom-by-class langs-tr "ToWrd"))
         ;; (target-word (dom-texts target-td))
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
    (mapcar (lambda (x)
              (wordreference-build-single-td-list x))
            tds)))

(defun wordreference-build-single-td-list (td)
  "Return textual result for a single TD.
\nReturns a property list containing to or from term, position of
speech, tooltip info, and conjunction table for a result, an
example for an example, and other for everything else."
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
                (conj (if (dom-by-class td "FrWrd")
                          (progn (dom-by-tag
                                  (dom-by-tag td 'strong)
                                  'a))
                        (dom-by-tag td 'a)))
                (conj-link-suffix (dom-attr conj 'href)))
           `(,to-or-from ,term-text :pos ,pos :tooltip ,tooltip-text :conj ,conj-link-suffix)))
        ((or (dom-by-class td "ToEx")
             (dom-by-class td "FrEx"))
         `(:example ,(dom-texts td)))
        ((or (dom-by-class td "notePubl")
             (string-prefix-p "Note :" (dom-texts td)))
         `(:note ,(dom-texts td)))
        (t
         `(:other ,(dom-texts td)))))


;; PRINTING:

(defun wordreference-print-translation-buffer (word html-parsed &optional source target)
  "Print translation results in buffer.
WORD is the search query, HTML-PARSED is what our query returned.
SOURCE and TARGET are languages."
  (with-current-buffer (get-buffer-create "*wordreference*")
    (let* ((inhibit-read-only t)
           (tables (wordreference--get-tables html-parsed))
           (word-tables (wordreference--get-word-tables tables))
           (pr-table (car word-tables))
           (pr-trs (wordreference--get-trs pr-table))
           (pr-trs-results-list (wordreference-collect-trs-results-list pr-trs))
           (post-article (dom-by-id html-parsed "postArticle"))
           (forum-heading (dom-by-id post-article "threadsHeader"))
           (forum-heading-string (dom-texts forum-heading))
           (forum-links (dom-children
                         (dom-by-id post-article "lista_link")))
           (forum-links-propertized
            (wordreference-process-forum-links forum-links))
           (source-lang (or (plist-get (car pr-trs-results-list) :source)
                            source
                            wordreference-source-lang))
           (target-lang (or (plist-get (car pr-trs-results-list) :target)
                            target
                            wordreference-target-lang)))
      ;; Debugging:
      ;; (setq wr-html html-parsed)
      ;; (setq wr-post-article post-article)
      ;; (setq wordreference-word-tables word-tables)
      ;; (setq wr-comp-table comp-table)
      ;; (setq wordreference-full-html html-parsed)
      ;; (setq wr-forum-links forum-links)
      ;; (setq wordreference-full-tables word-tables)
      ;; (setq wordreference-single-tr (caddr (wordreference--get-trs pr-table)))
      ;; (setq wordreference-full-pr-trs-list pr-trs-results-list)
      ;; (setq wordreference-full-sup-trs-list sup-trs-results-list)
      (erase-buffer)
      (wordreference-mode)
      ;; print principle, supplementary, particule verbs, and compound tables:
      (if (not word-tables)
          (insert "looks like wordreference returned nada.\n\nHit 'S' to search again with languages reversed.\n\n")
        (wordreference-print-tables word-tables))
      ;; print list of term aslso found in these entries
      (wordreference-print-also-found-entries html-parsed)
      ;; print forums
      (wordreference-print-heading forum-heading-string)
      (if (dom-by-class forum-links "noThreads")
          ;; no propertize if no threads:
          (insert "\n\n" (dom-texts (car forum-links)))
        (wordreference-print-forum-links forum-links-propertized))
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
      (setq-local wordreference-nearby-entries
                  (wordreference--get-nearby-entries html-parsed))
      (wordreference--make-buttons)
      (wordreference-prop-query-in-results word)
      (goto-char (point-min))))
  ;; handle searching again from wr:
  (when (not (equal (buffer-name (current-buffer)) "*wordreference*"))
    (switch-to-buffer-other-window (get-buffer "*wordreference*")))
  (message "w/s: search again, ./,: next/prev heading, b: view in browser, TAB: jump to terms, C: copy search term, n: browse nearby entries, S: switch langs and search, c: browse on www.cntrl.fr."))

(defun wordreference-prop-query-in-results (word)
  "Propertize query WORD in results buffer."
  (let ((word-spl (split-string word)))
    (save-excursion
      (goto-char (point-min))
      (mapc (lambda (x)
              (while (and (search-forward-regexp (concat "\\b" x "\\b")
                                                 nil 'noerror)
                          ;;don't add props to note boxes:
                          (not (equal (get-text-property (point) 'face)
                                      '(:height 0.8 :box t))))
                (add-text-properties (- (point) (length x)) (point)
                                     '(face (:inherit success :weight bold))))
              (goto-char (point-min)))
            word-spl))))

(defun wordreference-print-heading (heading)
  "Insert a single propertized HEADING."
  (insert (propertize heading
                      'heading t
                      'face '(:inherit font-lock-function-name-face
                                       :weight bold))))

(defun wordreference-print-tables (tables)
  "Print a list of TABLES."
  (mapcar (lambda (x)
            (wordreference-print-trs-results
             (wordreference-collect-trs-results-list
              (wordreference--get-trs x))))
          tables))

(defun wordreference-print-trs-results (trs)
  "Print a section heading followed by its definitions.
TRS is the list of table rows from the parsed HTML."
  (let* ((info (car trs))
         (table-name (plist-get info :title))
         (definitions (cadr trs)))
    (when table-name
      (wordreference-print-heading table-name))
    (if definitions
        (wordreference-print-definitions definitions)
      (insert "looks like wordreference returned nada."))
    (insert "\n")))

(defun wordreference-print-definitions (defs)
  "Print a list of definitions DEFS."
  (mapc (lambda (def)
          (wordreference-print-single-definition def))
        defs)
  (insert "\n"))

(defun wordreference--cull-double-spaces (result)
  "Remove any double spaces from RESULT."
  (when result
  (save-match-data
    (while (string-match "[[:blank:]]\\{2\\}"
                         result)
      (setq result (replace-match
                    " "
                    t nil result))))
  result))

(defun wordreference--cull-conj-arrows (result)
  "Remove any conjugation arrows from RESULT."
  (when result
  (save-match-data
    (while (string-match " ⇒"
                         result)
      (setq result (replace-match
                    ""
                    t nil result))))
  result))

(defun wordreference--cull-single-spaces-in-brackets (result)
  "Remove any spaces inside brackets from RESULT."
  ;;TODO: rewrite to handle single spaces also
  (when result
    (save-match-data
      (while (string-match
              ;; ( + SPC + any number of chars + SPC + ):
              "([[:blank:]]\\(.*\\)[[:blank:]])"
              result)
        (setq result (replace-match
                      "(\\1)"
                      t nil result))))
    result))

(defun wordreference--cull-space-between-brackets (result)
  "Remove any spaces between closing brackets from RESULT."
  (when result
    (save-match-data
      (while (string-match
              ;; ] + SPC + ) :
              "][[:blank:]])"
              result)
        (setq result (replace-match
                      "])"
                      t nil result))))
  result))

(defun wordreference-print-single-definition (def)
  "Print a single definition DEF in the buffer.
\nFor now a definition can be a set of source term, context term,
and target term, or an example sentence."
  (let* ((source (car def))
         (source-term-untrimmed (or (plist-get source :from) ; new term
                                    (plist-get source :other))) ; repeat term
         (source-term (when source-term-untrimmed
                        (wordreference--cull-conj-arrows
                         (wordreference--cull-double-spaces
                          (string-trim source-term-untrimmed)))))
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
         (eg (when (string= (plist-get source :other) " ")
               (plist-get context :example)))
         (note (plist-get source :note)))

    (cond
     (eg
      (insert
       (concat "\n -- "
               (propertize eg
                           'face '(:height 0.8)))))
     (note
      (insert
       (concat "\n -- "
               (propertize note
                           'face '(:height 0.8 :box t)))))
     (t
      (insert
       (concat
        (when source-term
          (if (string= source-term " ")
              (propertize "\n\"\"" ;; for repeat terms
                          'face font-lock-comment-face)
            (concat
             "\n\n"
             (wordreference-propertize-result-term source-term))))
        " "
        (when source-conj
          (concat
           (wordreference--propertize-conjunction-link source-term source-conj)
           " "))
        (propertize (or source-pos
                        "")
                    'face font-lock-comment-face)
        " "
        (when (and context-term
                   (not (string= context-term " ")))
          (propertize context-term
                      'face '(:inherit font-lock-comment-face :slant italic)))
        "\n           "
        (propertize "--> "
                    'face font-lock-comment-face)
        (when target-term
          (wordreference-unpropertize-source-phrase-in-target
           (propertize target-term
                       'button t
                       'type 'target
                       'keymap wordreference-result-search-map
                       'help-echo "RET to search wordreference for this term"
                       'face 'warning)))
        (when target-conj
          (concat
           " "
           (wordreference--propertize-conjunction-link target-term target-conj)))
        " "
        (propertize (or target-pos
                        "")
                    'face font-lock-comment-face)))))))

(defun wordreference-propertize-result-term (term)
  "Propertize result TERM in results buffer."
  (propertize
   term
   'button t
   'type 'source
   'keymap wordreference-result-search-map
   'help-echo "RET to search wordreference for this term"
   'face 'warning))

(defun wordreference--propertize-conjunction-link (term conj)
  "Propertize the icon link to conjunction table CONJ for TERM."
  (propertize
   (if (fontp (char-displayable-p #10r9638))
       "▦"
     "#")
   'button t
   'follow-link t
   'shr-url (concat wordreference-base-url conj)
   'keymap wordreference-link-map
   'fontified t
   'face font-lock-comment-face
   'mouse-face 'highlight
   'help-echo (concat "Browse inflexion table for '"
                      term "'")))

(defun wordreference-print-also-found-entries (html)
  "Insert a propertized list of 'also found in' entries.
HTML is what our original query returned."
  (let* ((also-found (dom-by-id html "FTintro"))
         (also-found-heading (string-trim (dom-texts also-found)))
         (also-found-langs (dom-by-class html "FTsource"))
         (also-found-source (string-trim
                             (dom-texts (car also-found-langs))))
         (also-found-target (string-trim
                             (dom-texts (cdr also-found-langs))))
         (also-list (dom-by-class html "FTlist"))
         (also-list-source (dom-by-tag (car also-list) 'a))
         (also-list-target (dom-by-tag (cdr also-list) 'a)))
    (when also-found
      (wordreference-print-heading also-found-heading)
      (insert
       (if (not also-list-source)
           ""
         (concat "\n"
                 also-found-source
                 "\n"
                 (wordreference-insert-also-found-list
                  also-list-source)))
       (if (not also-list-target)
           ""
         (concat "\n"
                 also-found-target
                 "\n"
                 (wordreference-insert-also-found-list
                  also-list-target)))
       "\n\n"))))

(defun wordreference-insert-also-found-list (list)
  "Propertize a LIST of 'also found in' entries."
  (mapconcat (lambda (x)
               (let* ((link (dom-by-tag x 'a))
                      (link-text (dom-text link))
                      (link-suffix (dom-attr link 'href)))
                 (propertize link-text
                             'button t
                             'follow-link t
                             'shr-url (concat wordreference-base-url
                                              link-suffix)
                             'keymap wordreference-result-search-map
                             'fontified t
                             'face 'warning
                             'mouse-face 'highlight
                             'help-echo (concat "Search for '"
                                                link-text "'"))))
             list
             " - "))

(defun wordreference-process-forum-links (links)
  "Propertize LINKS to forum entries for inserting."
  (mapcar (lambda (x)
            (when (and (not (stringp x)) ; skip " - grammaire" string for now
                       (not (equal (dom-tag x) 'br))) ; skip empty br tags too
              (let ((forum-text (dom-text x))
                    (forum-href (dom-attr x 'href)))
                (propertize forum-text
                            'button t
                            'follow-link t
                            'shr-url forum-href
                            'keymap wordreference-link-map
                            'fontified t
                            'face 'warning
                            'mouse-face 'highlight
                            'help-echo (concat "Browse forums for '"
                                               forum-text "'")))))
          links))

(defun wordreference-print-forum-links (links)
  "Print a list of LINKS to forum entries."
  (mapcar (lambda (x)
            (when x ; skip all our empties
              (insert "\n\n" x)))
          links))


;; BUFFER, NAVIGATION etc.

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
  "Get ITEM from `wordreference-results-info'."
  (plist-get wordreference-results-info item))

(defun wordreference--return-search-word ()
  "Translate result word or phrase at point.
Word or phrase at point is determined by button text property."
  (interactive)
  ;;TODO: remove all [sb] etc from result phrases
  ;; "assign [sth] to [sb]" should search "assign to"
  (let* ((result-entry
          (wordreference-cull-brackets-from-entry
           (buffer-substring-no-properties
                 (progn
                   (if (looking-back "[ \t\n]" nil) ; enter range if we tabbed here
                       (forward-char))
                   (previous-single-property-change (point) 'button)) ; range start
                 (next-single-property-change (point) 'button))))
         ;; handle calling this on a multi-term result:
         (text (let ((results
                       (wordreference-cull-brackets-from-entry-list
                        (split-string result-entry "[,;] "))))
                 (if (< 1 (length results))
                     (completing-read "Select or enter search term: " results nil nil nil nil (car results))
                   result-entry)))
         (text-type (get-text-property (point) 'type))
         (text-lang (if (equal text-type 'source)
                        (wordreference-get-results-info-item 'source)
                      (wordreference-get-results-info-item 'target)))
         (other-lang (if (equal text-type 'source)
                         (wordreference-get-results-info-item 'target)
                       (wordreference-get-results-info-item 'source))))
    (wordreference-search nil text text-lang other-lang)))

(defun wordreference-cull-brackets-from-entry-list (entries)
  "Cull any [bracketed] parts of a results in ENTRIES."
  (mapcar (lambda (entry)
            (wordreference-cull-brackets-from-entry entry))
          entries))

(defun wordreference-cull-brackets-from-entry (entry)
  "Cull any [bracketed] parts of a result ENTRY.
Used by `wordreference--return-search-word'."
  (save-match-data
    (while (string-match " \\(\\+ \\)?\\[.*?\\]"
                         ;; SPC + possible "+" + [anything], lazy match
                         entry )
      (setq entry (replace-match
                   ""
                   t nil entry))))
  entry)

(defun wordreference-unpropertize-source-phrase-in-target (entry)
  "Remove properties from any source phrase in ENTRY."
  (save-match-data
    (if (string-match ".* :"
                  ;; any chars + SPC + :
                        entry)
        (let ((match (match-string-no-properties 0 entry)))
                (replace-match
                       match
                       t nil entry))
      entry))) ; else do nothing

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
  "Search for same term with source and target reversed."
  (interactive)
  (let ((target (plist-get wordreference-results-info 'source))
        (source (plist-get wordreference-results-info 'target))
        (term (plist-get wordreference-results-info 'term)))
    (wordreference-search nil term source target)))

(defun wordreference-nearby-entries-search ()
  "Select an item from nearby dictionary items and search for it."
  (interactive)
  (let ((word (completing-read "View nearby entry: "
                               wordreference-nearby-entries
                               nil
                               nil)))
    (wordreference-search nil word)))

;; NB: runs on a modified `helm-dictionary'!:
(defun wordreference-helm-dict-search ()
  "Search for term in `helm-dictionary'.
\nUses the dictionary specified in `wordreference-helm-dictionary-name'."
  (interactive)
  (let ((query (concat "\\b"
                       (plist-get wordreference-results-info 'term)
                       "\\b")))
    (helm-dictionary wordreference-helm-dictionary-name query t)))

(defun wordreference-browse-term-cntrl ()
  "Search for the same term on https://www.cntrl.fr."
  ;;TODO: handle multi-term queries better
  (interactive)
  (let ((query (plist-get wordreference-results-info 'term)))
    (browse-url-generic (concat "https://www.cnrtl.fr/definition/"
                                query))))

(defun wordreference-browse-term-linguee ()
  "Search for current term in browser with French Linguee.com."
  ;;TODO: handle all language pairs
  (interactive)
  (let* ((query (plist-get wordreference-results-info 'term))
         (query-split (split-string query " "))
         (query-final (if (not (> (length query-split) 1))
                          query
                        (string-join query-split "+"))))
    (browse-url-generic (concat
                         "https://www.linguee.com/english-french/search?il=EN&tool=opensearch&query="
                         query-final))))


;;;###autoload
(defun wordreference-search (&optional prefix word source target)
  "Search wordreference for region, `word-at-point', or user input.
Optionally specify WORD, SOURCE and TARGET languages.
With a PREFIX arg, prompt for source and target language pair."
  (interactive "P")
  (let* ((source (or source ;from lisp
                     (if prefix ;prefix arg
                         (completing-read "From source: "
                                          wordreference-languages-full
                                          nil
                                          t)
                       (or
                        ;; prev search
                        (wordreference-get-results-info-item 'source)
                           wordreference-source-lang)))) ; fallback
         (target (or target
                     (if prefix
                         (completing-read "To source: "
                                          wordreference-languages-full
                                          nil
                                          t)
                       (or (wordreference-get-results-info-item 'target)
                           wordreference-target-lang))))
         (region (if (equal major-mode 'pdf-view-mode)
                     (when (region-active-p)
                       (pdf-view-active-region-text))
                   (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end)))))
         (word (or word
                   (read-string (format "Wordreference search (%s): "
                                        (or region (current-word) ""))
                                nil nil (or region (current-word))))))
    (wordreference-print-translation-buffer
     word
     (wordreference--retrieve-parse-html word source target)
     source
     target)))

(define-derived-mode wordreference-mode special-mode "wordreference"
  :group 'wordreference
  (read-only-mode 1))

(provide 'wordreference)
;;; wordreference.el ends here
