;; wordreference.el  -*- coding: utf-8-emacs; -*-

;; nb: wr requires a user agent to return a request

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
  "Returns a list of results of both source and target langs."
  (let ((tds (dom-by-tag tr 'td)))
    (mapcar (lambda (x)
              ;; (if (not (dom-by-class x "ToWrd"))
              (split-string (dom-texts x) "  " t " "))
            tds)))

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
      (erase-buffer)
      (special-mode)
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
      (goto-char (point-min)))))


;; PRINTING:

(defun wr-print-heading (heading)
  ""
  (insert (propertize heading
                      'face '((t :inherit font-lock-function-name-face
                                 :weight bold)))
          "\n\n"))

(defun wr-print-trs-results (trs)
  ""
  (let* ((table-name (caaar trs))
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
  (mapcar (lambda (def)
            (wr-print-single-definition def))
          defs))

(defun wr-get-pos-from-entry (entry)
  ""
  ;FIXME: doesn't work if we have a phrasal expression!
  (when (< 1 (length entry))
    (let ((pos (cadr entry)))
      (car (split-string pos)))))

(defun wr-print-single-definition (def)
  ""
  (let* ((source (car def))
         (source-term (caar def))
         (pos (cdar def)) ;; TODO: cull this
         ;; (pos-first-word (car (split-string pos " ")))
         (context (cadr def))
         (context-term (car context))
         (target (caddr def))
         ;; (target-pos-cull (wr-replace-pos-in-def target))
         (target-term (car target)))
    (setq wr-pos pos)
    (insert
     (concat
      (when source-term
        (propertize source-term
                    'face '((t :inherit success :weight bold))))
      " "
      (propertize (or (wr-get-pos-from-entry source)
                      "")
                  'face font-lock-comment-face)
      " "
      context-term
      "\n"
      ;; (wr-print-source-or-target-str-rest (cdr source))
      " " ;; "\n"
      ;; pos-first-word
      ;; (propertize
      ;; 'face font-lock-comment-face)
      "\n           "
      (propertize "--> "
                  'face font-lock-comment-face)
      (when target-term
        (propertize target-term
                    'face '((t :inherit success :weight bold))))
      " "
      (propertize (or (wr-get-pos-from-entry target)
                      "")
                  'face font-lock-comment-face)
      " "
      ;; (wr-print-source-or-target-str-rest (cdr target)) ;-term
      "\n"))))

(defun wr-print-source-or-target-str-rest (str-list)
  ""
  ;; (let ((str-list (wr-abbrev-all-pos-in-def str-list wr-all-pos-abbrev-types)))
    (concat
     (propertize
      (mapconcat 'identity str-list " ")
      'face font-lock-comment-face)));)

(defun wr-abbrev-all-pos-in-def (str-list pos-types)
  ""
  (let (new-str-list)
    (mapcar (lambda (x)
              (push
               (wr-abbrev-single-pos-in-def x pos-types)
               new-str-list))
            str-list)
    (reverse new-str-list)))

(defun wr-abbrev-single-pos-in-def (str pos-types)
  ""
  (let (new-str-list)
    (mapcar (lambda (x)
            (if (string-prefix-p x str)
                x
              str))
          pos-types)))

(defvar wr-all-pos-abbrev-types
  '("nm nom masculin"
    "vtr verbe transitif"
    "vi verbe intransitif"
    "loc adj locution-adjectivale"
    
    "vtr transitive verb"
    "adj adjective"
    "n noun"
    "vtr phrasal sep phrasal verb, transitive, separable"
    ))

(defvar wr-pos-replacement-table
  '(("vtr verbe transitif" . "vtr verbe transitif : verbe qui s'utilise avec un complément d'objet direct (COD).")
    ("vtr phrasal sep phrasal verb, transitive, separable" . "vtr phrasal sep phrasal verb, transitive, separable : Verb with adverb(s) or preposition(s), having special meaning, divisible--for example, \"call off\" [=cancel], \" call")))

(defun wr-fix-entry (tr)
  (let ((tds-texts
         (wordreference--build-tds-text-list tr)))
    (mapcar (lambda (x)
              (concat (car x)
                      " "
                      (wr-get-pos-from-entry x)))
             tds-texts)))

(defun wr-fix-entries-map (table)
  (let ((wr-trs (wordreference--get-trs table)))
    (mapcar (lambda (x)
              (wr-fix-entry x))
            wr-trs)))

(defun wordreference-tds-from-word-table (table)
  (dom-by-tag table 'td))

(defun wordreference-texts-from-all-tds (tds)
  (mapcar (lambda (x)
            (dom-texts x " |?|"))
          wr-all-tds))

(defun wordreference--get-td-text (table)
  (dom-texts (dom-children table)))

(defun wordreference--get-tr-text (tr)
  (dom-texts (dom-children tr)))

(defun wordreference--get-table-text (word-tables)
  (dom-texts (nth 1 word-tables)))

(defun wordreference--get-main-trans (tables)
  (let ((dom-by-tag tables 'table))))
