;; wordreference.el  -*- coding: utf-8-emacs; -*-

;; nb: wr requires a user agent to return a request

(defvar wordreference-source-lang "fr"
  "Default source language.")

(defvar wordreference-target-lang "en"
  "Default target language.")

(defun wordreference--retrieve-parse-html (word)
  "Query wordreference.com for WORD, and parse the HTML response."
  ;; (interactive)
  (let* ((url (format "https://www.wordreference.com/%s%s/%s"
                      wordreference-source-lang
                      wordreference-target-lang
                      word))
	 (html-buffer (url-retrieve-synchronously url)))
         ;; (html-parsed
    (with-current-buffer html-buffer
      (goto-char (point-min))
      (libxml-parse-html-region
       (search-forward "\n\n") (point-max)))));))

;;     (with-current-buffer (get-buffer-create "*wr-pased*")
;;       (print html-parsed (current-buffer))
;;       (setq wr-tables (wordreference--get-tables html-parsed))
;;       ;; (setq wr-all-tds (wordreference--get-all-tds wr-tables))
;;       (setq wr-word-tables
;;             (wordreference--get-word-tables wr-tables))
;;       (setq wr-single-table
;;             (cadr wr-word-tables)))))
;; ;; shd hold two tables: principle and additional translations
;; (supplementaires-table

(defun wordreference--get-tables (dom)
  "Get tables from parsed HTML response DOM."
  (dom-by-tag dom 'table))

(defun wordreference--get-word-tables (tables)
  (let ((word-tables))
    (mapcar (lambda (x)
              (when (equal (dom-attr x 'class) "WRD")
                (push x word-tables)))
            tables)))

(defun wordreference--build-tds-text-list (tr)
  "Returns a list of results of both source and target langs."
  (let ((tds (dom-by-tag tr 'td)))
    (mapcar (lambda (x)
              ;; (if (not (dom-by-class x "ToWrd"))
              (split-string (dom-texts x) "  " t " "))
            tds)))

;; (wordreference--build-tds-text-list wr-single-tr)

(defun wr-collect-trs-results-list (trs)
  ""
  ;; (let ((pricipales-table (cadr (wordreference--get-word-tables wr-tables)))
  ;;       (trs (wordreference--get-trs pricipales-table)))
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

;; (cadddr wr-word-tables)

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
          pos-types))

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

  ;; (let ((pos-abbrev "vtr tester")) ;(wr-get-pos-abbrev test-str)))
    ;; (when (member test-str def)
      ;; (setcdr (member test-str def) nil)
      ;; (setcar (member test-str def) pos-abbrev)
      ;; )))

(defvar wr-pos-replacement-table
  '(("vtr verbe transitif" . "vtr verbe transitif : verbe qui s'utilise avec un complément d'objet direct (COD).")
    ("vtr phrasal sep phrasal verb, transitive, separable" . "vtr phrasal sep phrasal verb, transitive, separable : Verb with adverb(s) or preposition(s), having special meaning, divisible--for example, \"call off\" [=cancel], \" call")))

(wr-replace-pos-in-def wr-single-def-phr "vtr phrasal sep phrasal verb, transitive, separable : Verb with adverb(s) or preposition(s), having special meaning, divisible--for example, \"call off\" [=cancel], \" call") 

(defun wr-get-pos-abbrev (pos-str)
  ""
  (split-string 
     
  ;; (member 
;; (caddr wr-single-def)
;; (cddr wr-trs-results-list)

;; (wr-print-trs-results wr-trs-results-list)

   ;; (butlast 

                
(length wr-tables)
(dom-by-class 'class)
;; to print a table in a buffer:
;; (insert
;;  (prin1-to-string
;;   (cadr wr-word-tables))
 
(wordreference--get-table-children (cadr wr-word-tables))

(wordreference--get-table-children wr-single-table)

(wordreference--get-trs wr-single-table)

;; works but useless
(setq wr-single-table-texts
      (dom-texts wr-single-table))

;; returns all tr nodes:
(setq wr-single-table-trs
      (wordreference--get-single-tr wr-single-table))

;; returns only 1 td node:
(dom-children wr-single-table-children)

(setq wr-single-table-children-tds
      (mapcar (lambda (x)
                (dom-by-tag wr-single-table-children 'td))
              wr-single-table-children))
(caadr wr-single-table-children-tds)

(wordreference--get-tds (cdr wr-word-tables))
(wordreference--get-trs (cadr wr-word-tables))

(wordreference--get-all-tds (cadr wr-word-tables))
(wordreference--get-table-text (cadr wr-word-tables))

;;works
(defun wordreference--get-trs (word-table)
  (dom-children word-table))

(setq wr-single-tr
      (caddr (wordreference--get-trs wr-single-table)))

(defun wordreference--get-tds-text (tr)
  (dom-children tr))

(wordreference--build-tds-text-list wr-single-tr)

(defun wr-collect-trs-results ()
  ""
  (let ((table (
        (trs (wordreference--get-trs table)))
    (mapcar (lambda (tr)
              (wordreference--build-tds-text-list tr))
            trs))
          
;; (("test" "nm nom masculin : s'utilise avec les articles" "\"le\", \"l'\"" "(devant une voyelle ou un h muet)," "\"un\" ." "Ex : garçon - nm > On dira \" le" "garçon\" ou \" un" "garçon\".") ("(essai)") ("test, trial" "n noun : Refers to person, place, thing, quality, etc."))

(wr-get-pos-from-entry (car wr-single-def))

(car
 (wordreference--build-tds-text-list wr-single-tr)))

(defun wr-fix-entry (tr)
  (let ((tds-texts
         (wordreference--build-tds-text-list tr)))
    (mapcar (lambda (x)
              ;; (when (length 1 x)
              ;; (append 
              (concat (car x)
                      " "
                      (wr-get-pos-from-entry x)))
             tds-texts)))

(defun wr-fix-entries-map (table)
  (let ((wr-trs (wordreference--get-trs table)))
    (mapcar (lambda (x)
              (wr-fix-entry x))
            wr-trs)))

(setq wr-fixed-trs
      (wr-fix-entries-map wr-single-table))

(defun wr-fixed-to-vector (fixed-trs)
  (mapcar (lambda (x)
            ;; all vectors gotta be full length for tab list:
            (while (> 3 (length x))
              (setq x (append x '(""))))
            (vconcat x))
          fixed-trs))

(setq wr-vector-trs
      (wr-fixed-to-vector wr-fixed-trs))
;; prob with this is that eg sentences are in col 2, and definitions in col 3
;; so the defintion is too far from search term

(wr-get-pos-from-entry
(car
(wordreference--build-tds-text-list wr-single-tr)))

(defun wordreference--build-single-td-text-string (td)
  (dom-texts td
(caddr wr-single-tr)


(dom-by-tag tr 'td

(wordreference--get-single-td wr-single-tr)

;; works finally:
(setq wr-all-tds
      (dom-by-tag (cadr wr-word-tables) 'td))
;; (dom-by-tag wr-single-table 'td))
;; (length wr-all-tds)
;; (wordreference-get-tds-from-word-table wr-single-table)

(defun wordreference-tds-from-word-table (table)
  (dom-by-tag table 'td))

(length
(wordreference-tds-from-word-table wr-single-table))

(dom-texts (nth 4 wr-all-tds))
(dom-texts (nth 6 wr-all-tds))

(defun wordreference-texts-from-all-tds (tds)
  (mapcar (lambda (x)
            (dom-texts x " |?|"))
          wr-all-tds))

;; text of a single table "Principle translations:
;; i.e. need to go one level lower still:
(setq wr-single-text
(wordreference-texts-from-all-tds
 (wordreference-tds-from-word-table wr-single-table)))


(dom-by-class wr-single-table "ToWrd")

;; (wordreference-get-tds-from-word-table wr-single-table) ",")

;; doesn't
;; (defun wordreference--get-tds (table)
  ;; (dom-children (wordreference--get-table-children table)))

;; (defun wordreference--get-trs (table)
  ;; (dom-children table))

;; doesn't
(defun wordreference--get-all-tds (tables)
  (mapcar (lambda (x)
            (wordreference--get-tds x))
          tables))
;; (setq wr-all-tds (wordreference--get-all-tds wr-tables))

(defun wordreference--get-td-text (table)
  (dom-texts (dom-children table)))

(defun wordreference--get-tr-text (tr)
  (dom-texts (dom-children tr)))


;; (setq wordref-table-text-2
;;       (wordreference--get-tr-text
;;        (wordreference--get-table-children
;;         (nth 1 wordref-word-tables))))

(defun wordreference--get-table-text (word-tables)
  (dom-texts (nth 1 word-tables)))

(defun wordreference--get-main-trans (tables)
  (let ((dom-by-tag tables 'table))))

(defun wr-make-tabulated-headers (column-names rows)
  "column width are calculated by picking the max width of every cell under that
column + the column name"
  (let ((widths
         (-reduce-from
          (lambda (acc x)
            (-zip-with
             (lambda (l r)
               (max l (length r)))
             acc (append x '())))
          (-map #'length columns-names)
          rows)))
    (cl-map
     #'vector #'identity
     (-zip-with
      (lambda (col size) (list col size nil))
      columns-names widths))))

(defun wr-display-table-in-buffer (columns-names rows)
  (let ((headers (wr-make-tabulated-headers columns-names rows))
        (bname "*display table*"))
    (with-current-buffer (get-buffer-create bname)
      ;; (erase-buffer)
      (tabulated-list-mode)
      (setq tabulated-list-format headers)
      (setq tabulated-list-padding 2)
      (tabulated-list-init-header)
      (setq tabulated-list-entries
            (-zip-with
             (lambda (i x) (list i x))
             (-iterate '1+ 0 (length rows))
             rows))
      (tabulated-list-print t)
      (switch-to-buffer bname))))
      ;; (popwin:popup-buffer bname))))
 
(defvar wordreference-source-lang "fr"
  "Default source language.")

(defvar wordreference-target-lang "en"
  "Default target language.")

(defun wordreference-retrieve-data (word)
  (interactive)
  (let* ((url (format "https://www.wordreference.com/%s%s/%s"
                      wordreference-source-lang
                      wordreference-target-lang
                      word))
	 (buffer (url-retrieve-synchronously url))
	 definitions)
    (with-current-buffer buffer
      (switch-to-buffer (current-buffer))
      (while (search-forward-regexp 
	      "<h3>\\(.*? \\)</h3><ol.*?>\\(.*?\\)</ol>" nil t)
	    (setq definitions (cons (list (match-string 1) 
				                      (match-string 2))
				                definitions)))
      (reverse definitions))))
 
(defun wordreference-clear-buffer ()
  (interactive) 
  (clipboard-kill-region 1 (point-max))
  (goto-char (point-min)))

(defun wordreference-replace-string (str rep)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward str nil t)
      (replace-match rep))))

(defun wordreference-replace-regexp (regexp rep)
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match rep))))

(defun wordreference-define-word (word)
  (interactive "MWord: ")
  (let ((defs (wordreference-retrieve-data word))
        (inhibit-read-only t))
    (with-current-buffer (get-buffer-create "* wordreference *")
    ;; (split-window-vertically)
    ;; (windmove-down)
    ;; (switch-to-buffer "* wordreference *")
      (wordreference-clear-buffer)
      (goto-char (point-min))
      (save-excursion
        (mapc (lambda (s) 
	            (insert (format "%s\n%s\n\n" (car s) (nth 1 s) (current-buffer))))
	          defs))
      (recode-region (point-min) (point-max) 'utf-8-unix 'utf-8-unix)
      (wordreference-replace-string "<br>" "\n")
      (wordreference-replace-regexp "[ ]*<li>[ ]*" "\n* ")
      (wordreference-replace-regexp "<span.*?class=i.*?>\\(.*?\\)</span>" "\t\\1")
      (wordreference-replace-regexp "<span.*?>\\(.*?\\)</span>" "\\1"))
    (switch-to-buffer-other-window "* wordreference *")
  nil))


(defun wordreference-define-current-word ()
  (interactive)
  (wordreference-define-word (current-word)))
