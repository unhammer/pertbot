(require 'iso-639)

(defvar pertbot-trunk-pairs
  '("apertium-br-fr" "apertium-ca-it" "apertium-cy-en" "apertium-en-ca" "apertium-en-es" "apertium-en-gl" "apertium-eo-ca" "apertium-eo-en" "apertium-eo-es" "apertium-eo-fr" "apertium-es-an" "apertium-es-ast" "apertium-es-ca" "apertium-es-gl" "apertium-es-it" "apertium-es-pt" "apertium-es-ro" "apertium-eu-es" "apertium-fr-ca" "apertium-fr-es" "apertium-is-en" "apertium-mk-bg" "apertium-mk-en" "apertium-nn-nb" "apertium-oc-ca" "apertium-oc-es" "apertium-pt-ca" "apertium-pt-gl" "apertium-sh-mk" "apertium-sv-da" "apertium-tr-az" "apertium-tr-ky"))
(defvar pertbot-staging-pairs
  '("apertium-af-nl" "apertium-ca-sc" "apertium-fr-pt" "apertium-mt-he" "apertium-pl-cs" "apertium-sme-nob"))
(defvar pertbot-nursery-pairs
  '("apertium-bg-en" "apertium-ca-ro" "apertium-da-en" "apertium-da-nb" "apertium-en-af" "apertium-en-pt" "apertium-eo-ru" "apertium-es-cs" "apertium-es-ssp" "apertium-fo-is" "apertium-fr-it" "apertium-ga-gd" "apertium-is-sv" "apertium-it-pt" "apertium-no-en" "apertium-pl-uk" "apertium-ro-it" "apertium-sl-es" "apertium-sme-fin" "apertium-sme-sma" "apertium-sme-smj" "apertium-sv-en" "apertium-tg-fa" "apertium-ur-hi"))
(defvar pertbot-incubator-pairs
  '("apertium-ru-cu" "apertium-es-ia" "apertium-pl-lt" "apertium-en-ga" "apertium-la-it" "apertium-pl-csb" "apertium-sw-rn" "apertium-en-mt" "apertium-ku-fa" "apertium-eo-cs" "apertium-ky-en" "apertium-eu-hu" "apertium-oc-fr" "apertium-cv-tr" "apertium-ru-en" "apertium-af-nl" "apertium-eo-ru" "apertium-en-de" "apertium-id-ms" "apertium-gn-es" "apertium-de-nl" "apertium-en-sq" "apertium-pl-eo" "apertium-fr-ro" "apertium-pl-hsb" "apertium-zu-xh" "apertium-sme-nob" "apertium-ro-rup" "apertium-pl-lv" "apertium-en-sco" "apertium-en-it" "apertium-fi-en" "apertium-en-lt" "apertium-bg-ru" "apertium-pl-sk" "apertium-eus-sme" "apertium-sw-en" "apertium-cv-ru" "apertium-bg-el" "apertium-fr-nl" "apertium-mr-hi" "apertium-la-en" "apertium-hu-en" "apertium-fr-no" "apertium-en-fr" "apertium-ht-en" "apertium-pl-dsb" "apertium-sl-mk" "apertium-sl-it" "apertium-bn-hi" "apertium-en-nl" "apertium-ne-en" "apertium-fo-nb" "apertium-de-en" "apertium-da-nb" "apertium-eu-en" "apertium-ga-gv" "apertium-sme-fin" "apertium-la-es" "apertium-ur-fa" "apertium-kv-ru" "apertium-en-hi" "apertium-pl-ru" "apertium-ca-sc" "apertium-sk-en" "apertium-tg-fa" "apertium-vi-en" "apertium-sc-pt" "apertium-mfe-en" "apertium-bn-en" "apertium-tr-en" "apertium-et-en" "apertium-mk-en" "apertium-cs-en" "apertium-tgl-ceb" "apertium-el-en" "apertium-fi-et" "apertium-br-cy" "apertium-si-en" "apertium-cs-sl" "apertium-ur-pa" "apertium-lt-lv" "apertium-eu-fr" "apertium-en-pl" "apertium-en-lv" "apertium-mk-sq" "apertium-sl-en" "apertium-br-es" "apertium-ru-uk" "apertium-eo-sk" "apertium-cy-es" "apertium-sme-sma" "apertium-zh_CN-zh_TW" "apertium-tt-kk" "apertium-da-fo" "apertium-sa-XX" "apertium-ro-en" "apertium-cs-sk"))

(defun pertbot-allpairs ()
  (append pertbot-trunk-pairs pertbot-staging-pairs pertbot-nursery-pairs pertbot-incubator-pairs))

(defvar pertbot-lang-pairs-cached nil)
(defun pertbot-lang-pairs ()
  (if pertbot-lang-pairs-cached
      pertbot-lang-pairs-cached
    (setq pertbot-lang-pairs-cached
	  (mapcar (lambda (p) (when (string-prefix-p "apertium-" p)
				(substring p (length "apertium-"))))
		  (pertbot-allpairs)))))

(defvar pertbot-lang-pairs-rev-cached nil)
(defun pertbot-lang-pairs-rev ()
  (if pertbot-lang-pairs-rev-cached
      pertbot-lang-pairs-rev-cached
    (setq pertbot-lang-pairs-rev-cached
	  (remove
	   nil
	   (mapcar (lambda (l) (when (string-match "\\(.*\\)-\\(.*\\)" l)
				 (concat (match-string 2 l) "-" (match-string 1 l))))
		   (pertbot-lang-pairs))))))

(defvar pertbot-lang-codes-cached nil)
(defun pertbot-lang-codes ()
  (if pertbot-lang-codes-cached
      pertbot-lang-codes-cached
    (setq pertbot-lang-codes-cached
	  (apply #'append
		 (mapcar (lambda (l) (when (string-match "\\(.*\\)-\\(.*\\)" l)
				       (list (match-string 2 l) (match-string 1 l))))
			 (pertbot-lang-pairs))))))


(defvar pertbot-langs-pairs-cached nil)
(defun pertbot-langs-pairs ()
  "Fill the hash table `pertbot-langs-pairs-cached', keys are
language names (cdr's of `iso-639-language-codes', or the code
itself if it's not in that list), and values are lists of the
language pairs for that language. Using `iso-639-language-codes'
means e.g. \"nob\" and \"nb\" will be treated the same."
  (if pertbot-langs-pairs-cached
      pertbot-langs-pairs-cached
    (setq pertbot-langs-pairs-cached (make-hash-table :test 'equal))
    (mapc
     (lambda (pair)
       (when (string-match "\\(.*\\)-\\(.*\\)" pair)
	 (let ((basename (concat "apertium-" pair))
	       (iso1 (or (cdr (assoc (match-string 1 pair) iso-639-language-codes))
			 (match-string 1 pair)))
	       (iso2 (or (cdr (assoc (match-string 2 pair) iso-639-language-codes))
			 (match-string 2 pair))))
	   (puthash iso1
		    (cons basename (gethash iso1 pertbot-langs-pairs-cached))
		    pertbot-langs-pairs-cached)
	   (puthash iso2
		    (cons basename (gethash iso2 pertbot-langs-pairs-cached))
		    pertbot-langs-pairs-cached))))
     (pertbot-lang-pairs))
    pertbot-langs-pairs-cached))

(defun pertbot-get-pairs-of-langcode (code)
  "Given a language code, return the language pairs of that
language."
  (gethash
   (or (cdr (assoc code iso-639-language-codes))
       code)
   (pertbot-langs-pairs)))

(defun pertbot-handle-pairs (msg)
  (if (string-match "^\\s *,pairs\\s +\\(\\S +*\\)\\s *\n" msg)
      (let* ((code (match-string 1 msg))
	     (pairs (pertbot-get-pairs-of-langcode code))
	     (pairstr (apply #'concat (mapcar (lambda (p)
						    (format "%s " p))
						  pairs)))
	     (lang (assoc code iso-639-language-codes)))
	(if pairs
	    (progn (if lang
		       (erc-send-message (format "The language %s appears in the following pairs:" (cdr lang)))
		     (erc-send-message "This language appears in the following pairs:"))
		   (erc-send-message pairstr))
	  (erc-send-message "Found no language pairs for that code.")))
    (erc-send-message "Usage: ,pairs CODE")
    (erc-send-message "where CODE is an apertium language pair code, typically a two- or three-letter ISO-639 language code.")))


(provide 'pertbot-apertium-pairs)