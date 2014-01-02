;; Extensions to ox-md.el to suppport rdiscount (and eventually kramdown)

(eval-when-compile (require 'cl))
(require 'ox-md)


;;; User-Configurable Variables
(message "Loaded justin808 org-mode ox-md-ext")

(defgroup org-export-md-ext nil
  "Options specific to Markdown export with extentsions back-end (extends org-export-md)."
  :tag "Org Markdown Extended"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))


;;; Define Back-End

(org-export-define-derived-backend 'md-ext 'md
  :export-block '("MD" "MARKDOWN")
  ;; don't override filter -- modified one for 'md
  ;; :filters-alist nil ; '((:filter-parse-tree . org-md-ext-separate-elements))
  :menu-entry
  '(?e "Export to Markdown Extended (RDiscount)"
       ((?M "To temporary buffer"
	    (lambda (a s v b) (org-md-ext-export-as-markdown a s v)))
	(?m "To file" (lambda (a s v b) (org-md-ext-export-to-markdown a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-md-export-to-markdown t s v)
		(org-open-file (org-md-ext-export-to-markdown nil s v)))))))
  :translate-alist '(
		     (code . org-md-ext-verbatim)
		     (example-block . org-md-ext-example-block)
		     (fixed-width . org-md-ext-example-block)
		     ;; footnotes could be included later with kramdown support
		     (footnote-definition . ignore)
		     (footnote-reference . ignore)
		     (inline-src-block . org-md-ext-verbatim)
		     (item . org-md-ext-item)
		     (quote-block . org-md-ext-quote-block)
		     (quote-section . org-md-ext-example-block)
		     (src-block . org-md-ext-src-block)
		     (template . org-md-ext-template)
		     (verbatim . org-md-ext-verbatim)))
		     

;; 1. Start here with these elements
;; 2. Then create a sample markdown document to test what needs to change in terms of generation
;; 3. Then be sure all issues addressed in list


;;; Filters

;; (setq org-md-ext-no-separator-elems (list 'org-data 'item 'paragraph))

;; (defun org-md-ext-separate-elements (tree backend info)
;;   "Some elements must be separated by at least one blank line.

;; TREE is the parse tree being exported.  BACKEND is the export
;; back-end used.  INFO is a plist used as a communication channel.

;; Assume BACKEND is `md-ext'."
;;   ;; Need to choose which elements to add extra blank line
;;   (edebug)
;;   (org-element-map tree org-element-all-elements
;;     (lambda (elem)
;;       (unless (member (org-element-type elem) org-md-ext-no-separator-elems)
;; 	(org-element-put-property
;; 	 elem :post-blank
;; 	 (let ((post-blank (org-element-property :post-blank elem)))
;; 	   (if (not post-blank) 1 (max 1 post-blank)))))
;;       (if (eq (org-element-type elem) 'item)
;; 	(org-element-put-property elem :post-blank 0))
;;       ))
;;   ;; Return updated tree.
;;   tree)



;;; Transcode Functions

;;;; Code and Verbatim

(defun org-md-ext-verbatim (verbatim contents info)
  "Transcode VERBATIM object into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  ;; Let's get the source code type and set it
  ;; like ```ruby
  (let ((value (org-element-property :value verbatim)))
    (format (cond ((not (string-match "`" value)) "`%s`")
		  ((or (string-match "\\``" value)
		       (string-match "`\\'" value))
		   "`` %s ``")
		  (t "``%s``"))
	    value)))

;;;; Example Block

(defun org-md-ext-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "    "
   (org-remove-indentation
    (org-element-property :value example-block))))

;;;; Src Block

;; (defun org-md-ext-src-block (src-block contents info)
;;   "Transcode EXAMPLE-BLOCK element into Markdown format.
;; CONTENTS is nil.  INFO is a plist used as a communication
;; channel."
;;   (let ((language (org-element-property :language src-block))
;; 	(value (org-remove-indentation (org-element-property :value src-block))))
;;     (format "``` %s\n%s```" language value)))

(defun org-md-ext-src-block (src-block contents info)
  "Transcode EXAMPLE-BLOCK element into Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((language (org-element-property :language src-block))
	(value (org-remove-indentation (org-element-property :value src-block))))
    (format "{%% codeblock lang: %s %%}\n%s{%% endcodeblock %%}" language value)))




;;;; Item

(defun org-md-ext-item (item contents info)
  "Transcode ITEM element into Markdown format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "-"
		   (concat (number-to-string
			    (car (last (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct)))))
			   "."))))
    (concat bullet
	    (make-string (- 4 (length bullet)) ? )
	    (case (org-element-property :checkbox item)
	      (on "[X] ")
	      (trans "[-] ")
	      (off "[ ] "))
	    (let ((tag (org-element-property :tag item)))
	      (and tag (format "**%s:** "(org-export-data tag info))))
	    (org-trim
	     (replace-regexp-in-string "^\\([^`]\\)" "    \\1" contents))
	    ;; (org-element-property :type item)
	    ;; (number-to-string (org-element-property :post-blank item))
	    )))


;;;; Quote Block

(defun org-md-ext-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into Markdown format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))


;;; Interactive function

;;;###autoload
(defun org-md-ext-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'md-ext "*Org MD Ext Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


;;;###autoload
(defun org-md-ext-convert-region-to-md ()
  "Assume the current region has org-mode syntax, and convert it to Markdown.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'md-ext))


;;;###autoload
(defun org-md-ext-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'md-ext outfile async subtreep visible-only)))

;;;###autoload
(defun org-md-ext-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'md-ext filename ".md" plist pub-dir))

(provide 'ox-md-ext)

