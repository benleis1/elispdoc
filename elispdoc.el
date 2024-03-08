;;; elispdoc.el --- Package to transform elisp to markdown code documentation  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2024 Benjamin Leis

;; Author: Benjamin Leis <benleis1@gmail.com>
;; Created: 5 Mar 2024
;; Version: 0.1
;; Keywords: tools
;; Homepage: https://github.com/benleis1/elispdoc
;; Package-Requires: (markdown-toc) (org-make-toc)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Simple tool to provide a javadoc like pipeline for elisp files that outputs a markdown file
;; made out of comments with interspersed code blocks
;;
;; This is intended as an alternative to writing a config file in an org file and tangling it
;; where you want the comments and code to be first class but also want to publish documentation
;; built from it. Embed any markdown/org you want in comments and it will render properly in the
;; transformed doc buffer.
;;
;; Usage:  eld-process-elisp-to-doc-buffer (buffername)
;; A new buffer will be created with the transformed code within it.
;;
;; To determine the output format customize elispdoc-syntax.  Its set to markdown by default.
;;
;; Transforms done:
;;  - all code is blocked together
;;  - comment prefixes `;` are removed
;;  - triple comments  `;;;` are turned into headers
;;  - Emacs variables filtered on the first line
;;  - Subheaders added for a functions
;;  - TOC inserted after Code: comment
;;
;; - TODO: add variable support in headers and sort option for toc
;;
;;; Code:

;; ## Customizable variables

;; Turn automatic function subheaders on or off.
(defvar elispdoc-include-function-headers t  "boolean indicating whether to add a subheader for each function")

;; Turn table of contents generation on or off.
(defvar elispdoc-include-toc t  "boolean indicating whether to add a table of contents after Code: section")

;; Map of document mode tokens
(setq elispdoc-markdown-syntax '((flavor . markdown) (begincode . "```") (endcode . "```")
			    (header . "#") (subheader . "##") (beginquote . ">")))

(setq elispdoc-org-syntax '((flavor . org) (begincode . "#+BEGIN_SRC lisp") (endcode . "#+END_SRC")
		       (header . "*") (subheader . "**") (beginquote . "#+BEGIN_QUOTE\n" )
		       (endquote . "#+END_QUOTE")))

;; Allow the user to pick between markdown, org and any future doc flavors
(defvar elispdoc-syntax elispdoc-markdown-syntax "Mapping used to determine the flavor of the output doc file")

;; Jump to the start of the next sexp and return t if successful
(defun eld--move-start-next-sexp ()
  (let ((start (point)))
    (forward-sexp)
    (if (or (eq (point) start) (eobp))
	nil
      (progn 
	(backward-sexp)
	t))))

;; Return line of next sexp or nil if there isn't one but doesn't move the cursor
(defun eld--line-next-sexp ()
  (save-excursion
    (if (eld--move-start-next-sexp) (line-number-at-pos) nil)))

;; Return point of next sexp or nil if there isn't one but doesn't move the cursor
(defun eld--point-next-sexp ()
  (save-excursion
    (if (eld--move-start-next-sexp) (point) nil)))

;; Return point at end of next sexp or nil if there isn't one but doesn't move the cursor
(defun eld--end-point-next-sexp ()
  (save-excursion
    (if (eld--move-start-next-sexp)
	(progn (forward-sexp)
	       (point))
      nil)))

;; Process the next sexp to find the function name if it exists
;; return nil if there isn't one
;; This is built on a simple regular expression
(defun eld--function-name-next-sexp ()
  (let ((bound (eld--end-point-next-sexp)))
    (save-excursion
      (if (re-search-forward "defun \\(.*?\\) " bound t)
	  (match-string 1) nil))))

;; Jump to the start of the next line with a comment or return null (hardcoded comment char)
(defun eld--move-start-next-comment () 
  ;;  (if (re-search-forward "^;" nil t)
  (if (forward-comment 1)
      (progn
	(forward-line -1)
	(beginning-of-line)
	t)
      nil)) 

;; Return line of next line that starts with a comment or nil if there isn't one but doesn't
;; move the cursor (hardcoded comment char)
(defun eld--line-next-comment ()
  (save-excursion
    (if (forward-comment 1)
	(progn
	  (forward-line -1)
	  (line-number-at-pos))
      nil)))

;; Return point of next line that starts with a comment or nil if there isn't one but doesn't
;; move the cursor (hardcoded comment char)
(defun eld--point-next-comment ()
  (save-excursion
    (if (forward-comment 1)
	(progn
	  (forward-line -1)
	  (point))
      nil)))

;; Add a begincode quote before the current line
(defun eld--begin-code ()
  (beginning-of-line)
  (insert (alist-get 'begincode elispdoc-syntax) "\n"))

;; Add am endcode quote before the current line
(defun eld--end-code ()
  (beginning-of-line)
  (insert (alist-get 'endcode elispdoc-syntax) "\n"))

;; Return the point at the end of the buffer
(defun eld--point-at-end-of-buffer ()
  (save-excursion
    (end-of-buffer)
    (point)))

;; Uncomment anything between current position and next sexp
;; leaving the point at the end of this region
;; This relies on the built in uncomment-region function
(defun eld--uncomment-noncode-block ()
  (interactive)
  (let ((start (point))
	(comment-pos (eld--point-next-comment))
	(code-pos (or (eld--point-next-sexp) (eld--point-at-end-of-buffer))))

    (when (and comment-pos (< comment-pos code-pos))
      (progn
	(goto-char (- code-pos 1))
	(uncomment-region start code-pos)))))

;; Add a header comment name at the first blank line above it
;  that is also after the start point
;; if no blank line is found insert a new line at start
(defun eld--add-header-to-function (name start)
  (save-excursion
    (eld--move-start-next-sexp)
    (if (re-search-backward "^$" start t)
	(insert "\n;;" (cdr (assoc 'subheader elispdoc-syntax)) " " name)
      (progn
	(goto-char start)
	(beginning-of-line)
	(insert "\n;;" (cdr (assoc 'subheader elispdoc-syntax)) " " name "\n")))))

;; Process and move past the next sexp
;;  - Add a function header if needed
;;  - Uncomment anything before the code block begins and then
;;  -quote the next code block and leave the cursor at the end of it
;; return t if a block was found o/w nil
(defun eld--transform-code-block ()

  ;; add function headers if enabled
  (when elispdoc-include-function-headers
    (let ((nextfun (eld--function-name-next-sexp)))
      (when nextfun
	(eld--add-header-to-function nextfun (point)))))
  
  (eld--uncomment-noncode-block)
  (if (eld--move-start-next-sexp)
      (progn
	(eld--begin-code)
	(forward-sexp)

	;; advance past any code that is before the next comment
	;; when adding function headers also stop at the next defun
	(while (and (eld--line-next-sexp)
		    (or (not elispdoc-include-function-headers) (not (eld--function-name-next-sexp)))
		    (or (not (eld--line-next-comment))
			(< (eld--line-next-sexp) (eld--line-next-comment))))
	  (forward-sexp))

	;; Add end quotes
	(forward-line)
	(eld--end-code)
	t)
    ;; else
    nil))

;; Quote all the code blocks in a buffer
(defun eld--transform-all-code-blocks ()
  (save-excursion
    (beginning-of-buffer)
    (while (eld--transform-code-block) t)
    ;; Deal with trailing comments at the end of the buffer
    (eld--uncomment-noncode-block)))

;; Replace a regular expression on the current line
(defun eld--regex-replace-on-line (exp replace)
  (save-excursion
   (beginning-of-line)
   (let ((start (point))
	 (end ()))
     (forward-line)
     (setq end (point))
     (replace-regexp-in-region exp replace start end))))

;; Wrapper for using org-make-toc
(defun eld--add-toc-org ()
  ;; directly add the template
  (insert
   ":PROPERTIES:\n"
   ":TOC:      :include descendants :depth 2\n"
   ":END:\n"
   ":CONTENTS:\n"
   ":END:\n")
  (org-make-toc))
  
;; setup a table of contents using markdown-toc
;; Placed after Code: comment
(defun eld--add-toc ()
  (beginning-of-buffer)
  (if (re-search-forward (concat "^" (alist-get 'header elispdoc-syntax) "\s?Code:") nil t)
      (progn
	(message "adding toc")
	(forward-line)
	(let ((flavor (alist-get 'flavor elispdoc-syntax)))
	  (cond ((eq flavor 'markdown) (markdown-toc-generate-toc))
		((eq flavor 'org) (eld--add-toc-org))
		(t (message "No mapping for doc flavor %s" (symbol-name flavor)))
		)))))

;; Main user command to generate the processed buffer
(defun elispdoc-process-elisp-to-doc-buffer (bufname)
  "Transform the current elisp buffer into a new markdown doc buffer"
  (interactive "sNew buffer name:")
  (generate-new-buffer bufname)
  (copy-to-buffer bufname nil nil)
  (with-current-buffer bufname
    (progn
      (lisp-mode)
      ;; remove any emacs file variables on the first line.
      (eld--regex-replace-on-line "\-\*\-.*\-*\-" "")
      ;; Convert any  triple comments to markdown headers 
      (replace-regexp-in-region "^;;;" (concat ";;" (cdr (assoc 'header elispdoc-syntax))) 1 nil)
      ;; Do the regular transform - quoting code and removing comments
      (eld--transform-all-code-blocks)
      (end-of-buffer)
      ;; Add a note at the end
      (insert "\n" (alist-get 'beginquote elispdoc-syntax)
	      " This file was auto-generated by elispdoc.el")
      (when  (alist-get 'endquote elispdoc-syntax)
	(insert "\n"  (alist-get 'endquote elispdoc-syntax)))

      (let ((flavor (cdr (assoc 'flavor elispdoc-syntax))))
	(cond ((eq flavor 'markdown) (markdown-mode))
	      ((eq flavor 'org) (org-mode))
	      (t (error (concat "unsupported document flavor: "
				(symbol-name (cdr (assoc 'flavor elispdoc-syntax))))))))
      ;; Insert a toc after Code: if specified
      (when elispdoc-include-toc
	(eld--add-toc)))))



