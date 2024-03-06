# elispdoc.el 

Copyright (C) 2010-2024 Benjamin Leis

Author: Benjamin Leis <benleis1@gmail.com>
Created: 5 Mar 2024
Version: 0.1
Keywords: tools
URL: https://github.com/benleis1/elispdoc

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Commentary:

Simple tool to provide a javadoc like pipeline for elisp files that outputs a markdown file
made out of comments with interspersed code blocks
This is intended as an alternative to writing a config file in an org file and tangling it
where you want the comments and code to be first class but also want to publish documentation
built from it. Embed any markdown you want in comments and it will render properly in the
transformed doc buffer.

Usage:  eld-process-elisp-to-doc-buffer (buffername)
A new buffer will be created with the transformed code within it.

Transforms done:
 - all code is blocked together
 - comment prefixes `;` are removed
 - triple comments  `;;;` are turned into headers
 - Emacs variables filtered on the first line 

TODO: add support for outputting as org as well

# Code:

Jump to the start of the next sexp and return t if successful
```
(defun eld-move-start-next-sexp ()
  (let ((start (point)))
    (forward-sexp)
    (if (or (eq (point) start) (eobp))
	nil
      (progn 
	(backward-sexp)
	t))))
```

Return line of next sexp or nil if there isn't one but doesn't move the cursor
```
(defun eld-line-next-sexp ()
  (save-excursion
    (if (move-start-next-sexp) (line-number-at-pos) nil)))
```

Return point of next sexp or nil if there isn't one but doesn't move the cursor
```
(defun eld-point-next-sexp ()
  (save-excursion
    (if (eld-move-start-next-sexp) (point) nil)))
```

Jump to the start of the next line with a comment or return null (hardcoded comment char)
```
(defun eld-move-start-next-comment () 
  (if (re-search-forward "^;" nil t)
      (progn
	(beginning-of-line)
	t)
      nil)) 
```

Return line of next line that starts with a comment or nil if there isn't one but doesn't
move the cursor (hardcoded comment char)
```
(defun eld-line-next-comment ()
  (save-excursion
    (if (re-search-forward "^;" nil t)
	(line-number-at-pos)
      nil)))
```

Return point of next line that starts with a comment or nil if there isn't one but doesn't
move the cursor (hardcoded comment char)
```
(defun eld-point-next-comment ()
  (save-excursion
    (if (re-search-forward "^;" nil t)
	(progn
	  (backward-char)
	  (point))
      nil)))
```

Add a markdown code quote before the current line
```
(defun eld-prequote-code ()
  (beginning-of-line)
  (insert "```\n"))
```

Return the point at the end of the buffer
```
(defun eld-point-at-end-of-buffer ()
  (save-excursion
    (end-of-buffer)
    (point)))
```

Uncomment anything between current position and next sexp
leaving the point at the end of this region
This relies on the built in uncomment-region function
```
(defun eld-uncomment-noncode-block ()
  (let ((comment-pos (eld-point-next-comment))
	(code-pos (or (eld-point-next-sexp) (eld-point-at-end-of-buffer))))

    (when (and comment-pos (< comment-pos code-pos))
      (progn
	(goto-char code-pos)
	(backward-char)
	(uncomment-region comment-pos code-pos)))))
```

Uncomment anything before the next code block and then
quote the next code block and leave the cursor at the end of it
return t if a block was found o/w nil
```
(defun eld-transform-code-block ()
  (eld-uncomment-noncode-block)
  (if (move-start-next-sexp)
      (progn
	(eld-prequote-code)
	(forward-sexp)
	;; advance past any code that is before the next comment
	(while (and (eld-line-next-sexp)
		    (or (not (eld-line-next-comment))
			(< (eld-line-next-sexp) (eld-line-next-comment))))
	  (forward-sexp))

	;; Add end quotes
	(forward-line)
	(eld-prequote-code)
	t)
    ;; else
    nil))
```


Quote all the code blocks in a buffer
```
(defun eld-transform-all-code-blocks ()
  (save-excursion
    (beginning-of-buffer)
    (while (eld-transform-code-block) t)
    ;; Deal with trailing comments at the end of the buffer
    (eld-uncomment-noncode-block)))
```

Replace on the current line
```
(defun eld-regex-replace-on-line (exp replace)
  (save-excursion
   (beginning-of-line)
   (let ((start (point))
	 (end ()))
     (forward-line)
     (setq end (point))
     (replace-regexp-in-region exp replace start end))))

(defun eld-process-elisp-to-doc-buffer (bufname)
  "Transform the current elisp buffer into a new markdown doc buffer"
  (interactive "sNew buffer name:")
  (generate-new-buffer bufname)
  (copy-to-buffer bufname nil nil)
  (with-current-buffer bufname
    (progn
      (lisp-mode)
      ;; remove any emacs file variables on the first line.
      (eld-regex-replace-on-line "\-\*\-.*\-*\-" ""))
      ;; Convert any  triple comments to markdown headers 
      (replace-regexp-in-region "^;;;" ";;#" 1 nil)
      ;; Do the regular transform - quoting code and removing comments
      (eld-transform-all-code-blocks)
      (end-of-buffer)
      ;; Add a note at the end
      (insert "\n> This file was auto-generated by elippdoc.el")
      (markdown-mode)))
```

> This file was auto-generated by elsipdoc.el
