;;; ob-clingraph.el --- org-babel functions for generating graph visualizations using Potassco clingraph

;; Copyright (C) 2026 Werner Jäger

;; Author: Werner Jäger
;; Keywords: emacs, org-mode, org-babel, clingo, clingraph, answer-set-programming, graphwiz
;; URL: https://github.com/wejaeger/ob-clingraph
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for generating graph visualizations using Potassco clingraph
;;
;; Supported header arguments:
;; :n - optional number of answer sets to generate; 0 tells it to generate all answer sets, defaults to 1
;; :options - optional command line options, list available options by invoking `clingraph --help'
;; :instance - optional file name containing a problem instance

;; Example:
;;
;; #+name: vizualize
;; #+caption: Visualization
;; #+begin_src clingraph :stdin graph.lp :type digraph :results file graphics :file "g1.png"
;;  attr(graph_nodes, default, style, filled).
;; #+end_src
;;
;;; Requirements:
;;;   clingraph  | https://clingraph.readthedocs.io/en/latest/clingraph/installation.html
;;;
;;; Code:
(require 'ob)
(require 'ob-eval)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("clingraph" . "lp"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:clingraph
  '((:results . "output") (:exports . "both"))
  "Default arguments for evaluatiing a clingraph source block.")


;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:clinggraph' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:clingraph (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-clingraph nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-clingraph-var-to-clingraph (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp
;; Here the output to STDOUT will be captured and returned
(defun org-babel-execute:clingraph (body params)
  "Execute a block of Clingraph code with org-babel.
   This function is called by `org-babel-execute-src-block'"
  (message "executing Clingraph source code block")
  (let* ((processed-params (org-babel-process-params params))
	 ;; set facts parameter
	 (facts (cdr (assoc :facts processed-params)))

	 ;; set stdin parameter
	 (stdin (cdr (assoc :stdin processed-params)))

	 ;; set the out parameter
	 (out (cdr (assoc :out processed-params)))

	 ;; set the default-graph parameter
	 (default-graph (cdr (assoc :default-graph processed-params)))

	 ;; set the type parameter
	 (type (cdr (assoc :type processed-params)))

	 ;; set the viz-encoding parameter
	 (viz-encoding (cdr (assoc :viz-encoding processed-params)))

	 ;; set the options parameter
	 (options (cdr (assoc :options processed-params)))

	 ;; get out, default-graph and format options from `:file' header argument
	 (file (cdr (assoc :file processed-params)))
	 (out (if file "render" (if out out nil)))
	 (format (if file (file-name-extension file) "pdf"))
	 (default-graph (if file (file-name-base file) (if default-graph default-graph "default")))
	 (dir (if file  (cdr (assoc :output-dir processed-params)) "out"))

         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))

         ;; expand the body with `org-babel-expand-body:clingraph'
         (full-body (org-babel-expand-body:clingraph body params processed-params))

 	 ;; if `:viz-encoding' concatenate full-body and viz-encoding file to a new temp file
         (viz-encoding (when viz-encoding
           (let* (
                  (temp-viz-file (org-babel-temp-file "clingraph-"))
                 )
                 (with-temp-buffer  (insert full-body)
                                    (insert-file-contents viz-encoding)
                                    (write-region (point-min) (point-max) temp-viz-file))
		 temp-viz-file
           )))

	 ;; if `:viz-encoding' ensure, that `:output-dir' header argument is set
         (output-dir (when viz-encoding (or (cdr (assoc :output-dir  processed-params))
         (error "clingraph requires a \":output-dir\" header argument when \":viz-encoding\" is set."))))

	 ;; when `:viz-encoding' then determine 'select-model' and 'dir' options from `:output-dir'
         (select-model (when viz-encoding (file-name-base output-dir)))
         (dir (when viz-encoding (file-name-directory output-dir)))


         (temp-file (org-babel-temp-file "clingraph-"))
         (clingraph (executable-find "clingraph"))
         (cmd (concat
	              "cat " (org-babel-process-file-name temp-file) " | "
	              (shell-quote-argument (expand-file-name clingraph))
                      (when out (concat " --out " out))
                      (when default-graph (concat " --default-graph " default-graph))
                      (when dir (concat " --dir " dir))
                      (when type (concat " --type " type))
                      (when format (concat " --format " format))
		      (when viz-encoding (concat " --viz-encoding " viz-encoding))
		      (when select-model (concat " --select-model " select-model))
		      (when options (concat " " options))
		      (when facts (concat " " (org-babel-process-file-name facts)))
		      (when stdin (concat " " (org-babel-process-file-name stdin)))
	      )
	 )
        )

        ;; body: uses the bound variables
        (unless (file-executable-p clingraph)
          (error "Cannot find or execute `clingraph', please check it is installed and in PATH"))

        ;; execute the source-code block by dropping it to a temporary file
        ;; and evaluating the file.
        (with-temp-file temp-file (insert full-body))
        (message "%s" cmd)
        (org-babel-eval cmd "")
  )
)

(defun org-babel-clingraph-var-to-clingraph (var)
  "Convert an elisp var into a string of clingraph source code
   specifying a var of the same value."
  (format "%S" var))


(provide 'ob-clingraph)
;;; ob-clingraph.el ends here
