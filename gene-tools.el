;;; gene-tools.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Yu Huo
;;
;; Author: Yu Huo <https://github.com/niwaka-ame>
;; Maintainer: Yu Huo <yhuo@tuta.io>
;; Created: February 24, 2022
;; Modified: February 24, 2022 Version: 0.0.1
;; Keywords: convenience data
;; Homepage: https://github.com/niwaka-ame/emacs-gene-tools
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;
(require 'cl-lib)
(require 'subr-x)

(defconst gene-tools-supported-organisms-alist
  '(("Saccharomyces cerevisiae" . gene-tools-sce)
    ("Escherichia coli" . gene-tools-eco))
  "Alist of supported organism in format (organism . library).")

(defconst gene-tools-common-action-alist nil
  "Alist of available actions for all organisms.")

(defcustom gene-tools-organisms-of-interest
  nil
  "List of organisms of interest. Only those will show up in the main menu."
  :type 'list)

(defun gene-tools ()
  "Major UI."
  (interactive)
  (let* ((options (mapcar #'car gene-tools-supported-organisms-alist))
         (choice (completing-read
                  "Choose organism: "
                  options)))
    (gene-tools--load-and-enter-organism choice)))

(defun gene-tools--load-and-enter-organism (orgn)
  "Load and enter a selected ORGN."
  (let ((source (cdr (assoc orgn gene-tools-supported-organisms-alist))))
    (progn
      (require source)
      (funcall source))))

(defun gene-tools-read-into-hash (file &optional sep header colnames keycol skiprows)
  "Read a data FILE into a hash table. Like pandas.read_csv() in Python."
  (let ((table (make-hash-table :test 'equal :size 10000))
        (sep (or sep "\t"))
        (header (or header 1))
        (keycol (or keycol 0)))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      ;; Go to the header line
      (forward-line (- header 1))
      ;; COLNAMES overrides HEADER argument.
      (if colnames
          (puthash "HEAD" (cl-coerce colnames 'vector) table)
        (puthash "HEAD" (gene-tools--read-line-at-pos sep) table))
      ;; Go forward and puthash line by line.
      (forward-line 1)
      (let ((line-number (count-lines (point-min) (point-max))))
        (while (<= (line-number-at-pos) line-number)
          ;; Puthash unless that row is to skip.
          (unless (cl-find (line-number-at-pos) skiprows :test #'=)
            (let* ((row (gene-tools--read-line-at-pos sep))
                   (key (elt row keycol)))
              (puthash key row table)))
          (forward-line 1))))
    table))

(defun gene-tools-gethash (key column table)
  "Read value from hash TABLE given KEY and COLUMN."
  (let ((colnames (gethash "HEAD" table))
        (row (gethash key table)))
    (elt row (cl-position column colnames :test 'string=))))

(defun gene-tools--read-line-at-pos (sep)
  "Read line at current position into a vector according to SEP."
  (let ((splitted-line
         (split-string (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))
                       sep)))
    (cl-map 'vector
            #'(lambda (string) (cl-remove ?\s string))
            splitted-line)))

(defun gene-tools--pop-window (string &optional height)
  "Show the gene info as STRING in a pop-up window with HEIGHT."
  (let ((sgd-buffer-name "*gene-tools-info*")
        (height (or height 0.2)))
    (get-buffer-create sgd-buffer-name)
    (with-current-buffer sgd-buffer-name
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert string)
      (org-mode))
    (display-buffer sgd-buffer-name
                    `(display-buffer-at-bottom . ((window-height . ,height))))))

(defun gene-tools--find-data-file (string)
  "Find curated data file in this repo."
  (let ((dir (file-name-directory
              (locate-library "gene-tools.el"))))
    (concat dir string)))


(provide 'gene-tools)
;;; gene-tools.el ends here
