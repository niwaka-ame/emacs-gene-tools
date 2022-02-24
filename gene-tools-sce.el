;;; gene-tools-sce.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Yu Huo
;;
;; Author: Yu Huo <https://github.com/niwaka-ame>
;; Maintainer: Yu Huo <yhuo@tuta.io>
;; Created: February 24, 2022
;; Modified: February 24, 2022
;; Version: 0.0.1
;; Keywords: convenience data
;; Homepage: https://github.com/niwaka-ame/emacs-gene-tools
;; Package-Requires: ((emacs "24.4"))
;; Maintainer: Yu Huo <yhuo@tuta.io>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
;;;
(require 'gene-tools)

(defconst gene-tools-sce-action-alist
  '(("deletion collection" . gene-tools-sce-query-deletion-primers)
    ("GFP collection" . gene-tools-sce-query-GFP-primers)
    ("query SGD" . gene-tools-sce-query-SGD)))
(defvar gene-tools-sce-name-to-oln-data nil)
(defvar gene-tools-sce-deletion-collection-data nil)
(defvar gene-tools-sce-GFP-collection-data nil)

(defun gene-tools-sce ()
  "Major UI for Saccharomyces cerevisiae."
  ;;(interactive)
  (let* ((options (mapcar #'car gene-tools-sce-action-alist))
         (choice (completing-read
                  "Choose action: "
                  options)))
    (funcall (cdr (assoc choice gene-tools-sce-action-alist)))))

(defun gene-tools-name-to-oln ()
  "Translate gene name to OLN."
  ;; lazy load
  (unless gene-tools-sce-name-to-oln-data
    (gene-tools-sce--load-name-to-oln-data))
  (let* ((table gene-tools-sce-name-to-oln-data)
         (gene-candidates (hash-table-keys table))
         (choice (completing-read
                  "Enter gene name: "
                  gene-candidates
                  #'(lambda (string) (not (string= "HEAD" string)))
                  t)))
    (gene-tools-gethash choice "OLN" table)))


(defun gene-tools-sce-query-deletion-primers ()
  "Return all info of deletion primers given OLN."
  ;; (interactive)
  ;; lazy load
  (unless gene-tools-sce-deletion-collection-data
    (gene-tools-sce--load-deletion-collection-data))
  (let ((table gene-tools-sce-deletion-collection-data)
        (oln (gene-tools-name-to-oln)))
    (gene-tools--pop-window (gene-tools-gethash oln "A_confirmation_primer_sequence" table))))

(defun gene-tools-sce-query-GFP-primers ()
  "Return all info of GFP primers given OLN."
  ;; (interactive)
  ;; lazy load
  (unless gene-tools-sce-GFP-collection-data
    (gene-tools-sce--load-GFP-collection-data)))

(defun gene-tools-sce-query-SGD ()
  "Query gene info of OLN on SGD."
  (require 'sgd-lookup))

(defun gene-tools-sce--load-name-to-oln-data ()
  "Load name to OLN data."
  (setq gene-tools-sce-name-to-oln-data (gene-tools-read-into-hash
                                         "./data/sce/yeast.txt"
                                         "[ ]+" 56 '("Name" "OLN") nil '(57 58))))
(defun gene-tools-sce--load-deletion-collection-data ()
  "Load deletion collection data."
  (setq gene-tools-sce-deletion-collection-data (gene-tools-read-into-hash
                                                 "./data/sce/Deletion_primers_PCR_sizes.txt"
                                                 "\t" nil nil 1 '(2))))

(defun gene-tools-sce--load-GFP-collection-data ()
  "Load GFP collection data.")


(provide 'gene-tools-sce)
;;; gene-tools-sce.el ends here
