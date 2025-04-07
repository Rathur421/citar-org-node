;;; citar-org-node.el --- Citar/org-node integration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024-2025 Arthur Varon
;;
;; Author: Arthur Varon <arthur.varon@gmail.com>
;; Created: February 12, 2024
;; Version: 0.5.1
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  A minor-mode to integrate 'citar' and the 'org-node'.
;;
;;  Provides functions for:
;;
;;  1. updating the 'citar' UI from the 'org-node' database
;;  2. using org-node to open or create notes
;;
;;; Code:
(require 'citar)
(require 'org-node)
(require 'cl-lib)

;;;###autoload
(defun citar-org-node-open-current-refs (&optional prefix)
  "Call `citar-open' on all citar cite keys in :ROAM_REFS: property of the node.
If PREFIX is given prompts to select one or more of the cite keys
before calling `citar-open' on them."
  (interactive "P")
  (if-let ((citar-open-prompt t)
           (node (org-node-at-point))
           (refs (citar-org-node-get-refs node)))
      (if prefix
          (citar-open (citar-select-refs :filter (lambda (key) (member key refs))))
        (citar-open refs))
    (message "No CiteRefs for this file")))

;;;###autoload
(defun citar-org-node-get-refs (node)
  "Return list of citekeys to use with citar from `NODE'.
This just remove the @ at the beginning of each refs in the node."
  (mapcar (lambda (str)
            (if (string-prefix-p "@" str)
                (substring original-ref 1)
              original-ref))
          (org-node-get-refs node)))

;;;###autoload
(defun citar-org-node-ref-add ()
  "Add a roam_ref to the node at point.
This is just a wrapper for `org-node-ref-add' with @ added at the beginning."
  (interactive nil org-mode)
  (let ((ref (citar-select-ref)))
    (org-node--add-to-property-keep-space "ROAM_REFS" (concat "@" ref))))


;;;###autoload
(defun citar-org-node--get-nodes-candidates (&optional citekeys)
  "Return a hash table mapping CITEKEYS to lists of org-node IDs.
If CITEKEYS is nil, map all citekeys found in org-node refs.
If CITEKEYS is non-nil, only include keys from that list.
Leading '@' in org-node refs are removed.
IMPORTANT: Only the *first* encountered node ID for each citekey is returned."
  (let ((results (make-hash-table :test 'equal)))
    (maphash
     (lambda (node-id node)
       (let ((original-refs (org-node-get-refs node)))
         (dolist (original-ref original-refs)
           ;; Normalize the ref ONCE by removing potential leading '@'
           (let ((normalized-ref (if (string-prefix-p "@" original-ref)
                                     (substring original-ref 1)
                                   original-ref)))
             ;; Check if this normalized ref should be included:
             (when (or (null citekeys)
                       (cl-member normalized-ref citekeys :test 'string=))
               ;; Only add if we haven't already found a node for this citekey.
               ;; TODO necessary for now but maybe org-node should filter ref node by default?
               (unless (gethash normalized-ref results)
                 ;; Store the node ID as a single-element list
                 (puthash normalized-ref (list node-id) results)))))))
     org-node--candidate<>node)
    results))


;;;###autoload
(defun citar-org-node--open-note (node-id)
  "Open the org-node associated with NODE-ID.
NODE-ID is the string identifier returned by the completion process."
  ;; Look up the node struct in the main hash table using the ID.
  (let ((node (gethash node-id org-node--candidate<>node)))
    (if node
        (org-node--goto node)
      ;; Handle the case where the ID might not be found (shouldn't normally happen)
      (warn "Org-node note with ID %s not found in org-node--candidate<>node" node-id))))


;;;###autoload
(defun citar-org-node-has-notes ()
  "Function to check if there's a note for each key in citar."
  (lambda (citekey)
    (citar-org-node--has-note-p citekey)))


;;;###autoload
(defun citar-org-node--has-note-p (key)
  "Look for note with associate KEY in org-node."
  (and (citar-org-node--get-node key) t))


;;;###autoload
(defun citar-org-node--get-node (key)
  "Find node associated with KEY.

TODO Take into account if key as not @?
TODO Take into account if multiple node for same refs?"
  (gethash (concat "@" key) org-node--candidate<>node))


;;;###autoload
(defun citar-org-node--new-file (key &optional _entry)
  "Create a file-level node for KEY with properties ENTRY.

Meant to be called indirectly as `org-node-creation-fn', so that some
necessary variables are set."
  (let* ((citekey (concat "@" key))
         (path-to-write (citar-file--get-note-filename key)))
    (when (file-exists-p path-to-write)
      (user-error "There's probably an issue, file already exist!")
      (find-file path-to-write))
    (when (find-buffer-visiting path-to-write)
      (user-error "A buffer already exists for filename %s" path-to-write)
      (find-buffer-visiting path-to-write))
    (find-file path-to-write)
    ;; TODO Template support: citar + org
    (insert ":PROPERTIES:"
            "\n:ROAM_REFS: " citekey
            "\n:ID:       " (org-id-new)
            "\n:END:"
            "\n#+title: " (citar-format--entry (citar--get-template 'note) key))
    (goto-char (point-max))
    (push (current-buffer) org-node--not-yet-saved)
    (run-hooks 'org-node-creation-hook)))

;; TODO ?
;; (defun citar-org-node-guess-or-ask-dir (prompt)
;;  "Maybe prompt for a directory, and if so, show string PROMPT.
;; Behavior depends on the user option `org-node-ask-directory'.
;; If multiple paths in `citar-notes-paths'. Prompt for one of them"
;;  (if (eq t org-node-ask-directory)
;;      (read-directory-name prompt)
;;    (if (stringp org-node-ask-directory)
;;        org-node-ask-directory
;;      (if (= (length citar-notes-paths) 1)
;;      (car citar-notes-paths)
;;    (completing-read prompt citar-notes-paths)))))


(defconst citar-org-node-notes-config ;; Necessary to make citar integrate well with org node and then setup citar
  (list :name "citar-org-node Notes"
        :category 'org-node
        :items #'citar-org-node--get-nodes-candidates
        :hasitems #'citar-org-node-has-notes
        :open #'citar-org-node--open-note
        :create #'citar-org-node--new-file))

(defun citar-org-node-setup ()
  "Setup `citar-org-node'."
  (org-node-cache-ensure)
  (citar-register-notes-source
   'citar-org-node citar-org-node-notes-config)
  (setq citar-notes-source 'citar-org-node))

(provide 'citar-org-node)
;;; citar-org-node.el ends here
