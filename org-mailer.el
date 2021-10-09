;;; org-mailer.el --- Functions to integrate `org-mode' and `mu4e' ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019,2020,2021
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Friday  8 January 2021 15:42:46 IST>
;; Keywords:	org, mu4e, mail, org-mime
;; Version:     0.2.0

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Convert a buffer (or subtree) from an `org-mode' buffer to a `mu4e' message
;; and mail it with some enhanced customizations.
;;
;; This package is built upon `org-mime' and contains extra utilities to
;; preprocess and postprocess the org buffer to remove headings or items or
;; other org structures.
;;
;; There are also subroutines for including org links mentioned in text and
;; attaching files automatically via some cloud platform.
;;
;; There's also a python script which allows you to mail the buffer with gmail
;; and XOAUTH2.

;;; Code:

(require 'dash)
(require 'org)
(require 'org-mime)
(require 'util)

(defconst org-mailer-version "0.2.0"
  "`org-mailer' version string.")

(defcustom org-mailer-links-cache-file ""
  "Text File in `org-mailer-links-cache' format." ;
  :type 'file
  :group 'org-mailer)

(defvar org-mailer-links-cache nil
  "A hash table mapping local files to a remote cache.
Must be defined and maintained by the user.  It should consist
`local-file;remote-file' pairs of lines of texts with a `;'
delimiter")

(defvar org-mailer-mail-subtree-preprocess-hooks nil
  "Preprocessing hooks to run after inserting contents of org subtree with `org-mailer-mail-subtree'.")

(defvar org-mailer-mail-subtree-postprocess-hooks nil
  "Postprocessing hooks to run after inserting contents of org subtree with `org-mailer-mail-subtree'.")

(defvar org-mailer-buffer-name "*org-mailer-mail-subtree-buffer*"
  "Name of the temp buffer for editing the subtree")

;; (setq org-mailer-mail-subtree-preprocess-hooks nil)
(add-to-list 'org-mailer-mail-subtree-preprocess-hooks
             #'org-mailer-remove-useless-items-from-buffer)
(add-to-list 'org-mailer-mail-subtree-preprocess-hooks
             #'org-mailer-convert-pdf-links-to-gdrive)
(add-to-list 'org-mailer-mail-subtree-preprocess-hooks
             #'org-mailer-insert-urls-in-properties-to-body)
(add-to-list 'org-mailer-mail-subtree-preprocess-hooks
             #'org-mailer-convert-file-links-to-references)
(add-to-list 'org-mailer-mail-subtree-postprocess-hooks
             #'util/org-remove-all-drawers)

(defun org-mailer-load-links-cache ()
  "Load the existing cache from disk if defined.
See `org-mailer-links-cache'."
  (interactive)
  (if (string-empty-p org-mailer-links-cache-file)
      (user-error "File %s is not defined" (replace-regexp-in-string "%" "%%"
                                                                     org-mailer-links-cache-file))
    (setq org-mailer-links-cache (make-hash-table :test 'equal))
    (seq-do (lambda (x)
              (let ((split (split-string x ";")))
                (puthash (car split) (cadr split) org-mailer-links-cache)))
            (split-string (with-current-buffer
                              (find-file-noselect org-mailer-links-cache-file)
                            (buffer-string)) "\n" t))
    (message "[org-mailer] Loaded remote links cache from disk.")))

(defun org-mailer-remove-useless-items-from-buffer ()
  "Remove unwanted items from current `org-mode' buffer.
Uses `util' package to remove items or headings.

This function removes STATE changes and list items beginning with
`[note]'. Items or subtrees matching custom regexps can be
searched and removed.

See `util/org-remove-list-items-matching-re-from-buffer' and
`util/org-remove-subtrees-matching-re'."
  ;; operates on current buffer
  (let ((state-re (concat "^State *\"" org-todo-regexp))
        (notes-re "^[^ ]*\\[note\\]"))
    (util/org-remove-list-items-matching-re-from-buffer
     (string-join (list state-re notes-re) "\\|"))))

(defvar org-mailer-source-buffer nil)
(defun org-mailer-convert-file-links-to-references ()
  (let* ((link-re util/org-fuzzy-or-custom-id-link-re)
         (links (with-current-buffer org-mailer-source-buffer
                 (-uniq (save-restriction
                          (util/org-get-text-links link-re t t)))))
        (buf-string (save-restriction
                      (org-narrow-to-subtree)
                      (buffer-string)))
        (level (org-current-level)))
    (setq links (mapcar (lambda (x)
                          (pcase-let* ((item (cadr x))
                                       (custid (string-match-p "^#" (cadr (split-string item "::"))))
                                       (`(,file ,re) (split-string item "::[*#]"))
                                       (buf (or (get-buffer (f-filename file))
                                                (find-file-noselect file))))
                            (with-current-buffer buf
                              (save-excursion
                                (save-restriction
                                  (if custid
                                      (util/org-get-subtree-with-body-for-custom-id re)
                                    (util/org-get-subtree-with-body-for-heading-matching-re re)))))))
                        links))
    (org-end-of-subtree t)
    (seq-do (lambda (x)
              (org-paste-subtree (+ 1 level) x))
            links)))

(defun org-mailer-replace-pdf-link-with-gdrive (cache)
  "Replace pdf file URIs for current heading with gdrive links.
CACHE is a hash table mapping file URIs to gdrive URIs.

For an example of such a cache, see `ref-man-public-links-cache' in
URL `https://github.com/akshaybadola/ref-man/blob/master/ref-man-remote.el'."
  (when (org-entry-get (point) "PDF_FILE")
    (let* ((pblock (org-get-property-block))
           (file (replace-regexp-in-string
                  "\\[\\|\\]" "" (org-entry-get (point) "PDF_FILE")))
           (link (gethash file cache)))
      (unless link
        (user-error "File %s not in cache" (replace-regexp-in-string "%" "%%" file)))
      (save-restriction
        (util/org-narrow-to-heading-and-body)
        (let ((link-str (concat "- gdrive_link: " link)))
          (unless (string-match-p (regexp-quote link-str) (buffer-string))
            (goto-char (cdr pblock))
            (end-of-line)
            (open-line 1)
            (forward-line)
            (indent-relative)
            (insert "- gdrive_link: " link)))))))

(defun org-mailer-move-urls-from-property-drawer-to-text ()
  "Move a URL from property drawer in current heading to text."
  (let ((pblock (org-get-property-block)))
    (seq-do (lambda (x) (when (string-match-p "URL$" (car x))
                          (save-restriction
                            (util/org-narrow-to-heading-and-body)
                            (let ((link-str (concat "- " (downcase (car x)) ": " (cdr x))))
                              (unless (string-match-p (regexp-quote link-str) (buffer-string))
                                (goto-char (cdr pblock))
                                (end-of-line)
                                (open-line 1)
                                (forward-line)
                                (indent-relative)
                                (insert link-str))))))
            (org-entry-properties))))

(defun org-mailer-insert-urls-in-properties-to-body ()
  "Move URLs from property drawers to text in current `org-mode' buffer to text."
  (goto-char (point-min))
  (when (org-at-heading-p)
    (org-mailer-move-urls-from-property-drawer-to-text))
  (while (and (not (eobp)) (outline-next-heading))
    (org-mailer-move-urls-from-property-drawer-to-text)))

(defun org-mailer-convert-pdf-links-to-gdrive ()
  "Replace pdf file URIs for entire buffer with gdrive links.
This hook is run only if `org-mailer-links-cache' is non-nil."
  (when org-mailer-links-cache
    (let ((cache (copy-hash-table org-mailer-links-cache)))
      (goto-char (point-min))
      (when (org-at-heading-p)
        (condition-case exception
            (org-mailer-replace-pdf-link-with-gdrive cache)
          (error (warn (nth 1 exception)))))
      (while (and (not (eobp)) (outline-next-heading))
        (condition-case exception
            (org-mailer-replace-pdf-link-with-gdrive cache)
          (error (warn (nth 1 exception))))))))

(defun org-mailer-dired-add-marked-files-as-attachment ()
  (interactive)
  (let ((files (dired-get-marked-files))
        (buf (ido-completing-read "Compose Buffer: "
                                  (mapcar
                                   (lambda (x) (format "%s" x))
                                   (-filter
                                    (lambda (x) (with-current-buffer x
                                                  (eq major-mode 'mu4e-compose-mode)))
                                    (buffer-list))))))
    (with-current-buffer buf
      (seq-do (lambda (x) (mail-add-attachment x))
              files)
      (message "Added attachments %s" files))))

(defun org-mailer-mime-htmlize (buf-string)
  "Convert org buffer contents to text/plain + text/html to mail.
BUF-STRING is the buffer string from the desired `org-mode'
buffer.  This is inserted into another mail buffer for
replying/forwarding etc.  Derived from
`org-mime-org-buffer-htmlize'"
  (interactive)
  (let* ((mail-buf (ido-completing-read "Compose Buffer: "
                                        (mapcar
                                         (lambda (x) (format "%s" x))
                                         (-filter
                                          (lambda (x) (with-current-buffer x
                                                        (eq major-mode 'mu4e-compose-mode)))
                                          (buffer-list))))))
    (when mail-buf
      (with-current-buffer mail-buf
        (insert buf-string)
        (org-mime-htmlize)))))

(defun org-mailer-htmlize-current-buffer ()
  "Export current buffer with `org-mime-htmlize'.
Prior to export `org-mailer-mail-subtree-postprocess-hooks' is run
on the current buffer.

With a single prefix arg `C-u' export to an existing
`mu4e-compose' buffer with `org-mailer-mime-htmlize'."
  (org-mailer-load-links-cache)
  (run-hooks 'org-mailer-mail-subtree-postprocess-hooks)
  (let ((org-export-with-toc nil)
        (org-export-with-broken-links 'mark)
        (org-export-with-timestamps nil)
        (org-export-with-clocks nil)
        (org-export-with-latex nil)
        (org-export-with-todo-keywords nil)
        (org-export-with-sub-superscripts nil)
        (org-export-with-date nil)
        (org-export-with-properties nil)
        (buf-string (buffer-string)))
    (bury-buffer)
    (if current-prefix-arg
        (org-mailer-mime-htmlize buf-string)
      (with-current-buffer org-mailer-buffer-name
        (let ((org-mime-export-options
               '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil
                                  :with-broken-links 'mark
                                  :with-latex nil
                                  :with-todo-keywords nil
                                  :with-clocks nil
                                  :with-sub-superscript nil
                                  :with-date nil
                                  :with-properties nil)))
          (org-mime-org-buffer-htmlize))))))

;; Another implementation with `org-mime' is given here, though not it's not
;; necessarily for mailing a subtree
;; https://kitchingroup.cheme.cmu.edu/blog/category/email/
(defun org-mailer-mail-subtree ()
  "Export org subtree with `org-mime'.
`\\[universal-argument]' is processed by
`org-mailer-htmlize-current-buffer' and if non-nil then ask for an
existing `mu4e-compose' buffer to insert the file.

It runs two hooks:
1. `org-mailer-mail-subtree-preprocess-hooks' just after inserting
the subtree in the temp buffer.
2. `org-mailer-mail-subtree-postprocess-hooks' is run after the
links cache is loaded and before it's exported to html."
  (interactive)
  (setq org-mailer-source-buffer (current-buffer))
  (unless org-mailer-links-cache
    (message "[org-mailer] %s is nil, proceeding without cache" org-mailer-links-cache))
  (save-excursion
    (let ((buf-string (save-restriction (org-narrow-to-subtree)
                                        (buffer-string)))
          (mu4e-export-buf (get-buffer-create org-mailer-buffer-name))
          (org-export-with-toc nil))
      (with-current-buffer mu4e-export-buf
        (goto-char (point-min))
        (insert (concat buf-string "\n"))
        (org-mode)
        (run-hooks 'org-mailer-mail-subtree-preprocess-hooks)
        (org-show-all)
        (setq-local org-finish-function #'org-mailer-htmlize-current-buffer)
        (goto-char (point-min))
        (switch-to-buffer mu4e-export-buf)))))

(provide 'org-mailer)

;;; org-mailer.el ends here

