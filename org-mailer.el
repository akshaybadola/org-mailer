;;; org-mailer.el --- Functions to integrate `org-mode' and `mu4e' ;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019,2020,2021,2022
;; Akshay Badola

;; Author:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Maintainer:	Akshay Badola <akshay.badola.cs@gmail.com>
;; Time-stamp:	<Saturday 24 September 2022 18:56:31 PM IST>
;; Keywords:	org, mu4e, mail, org-mime
;; Version:     0.2.1

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
;; There's also a python script `gmailer.py' which allows you to mail the buffer
;; with gmail and XOAUTH2 with the credentials being fetched from `pass'
;; password store or plain text.

;;; Code:

(require 'smtpmail)
(require 'sendmail)
(require 'dash)
(require 'org)
(require 'org-mime)
(require 'util/org "util-org.el")

(defconst org-mailer-version "0.2.0"
  "`org-mailer' version string.")

(defcustom org-mailer-links-cache-file ""
  "Text File in `org-mailer-links-cache' format." ;
  :type 'file
  :group 'org-mailer)

(defcustom org-mailer-addr ""
  "Address for the http gmail service."
  :type 'string
  :group 'org-gmailer)

(defcustom org-mailer-logfile ""
  "File for logging offlineimap output."
  :type 'file
  :group 'org-mailer)

(defvar org-mailer-links-cache nil
  "A hash table mapping local files to a remote cache.
Must be defined and maintained by the user.  It should consist
`local-file;remote-file' pairs of lines of texts with a `;'
delimiter")

(defvar org-mailer-mail-subtree-preprocess-hook nil
  "Preprocessing functions for `org-mailer-mail-subtree'.
The functions in this hook are executed just after inserting
contents of org subtree.")

(defvar org-mailer-mail-subtree-postprocess-hook nil
  "Postprocessing functions for `org-mailer-mail-subtree'.
The functions in this hook are executed just before the temp org
buffer is converted to html.")

(defvar org-mailer-buffer-name "*org-mailer-org-buffer*"
  "Name of the temp buffer for editing the subtree")

(defvar org-mailer-after-send-hook '(org-mailer-cleanup)
  "Hook to run after sending mail via `org-mailer'.")

;; (setq org-mailer-mail-subtree-preprocess-hook nil)
(add-to-list 'org-mailer-mail-subtree-preprocess-hook
             #'org-mailer-remove-useless-items-from-buffer)
(add-to-list 'org-mailer-mail-subtree-preprocess-hook
             #'org-mailer-convert-pdf-links-to-gdrive)
(add-to-list 'org-mailer-mail-subtree-preprocess-hook
             #'org-mailer-insert-urls-in-properties-to-body)
(add-to-list 'org-mailer-mail-subtree-preprocess-hook
             #'org-mailer-convert-file-links-to-references)
(add-to-list 'org-mailer-mail-subtree-postprocess-hook
             #'util/org-remove-all-drawers)

(defun org-mailer-install-py-deps ()
  (let* ((packages (shell-command-to-string "pip freeze"))
         (have-oauthclient (string-match-p "oauth2client" packages))
         (have-googlepyclient (string-match-p "google-api-python-client" packages))
         (have-flask (string-match-p "flask" (downcase packages))))
    (unless (and have-oauthclient have-googlepyclient)
      (message "Some missing dependencies for python package. Installing...")
      (shell-command "pip install flask oauth2client google-api-python-client"
                     "*org-mailer-install-py-deps*" "*org-mailer-install-py-deps*"))))

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
  "Convert org file links to internal links."
  (let* ((link-re util/org-fuzzy-or-custom-id-link-re)
         (links (with-current-buffer org-mailer-source-buffer
                  (-uniq (save-restriction
                           (util/org-get-text-links link-re t t)))))
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
                                  (let ((subtree
                                         (if custid
                                             (util/org-get-subtree-with-body-for-custom-id re)
                                           (util/org-get-subtree-with-body-for-heading-matching-re re))))
                                    (if (string-match-p "[a-zA-Z]+[0-9]\\{4\\}.+" re)
                                        subtree
                                      (warn "Link %s is not a publication. Not inserting references." re) nil)))))))
                        links))
    (org-end-of-subtree t)
    (seq-do (lambda (x) (org-paste-subtree (+ 1 level) x)) (-filter 'identity links))))

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
  (let* ((bufs (-filter
                (lambda (x) (with-current-buffer x
                              (eq major-mode 'mu4e-compose-mode)))
                (buffer-list)))
         (mail-buf (if (= (length bufs) 1)
                       (car bufs)
                     (ido-completing-read "Choose Buffer: "
                                          (mapcar
                                           (lambda (x) (format "%s" x))
                                           bufs)))))
    (if mail-buf
        (with-current-buffer mail-buf
          (insert buf-string)
          (org-mime-htmlize))
      (org-mime-org-buffer-htmlize))))

(defun org-mailer-htmlize-current-buffer ()
  "Export current buffer with `org-mime-htmlize'.
Prior to export `org-mailer-mail-subtree-postprocess-hook' is run
on the current buffer.

With a single prefix arg `C-u' export to an existing
`mu4e-compose' buffer with `org-mailer-mime-htmlize'."
  (org-mailer-load-links-cache)
  (run-hook-with-args 'org-mailer-mail-subtree-postprocess-hook)
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
        (if current-prefix-arg
            (org-mailer-mime-htmlize buf-string)
          (org-mime-org-buffer-htmlize))))))

;; Another implementation with `org-mime' is given here, though not it's not
;; necessarily for mailing a subtree
;; https://kitchingroup.cheme.cmu.edu/blog/category/email/
(defun org-mailer-compose ()
  "Export active region or org subtree with `org-mime'.
`\\[universal-argument]' is processed by
`org-mailer-htmlize-current-buffer' and if non-nil then ask for an
existing `mu4e-compose' buffer to insert the file.

It runs two hooks:
1. `org-mailer-mail-subtree-preprocess-hook' just after inserting
the subtree in the temp buffer.
2. `org-mailer-mail-subtree-postprocess-hook' is run after the
links cache is loaded and before it's exported to html."
  (interactive)
  (setq org-mailer-source-buffer (current-buffer))
  (unless org-mailer-links-cache
    (message "[org-mailer] %s is nil, proceeding without cache" org-mailer-links-cache))
  (save-excursion
    (let ((buf-string
           (save-restriction
             (if (region-active-p)
                 (narrow-to-region (region-beginning) (region-end))
               (org-narrow-to-subtree))
             (buffer-string)))
          (mu4e-export-buf (get-buffer-create org-mailer-buffer-name))
          (org-export-with-toc nil))
      (with-current-buffer mu4e-export-buf
        (goto-char (point-min))
        (insert (concat buf-string "\n"))
        (org-mode)
        (run-hook-with-args 'org-mailer-mail-subtree-preprocess-hook)
        (org-show-all)
        (setq-local org-finish-function #'org-mailer-htmlize-current-buffer)
        (goto-char (point-min))
        (switch-to-buffer mu4e-export-buf)))))

(defun org-mailer-cleanup (args)
  "Delete temp file and kill buffer."
  (let ((filename (plist-get args :filename)))
    (if (and (f-exists? (concat "~/" filename))
               (f-file-p (concat "~/" filename)))
        (delete-file (expand-file-name (concat "~/" filename)))
      (user-error "%s couldn't be deleted" filename))
    (when (get-buffer filename)
      (kill-buffer filename))))

;; NOTE: Defined in `smtpmail'
(defvar smtpmail-address-buffer)

;; TODO: How to make this async?
;; TODO: For some reason .tmpmail-* buffers are left hanging around
(defun org-mailer-send-via-gmailer ()
  "Send mail via gmailer.
Mostly adapted from `smtpmail-send-it'.

The address of the server is obtained from `gmailer-addr' which
should be the full http bind address including port.
E.g. https://localhost:1234."
  (let* ((errbuf (if mail-interactive
		     (generate-new-buffer " smtpmail errors")
		   0))
         ;; Not sure why this callback with lambda is here
         (callback (lambda (status url)
                     (message (format " %s" url))
                     (message (buffer-string))))
         (mailer-url (format "%s/sendmail?user=%s" org-mailer-addr user-mail-address))
	 (temp-buf (generate-new-buffer " smtpmail temp"))
	 (case-fold-search nil)
	 delimline
	 (mailbuf (current-buffer))
         ;; Examine this variable now, so that
	 ;; local binding in the mail buffer will take effect.
	 (smtpmail-mail-address
          (or (and mail-specify-envelope-from (mail-envelope-from))
              (let ((from (mail-fetch-field "from")))
	        (and from
		     (cadr (mail-extract-address-components from))))
	      (smtpmail-user-mail-address)))
	 (smtpmail-code-conv-from
	  (if enable-multibyte-characters
	      (let ((sendmail-coding-system smtpmail-code-conv-from))
	        (select-message-coding-system)))))
    (unwind-protect
	(with-current-buffer temp-buf
	  (erase-buffer)
	  ;; Use the same `buffer-file-coding-system' as in the mail
	  ;; buffer, otherwise any `write-region' invocations (e.g., in
	  ;; mail-do-fcc below) will annoy with asking for a suitable
	  ;; encoding.
	  (set-buffer-file-coding-system smtpmail-code-conv-from nil t)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (mail-sendmail-undelimit-header)
	  (setq delimline (point-marker))
          ;; (sendmail-synch-aliases)
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    ;; We used to process Resent-... headers here,
	    ;; but it was not done properly, and the job
	    ;; is done correctly in `smtpmail-deduce-address-list'.
	    ;; Don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
		(replace-match "")
	      ;; This one matches a Subject just before the header delimiter.
	      (if (and (re-search-forward "^Subject:\\([ \t]*\n\\)+" delimline t)
		       (= (match-end 0) delimline))
		  (replace-match "")))
	    ;; Put the "From:" field in unless for some odd reason
	    ;; they put one in themselves.
	    (goto-char (point-min))
	    (if (not (re-search-forward "^From:" delimline t))
		(let* ((login smtpmail-mail-address)
		       (fullname (user-full-name)))
		  (cond ((eq mail-from-style 'angles)
			 (insert "From: " fullname)
			 (let ((fullname-start (+ (point-min) 6))
			       (fullname-end (point-marker)))
			   (goto-char fullname-start)
			   ;; Look for a character that cannot appear unquoted
			   ;; according to RFC 822 or its successors.
			   (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
						  fullname-end 1)
			       (progn
				 ;; Quote fullname, escaping specials.
				 (goto-char fullname-start)
				 (insert "\"")
				 (while (re-search-forward "[\"\\]"
							   fullname-end 1)
				   (replace-match "\\\\\\&" t))
				 (insert "\""))))
			 (insert " <" login ">\n"))
			((eq mail-from-style 'parens)
			 (insert "From: " login " (")
			 (let ((fullname-start (point)))
			   (insert fullname)
			   (let ((fullname-end (point-marker)))
			     (goto-char fullname-start)
			     ;; RFC 822 and its successors say \ and
			     ;; nonmatching parentheses must be
			     ;; escaped in comments.
			     ;; Escape every instance of ()\ ...
			     (while (re-search-forward "[()\\]" fullname-end 1)
			       (replace-match "\\\\\\&" t))
			     ;; ... then undo escaping of matching parentheses,
			     ;; including matching nested parentheses.
			     (goto-char fullname-start)
			     (while (re-search-forward
				     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
				     fullname-end 1)
			       (replace-match "\\1(\\3)" t)
			       (goto-char fullname-start))))
			 (insert ")\n"))
			((null mail-from-style)
			 (insert "From: " login "\n")))))
	    ;; Insert a `Message-Id:' field if there isn't one yet.
	    (goto-char (point-min))
	    (unless (re-search-forward "^Message-Id:" delimline t)
	      (insert "Message-Id: " (message-make-message-id) "\n"))
	    ;; Insert a `Date:' field if there isn't one yet.
	    (goto-char (point-min))
	    (unless (re-search-forward "^Date:" delimline t)
	      (insert "Date: " (message-make-date) "\n"))
	    ;; Possibly add a MIME header for the current coding system
	    (let (charset)
	      (goto-char (point-min))
	      (and (eq mail-send-nonascii 'mime)
		   (not (re-search-forward "^MIME-version:" delimline t))
		   (progn (skip-chars-forward "\0-\177")
			  (/= (point) (point-max)))
		   smtpmail-code-conv-from
		   (setq charset
			 (coding-system-get smtpmail-code-conv-from
					    'mime-charset))
		   (goto-char delimline)
		   (insert "MIME-version: 1.0\n"
			   "Content-type: text/plain; charset="
			   (symbol-name charset)
			   "\nContent-Transfer-Encoding: 8bit\n")))
	    ;; Insert an extra newline if we need it to work around
	    ;; Sun's bug that swallows newlines.
	    (goto-char (1+ delimline))
            (cond ((boundp 'mail-mailer-swallows-blank-line)
                   (if (eval mail-mailer-swallows-blank-line)
		       (newline)))
                  ((boundp 'message-mailer-swallows-blank-line)
                   (if (eval message-mailer-swallows-blank-line)
		       (newline)))
                  (t (error "Not sure how we got here")))
	    ;; Find and handle any Fcc fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^Fcc:" delimline t)
		;; Force `mail-do-fcc' to use the encoding of the mail
		;; buffer to encode outgoing messages on Fcc files.
		(let ((coding-system-for-write
		       ;; mbox files must have Unix EOLs.
		       (coding-system-change-eol-conversion
			smtpmail-code-conv-from 'unix)))
		  (mail-do-fcc delimline)))
	    (if mail-interactive
		(with-current-buffer errbuf
		  (erase-buffer))))
	  ;; Encode the header according to RFC2047.
	  (mail-encode-header (point-min) delimline)
	  ;;
	  (setq smtpmail-address-buffer (generate-new-buffer "*smtp-mail*"))
	  (setq smtpmail-recipient-address-list
                (smtpmail-deduce-address-list temp-buf (point-min) delimline))
	  (kill-buffer smtpmail-address-buffer)

	  (smtpmail-do-bcc delimline)
          ;; NOTE: This is where we differ from `smtpmail-send-it'. Instead of
          ;;       sending via an smtp server we use the gmailer service running
          ;;       at gmailer-url.
          (let ((filename (make-temp-name ".tmpmail-")))
            (write-file (expand-file-name (concat "~/" filename)))
            (with-current-buffer (url-retrieve-synchronously (format "%s&filename=%s" mailer-url filename))
              (let ((case-fold-search t))
                (goto-char (point-min))
                (re-search-forward "\r?\n\r?\n")
                (cond ((looking-at "error")
                       (error "Sending mail failed: %s"
                              (buffer-substring-no-properties (point) (point-max))))
                      ((looking-at "success")
                       (message "Sending mail sucessful: %s"
                                (buffer-substring-no-properties (point) (point-max))))
                      (t (error "Some weird error occurred while sending mail")))))
            (run-hook-with-args 'org-mailer-after-send-hook `(:filename ,filename)))))))

(provide 'org-mailer)

;;; org-mailer.el ends here
