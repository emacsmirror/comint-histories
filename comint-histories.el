;;; comint-histories.el --- Multiple comint histories  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Nicholas Hubbard <nicholashubbard@posteo.net>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; SPDX-License-Identifier: MIT

;; Author: Nicholas Hubbard <nicholashubbard@posteo.net>
;; URL: https://github.com/NicholasBHubbard/comint-histories
;; Package-Requires: ((emacs "25.1"))
;; Version: 1.0
;; Created: 2025-01-02
;; By: Nicholas Hubbard <nicholashubbard@posteo.net>
;; Keywords: convenience

;;; Commentary:

;; This package provides functionality for defining multiple histories for
;; comint inputs. This is useful for dividing up histories for different
;; programs run through comint buffers.

;; Please see https://github.com/NicholasBHubbard/comint-histories for more
;; information.

;;; Code:

(require 'comint)
(require 'cl-lib)
(require 'seq)
(require 'f)

(defvar comint-histories-persist-dir
  (f-join user-emacs-directory "comint-histories")
  "Directory to place saved histories.")

(defvar comint-histories-default-persist t
  "Default :persist value for a history.")

(defvar comint-histories-default-length 500
  "Default :length value for a history.")

(defvar comint-histories-default-rtrim t
  "Default :rtrim value for a history.")

(defvar comint-histories-default-ltrim t
  "Default :ltrim value for a history")

(defvar comint-histories--histories nil
  "Internal alist of plists containing all defined histories.")

(defmacro comint-histories-add-history (name &rest props)
  "Declare a comint-histories history named NAME with properties PROPS.

Usage: (comint-histories-add-history history-name
          [:keyword [option]]...)

:predicates    List of functions that take zero args who's conjunction
               determines the selection of this history.

:filters       List of regexp strings and functions that take one arg. If the
               input matches any of the regexp's, or any of the functions return
               non-nil when applied to the input, then the input is not added
               to the history.

:persist       If non-nil then save and load the history to/from a file.

:length        Maximum length of the history ring.

:rtrim         If non-nil then trim beginning whitespace from the input before
               adding to the history.

:ltrim         If non-nil then trim ending whitespace from the input before
               adding to the history."
  (declare (indent defun))
  (let* ((name (symbol-name name))
         (history (list :history nil ; a ring
                        :predicates nil
                        :filters nil
                        :persist comint-histories-default-persist
                        :length comint-histories-default-length
                        :rtrim comint-histories-default-rtrim
                        :ltrim comint-histories-default-ltrim))
         (valid-props '(:predicates :filters :persist :length :rtrim :ltrim)))
    (while props
      (let ((prop (car props))
            (val (cadr props)))
        (if (not (memq prop valid-props))
            (error "Invalid history property: %s" prop)
          (setq history (plist-put history prop (eval val)))
          (setq props (cddr props)))))
    (let ((history (cons name history)))
      (setf (plist-get (cdr history) :history) (make-ring (plist-get (cdr history) :length)))
      (when (plist-get (cdr history) :persist) (comint-histories--load-history history t))
      `(progn
         (setf (alist-get ,name comint-histories--histories nil 'remove #'string=) nil)
         (add-to-list 'comint-histories--histories (quote ,history) t)))))

(defun comint-histories-search-history (&optional history)
  "Search a history with `completing-read'."
  (interactive)
  (let ((history (or history (comint-histories--select-history))))
    (if (not history)
        (user-error "no history could be selected")
      (let ((completion-styles nil)
            (completion-category-overrides nil))
        (completing-read (format "history (%s): " (car history))
                         (ring-elements (plist-get (cdr history) :history)))))))

(defun comint-histories-get-prompt ()
  "Return the prompt for the comint buffer as a string."
  (when (derived-mode-p 'comint-mode)
    (save-excursion
      (goto-char (point-max))
      (let ((prompt-start
             (save-excursion
               (re-search-backward comint-prompt-regexp nil t))))
        (when prompt-start
          (buffer-substring-no-properties
           prompt-start (point)))))))

(defun comint-histories--history-file (history)
  "Return the history file for `history', creating it if it doesn't exist."
  (let* ((dir (f-join user-emacs-directory "comint-histories"))
         (file (f-join dir (car history))))
    (when (not (f-directory? dir))
      (f-mkdir dir))
    (when (not (f-file? file))
      (f-touch file))
    file))

(defun comint-histories--load-history (history &optional insert)
  "Load the history from `history's persistent file and return it as a list.

If `insert' is non-nil then insert the history into `history's history ring."
  (let* ((history-file (comint-histories--history-file history))
         (history-text (f-read-text history-file 'utf-8))
         (lines (split-string history-text "\n" t)))
    (when insert
      (dolist (x (reverse lines))
        (comint-histories--insert-into-history history x)))
    lines))

(defun comint-histories--save-history (history)
  "Save `history's history ring to it's persistent file."
  (let* ((history-file (comint-histories--history-file history))
         (existing-history (ring-elements (plist-get (cdr history) :history)))
         (loaded-history (comint-histories--load-history history))
         (text ""))
    (dolist (x (take (plist-get (cdr history) :length)
                     (delete-dups (append existing-history loaded-history))))
      (setq text (concat text (format "%s\n" x))))
    (f-write-text text 'utf-8 history-file)))

(defun comint-histories--insert-into-history (history input)
  "Insert `input' into `history's history ring.

If `history' has its :ltrim or :rtrim props set the trim the leading/trailing
whitespace from `input' before processing. If and of `history's :filter's return
non-nil when applied to `input', then do not insert `input' into the history."
  (when (plist-get (cdr history) :ltrim)
    (setq input (replace-regexp-in-string "^[\n ]+" "" input)))
  (when (plist-get (cdr history) :rtrim)
    (setq input (replace-regexp-in-string "[\n ]+$" "" input)))
  (unless (string-empty-p input)
    (let ((filtered))
      (catch 'loop
        (dolist (filter (plist-get (cdr history) :filters))
          (if (functionp filter)
              (when (funcall filter input)
                (setq filtered t)
                (throw 'loop t))
            (when (string-match-p filter input) ; regexp
              (setq filtered t)
              (throw 'loop t)))))
      (when (not filtered)
        (let ((ring (plist-get (cdr history) :history)))
          (when-let ((existing-idx (ring-member ring input)))
            (ring-remove ring existing-idx))
          (ring-insert ring input))))))

(defun comint-histories--select-history ()
  "Select a history from `comint-histories--histories'."
  (let ((selected-history))
    (catch 'loop
      (dolist (history comint-histories--histories)
        (when (cl-every (lambda (fn) (funcall fn))
                        (plist-get (cdr history) :predicates))
          (setq selected-history history)
          (throw 'loop t))))
    selected-history))

(defun comint-histories--process-comint-input ()
  "Process the current comint input buffer, potentially adding it to a history.

This function is used as advice aroung `comint-send-input' when
`comint-histories-mode' is enabled."
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
        (user-error "Current buffer has no process")
      (when-let ((history (comint-histories--select-history)))
        (widen)
        (let* ((pmark (process-mark proc))
               (input (if (>= (point) (marker-position pmark))
                          (progn (if comint-eol-on-send
                                     (if comint-use-prompt-regexp
                                         (end-of-line)
                                       (goto-char (field-end))))
                                 (buffer-substring pmark (point)))
                        (let ((copy (funcall comint-get-old-input)))
                          (goto-char pmark)
                          (insert copy)
                          copy))))
          (comint-histories--insert-into-history history input))))))

(defun comint-histories--save-histories ()
  "Save persistent histories in `comint-histories--histories' to disk."
  (dolist (history comint-histories--histories)
    (when (plist-get (cdr history) :persist)
      (comint-histories--save-history history))))

(define-minor-mode comint-histories-mode
  "Toggle `comint-histories-mode'."
  :require 'comint-histories
  (if comint-histories-mode
      (progn
        (advice-add 'comint-send-input :before #'comint-histories--process-comint-input)
        (add-hook 'kill-emacs-hook #'comint-histories--save-histories))
    (advice-remove 'comint-send-input #'comint-histories--process-comint-input)
    (remove-hook 'kill-emacs-hook #'comint-histories--save-histories)))

(provide 'comint-histories)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; comint-histories.el ends here
