;; Copyright (C) 2014
;;
;; Author: BoyW165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x, GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-08-01 (0.0.1)
;;    Initial release.

(require 'ido)

(defvar prj-total-files-cache nil)

(defvar prj-process-grep nil)

(defvar prj-grep-output-file nil)

(defun prj-filedb-path (&optional name)
  (let ((dir (expand-file-name (format "%s/%s/files"
                                       prj-workspace-path
                                       (prj-project-name)))))
    (if name
        (format "%s/%s.files" dir (or (and (string= name "all") name)
                                      (and (string-match "^temp.*" name) name)
                                      (secure-hash 'sha1 name)))
      dir)))

(defun prj-export-data (filename data)
  "Export `data' to `filename' file. The saved data can be imported with `prj-import-data'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length)
		(prin1-to-string data))))))

(defun prj-import-data (filename)
  "Read data exported by `prj-export-data' from file `filename'."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (read (buffer-string)))))

(defun prj-list-to-cmd-form (filepaths)
  (let ((filepath filepaths)
        ret)
    (while filepath
      (setq ret (concat ret (car filepath))
            filepath (cdr filepath))
      (and filepath
           (setq ret (concat ret " "))))
    ret))

(defun prj-matches-to-find-form (doctype &optional is-exclude)
  "Convert DOCTYPE to string as include-path parameter for FIND.
Example:
  *.md;*.el;*.txt   =>   -name *.md -o -name *.el -o -name *.txt
If `is-exclude' is t:
  *.git;*.svn       =>   ! -name .git ! -name .svn"
  (if is-exclude
      (let ((matches (concat "! -name \"" doctype "\"")))
        (replace-regexp-in-string ";" "\" ! -name \"" matches))
    (let ((matches (concat "-name \"" doctype "\"")))
      (replace-regexp-in-string ";" "\" -o -name \"" matches))))

(defun prj-process-find (output filepaths &optional matches excludes)
  "Use `call-process' to call FIND. FILEPATHS is a file list, (FILE1 FILE2 ...).
 MATCHES and EXCLUDES is strings in format of \"-name pattern\"."
  (let ((dir (file-name-directory output))
        (stream (shell-command-to-string (format "find %s %s %s"
                                                 filepaths
                                                 (or matches "")
                                                 (or excludes "")))))
    (and dir (make-directory dir t))
    (with-temp-file output
      (insert stream)
      stream)))

(defun prj-process-find-change ()
  ;; TODO:
  t)

(defun prj-matches-to-grep-form (doctype)
  "Convert DOCTYPE to string as include-path parameter for FIND.
Example:
  *.md;*.el;*.txt   =>   -name *.md -o -name *.el -o -name *.txt"
  (let ((matches (concat "-name \"" doctype "\"")))
    (replace-regexp-in-string ";" "\" -o -name \"" matches)))

(defun prj-process-async-grep (match input-file output-file case-sensitive)
  (message (format "[%s] Searching is processing..." (prj-project-name)))
  (call-process-shell-command (format "echo \">>>>> %s\" 1>>\"%s\""
                                      match
                                      output-file))
  (setq prj-process-grep (start-process-shell-command
                          "prj-process-grep"
                          nil
                          (format "xargs grep -nH %s \"%s\" < \"%s\" 1>>\"%s\" 2>/dev/null"
                                  (if (null case-sensitive) "-i" "")
                                  match
                                  input-file
                                  output-file))
        ;; Remember filename of output file.
        prj-grep-output-file output-file)
  ;; add process sentinel.
  (set-process-sentinel prj-process-grep 'prj-async-grep-complete))

(defun prj-async-grep-complete (process message)
  (cond
   ((eq (process-status process) 'run))
   ((memq (process-status process) '(stop exit signal))
    (setq prj-process-grep nil)
    (with-temp-file prj-grep-output-file
      (insert-file-contents-literally prj-grep-output-file)
      (goto-char (point-max))
      (unless (looking-back "\n")
        (insert "\n"))
      (insert "<<<<<\n\n"))
    (find-file prj-grep-output-file)
    (goto-char (point-max))
    (goto-char (or (re-search-backward "^>>>>>.*$" nil t)
                   (point)))
    (forward-line)
    ;; Because executing `goto-char' and `forward-line' in script won't trigger
    ;; `post-command-hook' which makes some features won't work correctly.
    (run-hooks 'post-command-hook))))

(defun prj-process-sync-grep (match input-file output-file &optional case-sensitive)
  (call-process-shell-command (format "grep %s \"%s\" \"%s\" 1>>\"%s\" 2>/dev/null"
                                      (if (null case-sensitive) "-i" "")
                                      match
                                      input-file
                                      output-file)))

(defun prj-process-cat (input-file output-file)
  (call-process-shell-command (format "cat \"%s\" 1>>\"%s\" 2>/dev/null"
                                      input-file
                                      output-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Back-ends ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun prj-index-files-backend (command &optional is-rebuild)
  (case command
    (:init)
    (:destroy
     (setq prj-total-files-cache nil)
     (garbage-collect))
    (:index-files
     (let (all-files)
       ;; Collect files in respect of document types.
       (dolist (doctype (prj-project-doctypes))
         (setq all-files (concat
                          all-files
                          (prj-process-find
                           (prj-filedb-path doctype)
                           (prj-list-to-cmd-form (prj-project-filepaths))
                           (prj-matches-to-find-form (cdr (assoc doctype prj-document-types)))
                           (prj-matches-to-find-form prj-exclude-types t)))))
       ;; Export all database.
       (when all-files
         (setq prj-total-files-cache (split-string all-files "\n" t))
         (with-temp-file (prj-filedb-path "all")
           (insert all-files)))))))

;;;###autoload
(defun prj-find-files-backend (command &optional doctypes filepaths return-list)
  (case command
    (:init)
    (:destroy)
    (:find-files
     (unless (listp doctypes)
       (error "The parameter, doctypes, must be a list!"))
     (unless (listp filepaths)
       (error "The parameter, filepaths, must be a list!"))
     (cond
      ;; Return "all" file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((and (null doctypes) (null filepaths) (null return-list))
       (prj-filedb-path "all"))
      ;; Return list of all files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ((and (null doctypes) (null filepaths) return-list)
       prj-total-files-cache)
      ;; Others ... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      (t
       (let ((temp1 (prj-filedb-path "temp1"))
             (temp2 (prj-filedb-path "temp2"))
             file)
         ;; Filter document types
         (with-temp-file temp1)
         (if doctypes
             (dolist (doctype doctypes)
               (when (and (setq file (prj-filedb-path doctype))
                          (file-exists-p file))
                 (prj-process-cat file temp1)))
           (setq temp1 (prj-filedb-path "all")))
         ;; Filter file paths
         (with-temp-file temp2)
         (dolist (filepath filepaths)
           (prj-process-sync-grep (expand-file-name filepath) temp1 temp2))
         (if return-list
             (with-temp-buffer
               (insert-file-contents-literally temp2)
               (split-string (buffer-string) "\n" t))
           temp2)))))))

;;;###autoload
(defun prj-async-grep-backend (command &optional match input-file output-file case-sensitive)
  (case command
    (:init)
    (:destroy)
    (:search
     (if (and (processp prj-process-grep)
              (process-live-p prj-process-grep))
         (message "Searching is under processing, please wait...")
       (prj-process-async-grep match input-file output-file case-sensitive)))))

(provide 'prj-default-backend)
