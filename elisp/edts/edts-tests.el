;; Copyright 2012 João Neves <sevenjp@gmail.com>
;;
;; This file is part of EDTS.
;;
;; EDTS is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; EDTS is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with EDTS. If not, see <http://www.gnu.org/licenses/>.
;;
;; Test library for edts.
(require 'em-glob)
(require 'path-util)

(defconst edts-test-directory (path-util-join edts-root-directory "test")
  "Directory where EDTS test files are located")

(defconst edts-test-project1-directory
  (path-util-join edts-test-directory "edts-test-project1")
  "Directory where EDTS edts-test-project1 is located")

(defun edts-test-project1-modules ()
  "Return a list of all modules in edts-test-project1."
  (file-expand-wildcards
   (path-util-join edts-test-project1-directory "lib" "*" "src" "*.erl")))

(defun edts-test-cleanup ()
  (edts-log-info "Doing test cleanup")
  (setq eproject-attributes-alist nil)
  (edts-shell-kill-all)
  (loop for  buf in (buffer-list)
        when (and (buffer-live-p buf) (buffer-local-value 'edts-mode buf))
        do   (progn
               (kill-buffer buf)
               (edts-log-info "killed %s" buf)))
  (edts-log-info "Test cleanup done"))

(defmacro edts-test-with-config (project-path config &rest body)
  "Run BODY with the project in PROJECT-PATH using CONFIG."
  `(let ((cfg-file (path-util-join ,project-path ".edts"))
         (config   ,config))
     (edts-project-write-config cfg-file config)
     (progn ,@body)
     (delete-file cfg-file)))
