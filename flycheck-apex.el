;;; flycheck-pmd.el --- Flycheck: PMD support    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Remy Goldschmidt <taktoa@gmail.com>

;; Author: Remy Goldschmidt <taktoa@gmail.com>
;; URL: https://github.com/taktoa/flycheck-pmd
;; Keywords: convenience, tools, languages
;; Version: 0.2-git
;; Package-Requires: ((emacs "24.1") (flycheck "0.22") (let-alist "1.0.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; PMD support for Flycheck.

;;; Code:

(eval-when-compile
  (require 'let-alist)
  (require 'cl))

(require 'flycheck)

(defun pair-fold1 (proc seed ls)
  "Fold the procedure (PROC) with a seed (SEED) over a list (LS)."
  (loop
     for e on ls by #'cdr
     for acc = (funcall proc ls seed)
     then (funcall proc e acc)
     finally (return acc)))

(defun single? (ls)
  "Is the given list (LS) composed of a single element?"
  (and (not (null ls))
       (null (cdr ls))))

(defun intersperse (item ls)
  "Intersperse an element (ITEM) in a list (LS)."
  (pair-fold1 #'(lambda (pr acc)
                  (if (single? pr)
                      (cons (car pr) acc)
                      (cons item (cons (car pr) acc))))
              '() (reverse ls)))

(defcustom
  flycheck-pmd-rulesets
  '("")
  "List of rulesets for flycheck-pmd."
  :group 'flycheck-options
  :type '(repeat string))

(defconst
  flycheck-pmd-args
  (concat " -l java "
          " -f emacs "
          " -R " (apply 'concat (intersperse "," flycheck-pmd-rulesets)))
  "Arguments for PMD error message.")

(defconst apex-ruleset "rulesets/apex/ruleset.xml")
(defconst pmd-dir "~/dev/pmd/pmd/bin/run.sh")

(flycheck-define-command-checker 'apex-pmd
  "A syntax checker for Apex using PMD."
  :command `(,pmd-dir "pmd" " -f " " emacs" " -R " ,apex-ruleset " -d " source )
  ;; :command `("pmd" "pmd" ,flycheck-pmd-args " -d " source)
  :error-patterns '((error line-start (file-name) ":" line ": " (message) line-end))
  :modes '(java-mode))

;;;###autoload
(defun flycheck-pmd-setup ()
  "Setup Flycheck PMD.
Add `java-pmd' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'apex-pmd))

(provide 'flycheck-pmd-apex)

;;; flycheck-pmd.el ends here
