;;; org-agenda-conflct.el --- Org agenda show conflict -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/org-agenda-conflict
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: org org-agenda

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This code marks conflicting items in the org agenda. Conflicting
;; items are items with an overlap between their start and end date.
;;
;;; Usage:

;; (add-hook 'org-agenda-finalize-hook
;;          '(lambda () (org-agenda-conflict-mark 'error)))

;;; Code:
(require 'org-agenda)

(defun org-agenda-conflict--get-item ()
  "Return the date range of an agenda item at point unless tagged CANCELLED.

The data range is a cons of the start and end date timestamp."

  (when-let* ((date (get-text-property (point) 'date))
              (tags (or (get-text-property (point) 'tags) '()))
              ;; For e.g. 11:30, time-of-day is "1130"
              (time-of-day (get-text-property (point) 'time-of-day))
              (duration (get-text-property (point) 'duration)))
    (when (not (member "CANCELLED" tags))
      (let* ((day (nth 1 date))
             (month (nth 0 date))
             (year (nth 2 date))
             (hour (/ time-of-day 100))
             (minutes (- time-of-day (* hour 100)))
             (start (encode-time 0 minutes hour day month year))
             (end (encode-time 0 (+ minutes (floor duration)) hour day month year)))
        (cons start end)))))

(defun org-agenda-conflict--check (item-1 item-2)
  "Check if date ranges ITEM-1 and ITEM-2 overlap."

  (when (and item-1 item-2)
    (let ((beg-1 (car item-1))
          (end-1 (cdr item-1))
          (beg-2 (car item-2))
          (end-2 (cdr item-2)))
      (or  (time-equal-p beg-1 beg-2)       ;; Events start at the same time
           (and (time-less-p beg-1 beg-2)   ;; Event 2 beg is inside event 1 range
                (time-less-p beg-2 end-1))
           (and (time-less-p end-2 end-1)   ;; Event 2 end is inside event 1 range
                (time-less-p beg-1 end-2))))))


(defun org-agenda-conflict-mark (face)
  "Mark items whose schedule conflict with face FACE.
Tags are not marked."

  (goto-char (point-min))
  (while (not (eobp))
    (let ((inhibit-read-only t)
          (point-1 (point))
          (date-1 (get-text-property (point) 'date))
          (item-1 (org-agenda-conflict--get-item))
          (next-day nil))
      (save-excursion
        (forward-line)
        (while (and (not next-day) (not (eobp)))
          (let ((point-2 (point))
                (date-2 (get-text-property (point) 'date))
                (item-2 (org-agenda-conflict--get-item)))
            (when (org-agenda-conflict--check item-1 item-2)
              (save-excursion
                (goto-char point-1)
                (beginning-of-line)
                (when (search-forward-regexp "^ \\(.+? \\)[ ]+:.*" nil t)
                  (add-text-properties (match-beginning 1) (match-end 1)
                                       `(face ,face)))
                (goto-char point-2)
                (beginning-of-line)
                (when (search-forward-regexp "^ \\(.+? \\)[ ]+:.*" nil t)
                  (add-text-properties (match-beginning 1) (match-end 1)
                                       `(face ,face)))))
            (when (not (eq date-1 date-2))
              (setq next-day t)))
          (forward-line))))
    (forward-line)))


(provide 'org-agenda-conflict)
;;; org-agenda-conflict.el ends here
