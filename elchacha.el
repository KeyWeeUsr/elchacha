;;; elchacha.el --- Elisp ChaCha20 implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, elchacha
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/KeyWeeUsr/elchacha

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

;; TBD

;;; Code:

(defconst elchacha-constants
  (let* ((size 4) (const "expand 32-byte k") (len (length const))
         chunks result)
    (dotimes (idx (/ len size))
      (push (substring const (* idx size) (min len (* (1+ idx) size))) chunks))
    (setq chunks (reverse chunks))
    (dolist (chunk chunks)
      (push (string-to-number
             (apply 'concat (mapcar (lambda (x) (format "%x" x))
                                    (reverse (string-to-list chunk))))
             16)
            result))
    (apply 'vector (reverse result)))
  "ChaCha20 32-bit constants.")

(defun elchacha-rotate (v n &optional size)
  "Left (N>0) or right (N<0) rotate the *unsigned* SIZE-bit value V N-times."
  (unless size (setq size 32))
  (let ((n (mod (+ n size) size)))
    (if (= n 0) v
      (logand (logior (ash v n) (ash v (* -1 (- size n))))
              (1- (expt 2 size))))))

(defun elchacha-quarter-round (a b c d)
  "Perform a quarter-round operation on four numbers."
  (let ((bound (expt 2 32)))
    (setq a (mod (+ a b) bound)
          d (logxor d a)
          d (elchacha-rotate d 16)
          c (mod (+ c d) bound)
          b (logxor b c)
          b (elchacha-rotate b 12)
          a (mod (+ a b) bound)
          d (logxor d a)
          d (elchacha-rotate d 8)
          c (mod (+ c d) bound)
          b (logxor b c)
          b (elchacha-rotate b 7))
    (vector a b c d)))

(provide 'elchacha)
;;; elchacha.el ends here
