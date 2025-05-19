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

(defun elchacha-quarter-round-on (state &rest positions)
  "Perform a quarter-round on the chacha20 state."
  (let* ((a (aref state (nth 0 positions)))
         (b (aref state (nth 1 positions)))
         (c (aref state (nth 2 positions)))
         (d (aref state (nth 3 positions)))
         (tmp (elchacha-quarter-round a b c d)))
    (dotimes (idx 4)
      (aset state (nth idx positions) (aref tmp idx)))
    state))

(defun elchacha-read-int32-ul (data &optional offset)
  "Read an unsigned 32-bit integer from DATA at OFFSET."
  (let ((offset (or offset 0)))
    (+ (lsh (aref data offset) 0)
       (lsh (aref data (1+ offset)) 8)
       (lsh (aref data (+ 2 offset)) 16)
       (lsh (aref data (+ 3 offset)) 24))))

(defun elchacha-state-init (key nonce &optional block-count)
  "Initialize cipher state - the matrix."
  (unless block-count (setq block-count 0))
  ;; 32-bit
  (let ((state (make-vector 16 0)))
    (dotimes (idx (length elchacha-constants))
      (aset state (+ 0 idx) (aref elchacha-constants idx)))
    (let ((key-chunk-size 4))
      (dotimes (idx (/ (length key) key-chunk-size))
        (aset state (+ 4 idx)
              (elchacha-read-int32-ul key (* key-chunk-size idx)))))
    (aset state 12 block-count)
    (let ((nonce-chunk-size 4))
      (dotimes (idx (/ (length nonce) nonce-chunk-size))
        (aset state (+ 13 idx)
              (elchacha-read-int32-ul nonce (* nonce-chunk-size idx)))))
    state))

(provide 'elchacha)
;;; elchacha.el ends here
