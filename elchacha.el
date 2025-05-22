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

;;; Commentary: Implementation as per RFC7539.

;; TBD

;;; Code:

(defconst elchacha-rounds 20
  "https://www.rfc-editor.org/rfc/rfc7539#section-1.1")

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
          d (logand (logxor d a) #xFFFFFFFF)
          d (elchacha-rotate d 16)
          c (mod (+ c d) bound)
          b (logand (logxor b c) #xFFFFFFFF)
          b (elchacha-rotate b 12)
          a (mod (+ a b) bound)
          d (logand (logxor d a) #xFFFFFFFF)
          d (elchacha-rotate d 8)
          c (mod (+ c d) bound)
          b (logand (logxor b c) #xFFFFFFFF)
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
    ;; TODO: possible little endian conversion?
    ;; (let ((counter (elchacha-write-int32-ul block-count)))
    ;;   (dotimes (idx (length counter))
    ;;     (aset state (+ 12 idx) (aref counter idx))))
    (aset state 12 block-count)
    (let ((nonce-chunk-size 4))
      (dotimes (idx (/ (length nonce) nonce-chunk-size))
        (aset state (+ 13 idx)
              (elchacha-read-int32-ul nonce (* nonce-chunk-size idx)))))
    state))

(defun elchacha-inner-block (state)
  (elchacha-quarter-round-on state 00 04 08 12)
  (elchacha-quarter-round-on state 01 05 09 13)
  (elchacha-quarter-round-on state 02 06 10 14)
  (elchacha-quarter-round-on state 03 07 11 15)
  (elchacha-quarter-round-on state 00 05 10 15)
  (elchacha-quarter-round-on state 01 06 11 12)
  (elchacha-quarter-round-on state 02 07 08 13)
  (elchacha-quarter-round-on state 03 04 09 14)
  state)

(defun elchacha-block (init-state key nonce &optional block-counter)
  "Compute ChaCha20 block with KEY, NONCE and BLOCK-COUNTER."
  (let ((state (vconcat init-state)))
    (dotimes (_ (/ elchacha-rounds 2))
      (setq state (elchacha-inner-block state)))
    state))

(defun elchacha-block-sum (key nonce &optional block-counter)
  (let* ((init-state (elchacha-state-init key nonce block-counter))
         (state (elchacha-block init-state key nonce block-counter))
         (bound (expt 2 32)))
    (dotimes (idx (length init-state))
      (aset state idx (mod (+ (aref init-state idx) (aref state idx)) bound)))
    state))

(defun elchacha-write-int32-ul (number &optional endian)
  "Write an unsigned 32-bit integer and return LE vector."
  (unless endian
    (setq endian 'little))
  (let ((bytes (make-vector (/ 32 8) 0)))
    (cond ((eq endian 'little)
           (setf (aref bytes 0) (logand number #xFF))
           (setf (aref bytes 1) (logand (ash number -8) #xFF))
           (setf (aref bytes 2) (logand (ash number -16) #xFF))
           (setf (aref bytes 3) (logand (ash number -24) #xFF))
           bytes)
          ((eq endian 'big)
           (setf (aref bytes 0) (logand (ash number -24) #xFF))
           (setf (aref bytes 1) (logand (ash number -16) #xFF))
           (setf (aref bytes 2) (logand (ash number -8) #xFF))
           (setf (aref bytes 3) (logand number #xFF)))
          (t (error "Unexpected endian value! ('little, 'big)")))))

(defun elchacha-block-stream (key nonce &optional block-counter)
  (let ((block (elchacha-block-sum key nonce block-counter))
        stream)
    (dotimes (idx (length block))
      (setq stream (vconcat stream (elchacha-write-int32-ul
                                    (aref block idx)))))
    stream))

(defun elchacha-encrypt-decrypt (key nonce data &optional block-counter)
  (unless key (error "Bad key"))
  (unless nonce (error "Bad nonce"))
  (unless data (error "Bad data"))
  (unless block-counter (setq block-counter 0))

  (unless (= (length key) (/ 256 8))
    (error "Bad key length, should be 256-bit"))

  ;; TODO: Implement me
  (when (= (length nonce) (/ 64 8))
    (error "ChaCha20 for 64-bit nonce not implemented."))
  (when (= (length nonce) (/ 192 8))
    (error "ChaCha20 for 192-bit nonce not implemented."))

  (unless (= (length nonce) (/ 96 8))
    (error "Bad nonce length, should be 96-bit"))

  (when (or (> block-counter (1- (expt 2 32)))
            (< block-counter 0))
    (error "Bad block counter length, should be 32-bit"))

  (when (stringp data)
    (setq data (string-to-vector data)))

  (let* ((block-size 64)
         (data-len (length data))
         encrypted
         (total-blocks (floor (/ data-len (float block-size)))))
    (dotimes (idx total-blocks)
      (let ((key-stream (elchacha-block-stream key nonce (+ block-counter idx)))
            (block (make-vector block-size 0)))
        (let* ((start (* idx 64))
               (end (* (1+ idx) 64))
               (result (make-vector block-size 0)))
          (dotimes (bidx block-size)
            (aset result bidx (aref data (+ start bidx))))
          (setq block result))

        (let ((tmp (make-vector block-size 0)))
          (dotimes (tmp-idx block-size tmp)
            (setf (aref tmp tmp-idx)
                  (logand (logxor (aref block tmp-idx)
                                  (aref key-stream tmp-idx))
                          #xFF)))
          (setq encrypted (vconcat encrypted tmp)))))

    (unless (= (mod data-len block-size) 0)
      (let* ((idx total-blocks)
             (key-stream (elchacha-block-stream key nonce (+ block-counter idx)))
             (block (make-vector (- data-len (* idx block-size)) 0)))
        (let* ((start (* idx block-size))
               (end data-len))
          (dotimes (bidx (- end start))
            (aset block bidx (aref data (+ start bidx)))))
        (let ((tmp (make-vector (length block) 0)))
          (dotimes (bidx (length block))
            (aset tmp bidx (logand (logxor (aref block bidx)
                                           (aref key-stream bidx))
                                  #xFF)))
          (setq encrypted (vconcat encrypted tmp)))))
    encrypted))

(provide 'elchacha)
;;; elchacha.el ends here
