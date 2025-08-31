;;; elchacha.el --- Elisp ChaCha20 implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, elchacha
;; Version: 1.0.3
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

;; This library is a partial implementation of ChaCha20 stream cipher as per
;; RFC7539. The implementation hasn't been tested against any attacks and
;; shouldn't be used as the first choice when using a cryptographic
;; library.
;;
;; The use-case I wrote this library for is a very limited one.  It's intended
;; for a personal use for platforms where using libraries such as OpenSSL is
;; inefficient, impractical or would simply require too much effort and baggage
;; involved for what it's simply supposed to do -- xor some things in a
;; particular way.

;;; Code:

(defconst elchacha-rounds 20
  "Ref: https://www.rfc-editor.org/rfc/rfc7539#section-1.1 .")

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
  "Perform a quarter-round operation on four numbers (A, B, C, D)."
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
  "Perform a quarter-round on the ChaCha20 STATE on nth POSITIONS (4)."
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
    (+ (ash (aref data offset) 0)
       (ash (aref data (1+ offset)) 8)
       (ash (aref data (+ 2 offset)) 16)
       (ash (aref data (+ 3 offset)) 24))))

(defun elchacha-state-init (key nonce &optional block-counter)
  "Initialize cipher state - the matrix - with KEY and NONCE.
Optional argument BLOCK-COUNTER defaults to 0."
  (unless block-counter (setq block-counter 0))
  ;; 32-bit
  (let ((state (make-vector 16 0)))
    (dotimes (idx (length elchacha-constants))
      (aset state (+ 0 idx) (aref elchacha-constants idx)))
    (let ((key-chunk-size 4))
      (dotimes (idx (/ (length key) key-chunk-size))
        (aset state (+ 4 idx)
              (elchacha-read-int32-ul key (* key-chunk-size idx)))))

    (aset state 12 block-counter)

    (let ((nonce-chunk-size 4))
      (dotimes (idx (/ (length nonce) nonce-chunk-size))
        (aset state (+ 13 idx)
              (elchacha-read-int32-ul nonce (* nonce-chunk-size idx))))
      state)))

(defun elchacha-inner-block (state)
  "Calculate column+diagonal quarter rounds on STATE.
Ref: https://www.rfc-editor.org/rfc/rfc7539#section-2.3.1"
  ;; column round
  (elchacha-quarter-round-on state 00 04 08 12)
  (elchacha-quarter-round-on state 01 05 09 13)
  (elchacha-quarter-round-on state 02 06 10 14)
  (elchacha-quarter-round-on state 03 07 11 15)
  ;; diagonal round
  (elchacha-quarter-round-on state 00 05 10 15)
  (elchacha-quarter-round-on state 01 06 11 12)
  (elchacha-quarter-round-on state 02 07 08 13)
  (elchacha-quarter-round-on state 03 04 09 14)
  state)

(defun elchacha-block (init-state)
  "Calculate ChaCha20 state after 20 rounds on INIT-STATE."
  (let ((state (vconcat init-state)))
    (dotimes (_ (/ elchacha-rounds 2) state)
      (setq state (elchacha-inner-block state)))))

(defun elchacha-block-sum (key nonce &optional block-counter)
  "Calculate ChaCha20 operation to the end with KEY and NONCE.
Optional argument BLOCK-COUNTER defaults to 0
Ref: https://www.rfc-editor.org/rfc/rfc7539#section-2.3.2 ."
  (let* ((init-state (elchacha-state-init key nonce block-counter))
         (state (elchacha-block init-state))
         (bound (expt 2 32)))
    (dotimes (idx (length init-state))
      (aset state idx
            (mod (+ (aref init-state idx) (aref state idx)) bound)))
    state))

(defun elchacha-write-int32-ul (number &optional endian)
  "Write an unsigned 32-bit integer from NUMBER and return LE vector.
Optional argument ENDIAN: \\='big | \\='little"
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
  "Serialize ChaCha20 block calculated with KEY and NONCE into byte stream.
Optional argument BLOCK-COUNTER defaults to 0
Ref: https://www.rfc-editor.org/rfc/rfc7539#section-2.3.2"
  (let ((block (elchacha-block-sum key nonce block-counter))
        stream)
    (dotimes (idx (length block))
      (setq stream (vconcat stream (elchacha-write-int32-ul
                                    (aref block idx)))))
    stream))

(defun elchacha-encrypt-decrypt (key nonce data &optional block-counter)
  "Encrypt/decrypt DATA with KEY, NONCE and optional BLOCK-COUNTER (0)."
  (unless key (error "Bad key"))
  (unless nonce (error "Bad nonce"))
  (unless data (error "Bad data"))
  (unless block-counter (setq block-counter 0))

  (unless (= (length key) (/ 256 8))
    (error "Bad key length, should be 256-bit"))

  ;; TODO: Implement me
  (when (= (length nonce) (/ 64 8))
    (error "ChaCha20 for 64-bit nonce not implemented"))
  (when (= (length nonce) (/ 192 8))
    (error "ChaCha20 for 192-bit nonce not implemented"))

  (unless (= (length nonce) (/ 96 8))
    (error "Bad nonce length, should be 96-bit"))

  (when (or (> block-counter (1- (expt 2 32)))
            (< block-counter 0))
    (error "Bad block counter length, should be 32-bit"))

  (when (stringp data)
    (setq data (string-to-vector data)))

  (let* ((block-size 64)
         (data-len (length data))
         head
         (encrypted (make-vector data-len 0))
         (total-blocks (floor (/ data-len (float block-size)))))
    (dotimes (idx total-blocks)
      (let ((key-stream
             (elchacha-block-stream key nonce (+ block-counter idx)))
            (block (make-vector block-size 0)))
        (let* ((start (* idx 64))
               (result (make-vector block-size 0)))
          (dotimes (bidx block-size)
            (aset result bidx (aref data (+ start bidx))))
          (setq block result))

        (dotimes (tmp-idx block-size)
          (unless head (setq head tmp-idx))
          (setf (aref encrypted head)
                (logand (logxor (aref block tmp-idx) (aref key-stream tmp-idx))
                        #xFF))
          (setq head (1+ head)))))

    (unless (= (mod data-len block-size) 0)
      (let* ((key-stream
              (elchacha-block-stream key nonce (+ block-counter total-blocks)))
             (block (make-vector (- data-len (* total-blocks block-size)) 0)))
        (let* ((start (* total-blocks block-size))
               (end data-len))
          (dotimes (idx (- end start))
            (aset block idx (aref data (+ start idx)))))
        (dotimes (idx (length block))
          (unless head (setq head idx))
          (aset encrypted head
                (logand (logxor (aref block idx) (aref key-stream idx)) #xFF))
          (setq head (1+ head)))))
    encrypted))

(provide 'elchacha)
;;; elchacha.el ends here
