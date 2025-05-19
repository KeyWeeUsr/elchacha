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

(provide 'elchacha)
;;; elchacha.el ends here
