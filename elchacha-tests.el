;;; elchacha-tests.el --- tests for Elchacha -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'elchacha)
(setq ert-quiet t)

(ert-deftest elchacha-constants-expand ()
  (should (equal [#x61707865 #x3320646e #x79622d32 #x6b206574]
                 elchacha-constants)))

(defun format-bin (num)
  "Stringified binary representation of NUM."
  (let ((res ""))
    (while (not (= num 0))
      (setq res (concat (number-to-string (logand num 1)) res))
      (setq num (lsh num -1)))
    (when (string= res "") (setq res "0"))
    res))

(ert-deftest elchacha-rotl ()
  (let ((matrix `((:v #x12345678 :n #x00 :result #x12345678)
                  (:v #x12345678 :n #x20 :result #x12345678)
                  (:v #x12345678 :n #x40 :result #x12345678)
                  (:v #x00000000 :n #x0A :result #x00000000)
                  (:v #x12345678 :n #x01 :result #x2468ACF0)
                  (:v #x12345678 :n #x21 :result #x2468ACF0)
                  (:v #x12345678 :n #x08 :result #x34567812)
                  (:v #xFFFFFFFF :n #x05 :result #xFFFFFFFF)
                  (:v #x12345678 :n #x1F :result #x091A2B3C)
                  (:v #x80000000 :n #x01 :result #x00000001)
                  (:v #x00000001 :n #x01 :result #x00000002)
                  (:v #x12345678 :n ,(* -1 #x05) :result #xc091a2b3))))
    (dolist (case matrix)
      (should (string= (format-bin (plist-get case :result))
                       (format-bin (elchacha-rotate (plist-get case :v)
                                                    (plist-get case :n))))))))

(ert-deftest elchacha-quarter-round-test-vector ()
  (let ((input-a #x11111111) (input-b #x01020304)
        (input-c #x9B8D6F43) (input-d #x01234567)
        (output-a #xEA2A92F4) (output-b #xCB1CF8CE)
        (output-c #x4581472E) (output-d #x5881C4BB))
    (should (equal (elchacha-quarter-round input-a input-b input-c input-d)
                   (vector output-a output-b output-c output-d)))))

(provide 'elchacha-tests)
;;; elchacha-tests.el ends here
