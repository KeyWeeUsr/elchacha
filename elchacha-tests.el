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

(ert-deftest chacha20-quarter-round-on-state ()
  (let ((state [#x879531E0 #xC5ECF37D #x516461B1 #xC9A62F8A
                #x44C20EF3 #x3390AF7F #xD9FC690B #x2A5F714C
                #x53372767 #xB00A5631 #x974C541A #x359E9963
                #x5C971061 #x3D631689 #x2098D9D6 #x91DBD320]))
    (should (equal (elchacha-quarter-round-on state 2 7 8 13)
                   [#x879531E0 #xC5ECF37D #xBDB886DC #xC9A62F8A
                    #x44C20EF3 #x3390AF7F #xD9FC690B #xCFACAFD2
                    #xE46BEA80 #xB00A5631 #x974C541A #x359E9963
                    #x5C971061 #xCCC07C79 #x2098D9D6 #x91DBD320]))))

(ert-deftest elchacha-state ()
  (let ((key [#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07
              #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
              #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
              #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f])
        (nonce [#x00 #x00 #x00 #x09 #x00 #x00 #x00 #x4a #x00 #x00 #x00 #x00])
        (block-count 1))
    (should (equal (elchacha-state-init key nonce block-count)
                   [#x61707865 #x3320646E #x79622D32 #x6B206574
                    #x03020100 #x07060504 #x0B0A0908 #x0F0E0D0C
                    #x13121110 #x17161514 #x1B1A1918 #x1F1E1D1C
                    #x00000001 #x09000000 #x4A000000 #x00000000]))))

(provide 'elchacha-tests)
;;; elchacha-tests.el ends here
