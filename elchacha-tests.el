;;; elchacha-tests.el --- tests for Elchacha -*- lexical-binding: t; -*-

;;; Commentary: Implementation as per RFC7539.

;;; Code:

(require 'ert)
(require 'elchacha)
(setq ert-quiet t)

(ert-deftest elchacha-constants-expand ()
  "https://www.rfc-editor.org/rfc/rfc7539#section-2.3"
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
  "Bit rotation (<<<=)."
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
  "https://www.rfc-editor.org/rfc/rfc7539#section-2.1.1"
  (let ((input-a #x11111111) (input-b #x01020304)
        (input-c #x9B8D6F43) (input-d #x01234567)
        (output-a #xEA2A92F4) (output-b #xCB1CF8CE)
        (output-c #x4581472E) (output-d #x5881C4BB))
    (should (equal (elchacha-quarter-round input-a input-b input-c input-d)
                   (vector output-a output-b output-c output-d)))))

(ert-deftest elchacha-quarter-round-on-state ()
  "https://www.rfc-editor.org/rfc/rfc7539#section-2.2.1"
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
  "ChaCha state with the key setup, rfc7539#section-2.3.2"
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

(ert-deftest elchacha-block ()
  "ChaCha state after 20 rounds, rfc7539#section-2.3.2"
  (let* ((key [#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07
               #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
               #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
               #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f])
         (nonce [#x00 #x00 #x00 #x09 #x00 #x00 #x00 #x4a #x00 #x00 #x00 #x00])
         (block-count 1)
         (init-state (elchacha-state-init key nonce block-count)))
    (should (equal (elchacha-block init-state key nonce block-count)
                   [#x837778AB #xE238D763 #xA67AE21E #x5950BB2F
                    #xC4F2D0C7 #xFC62BB2F #x8FA018FC #x3F5EC7B7
                    #x335271C2 #xF29489F3 #xEABDA8FC #x82E46EBD
                    #xD19C12B4 #xB04E16DE #x9E83D0CB #x4E3C50A2]))))

(ert-deftest elchacha-block-sum ()
  "ChaCha state at the end of the ChaCha20 operation, rfc7539#section-2.3.2"
  (let* ((key [#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07
               #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
               #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
               #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f])
         (nonce [#x00 #x00 #x00 #x09 #x00 #x00 #x00 #x4a #x00 #x00 #x00 #x00])
         (block-count 1))
    (should (equal (elchacha-block-sum key nonce block-count)
                   [#xE4E7F110 #x15593BD1 #x1FDD0F50 #xC47120A3
                    #xC7F4D1C7 #x0368C033 #x9AAA2204 #x4E6CD4C3
                    #x466482D2 #x09AA9F07 #x05D7C214 #xA2028BD9
                    #xD19C12B5 #xB94E16DE #xE883D0CB #x4E3C50A2]))))

(ert-deftest elchacha-block-sum-serialized ()
  "ChaCha state serialized for XORing, rfc7539#section-2.3.2"
  (let* ((key [#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07
               #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
               #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
               #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f])
         (nonce [#x00 #x00 #x00 #x09 #x00 #x00 #x00 #x4a #x00 #x00 #x00 #x00])
         (block-count 1)
         (init-state (elchacha-state-init key nonce block-count)))
    (should (equal (elchacha-block-stream key nonce block-count)
                   [#x10 #xf1 #xe7 #xe4 #xd1 #x3b #x59 #x15
                    #x50 #x0f #xdd #x1f #xa3 #x20 #x71 #xc4
                    #xc7 #xd1 #xf4 #xc7 #x33 #xc0 #x68 #x03
                    #x04 #x22 #xaa #x9a #xc3 #xd4 #x6c #x4e
                    #xd2 #x82 #x64 #x46 #x07 #x9f #xaa #x09
                    #x14 #xc2 #xd7 #x05 #xd9 #x8b #x02 #xa2
                    #xb5 #x12 #x9c #xd1 #xde #x16 #x4e #xb9
                    #xcb #xd0 #x83 #xe8 #xa2 #x50 #x3c #x4e]))))

(provide 'elchacha-tests)
;;; elchacha-tests.el ends here
