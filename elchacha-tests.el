;;; elchacha-tests.el --- tests for Elchacha -*- lexical-binding: t; -*-

;;; Commentary: Implementation as per RFC7539.

;;; Code:

(require 'ert)
(require 'elchacha)
(setq ert-quiet t)

(ert-deftest elchacha-constants-expand ()
  "Ref: https://www.rfc-editor.org/rfc/rfc7539#section-2.3 ."
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
  "Ref: https://www.rfc-editor.org/rfc/rfc7539#section-2.1.1 ."
  (let ((input-a #x11111111) (input-b #x01020304)
        (input-c #x9B8D6F43) (input-d #x01234567)
        (output-a #xEA2A92F4) (output-b #xCB1CF8CE)
        (output-c #x4581472E) (output-d #x5881C4BB))
    (should (equal (elchacha-quarter-round input-a input-b input-c input-d)
                   (vector output-a output-b output-c output-d)))))

(ert-deftest elchacha-quarter-round-on-state ()
  "Ref: https://www.rfc-editor.org/rfc/rfc7539#section-2.2.1 ."
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
  "ChaCha state with the key setup, rfc7539#section-2.3.2 ."
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
  "ChaCha state after 20 rounds, rfc7539#section-2.3.2 ."
  (let* ((key [#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07
               #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
               #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
               #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f])
         (nonce [#x00 #x00 #x00 #x09 #x00 #x00 #x00 #x4a #x00 #x00 #x00 #x00])
         (block-count 1)
         (init-state (elchacha-state-init key nonce block-count)))
    (should (equal (elchacha-block init-state)
                   [#x837778AB #xE238D763 #xA67AE21E #x5950BB2F
                    #xC4F2D0C7 #xFC62BB2F #x8FA018FC #x3F5EC7B7
                    #x335271C2 #xF29489F3 #xEABDA8FC #x82E46EBD
                    #xD19C12B4 #xB04E16DE #x9E83D0CB #x4E3C50A2]))))

(ert-deftest elchacha-block-sum ()
  "ChaCha state at the end of the ChaCha20 operation, rfc7539#section-2.3.2 ."
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
  "ChaCha state serialized for XORing, rfc7539#section-2.3.2 ."
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

(ert-deftest elchacha-encrypt-decrypt-from-rfc ()
  "Ref: https://www.rfc-editor.org/rfc/rfc7539#section-2.4.2 ."
  (let* ((msg-bytes [#x4c #x61 #x64 #x69 #x65 #x73 #x20 #x61
                     #x6e #x64 #x20 #x47 #x65 #x6e #x74 #x6c
                     #x65 #x6d #x65 #x6e #x20 #x6f #x66 #x20
                     #x74 #x68 #x65 #x20 #x63 #x6c #x61 #x73
                     #x73 #x20 #x6f #x66 #x20 #x27 #x39 #x39
                     #x3a #x20 #x49 #x66 #x20 #x49 #x20 #x63
                     #x6f #x75 #x6c #x64 #x20 #x6f #x66 #x66
                     #x65 #x72 #x20 #x79 #x6f #x75 #x20 #x6f
                     #x6e #x6c #x79 #x20 #x6f #x6e #x65 #x20
                     #x74 #x69 #x70 #x20 #x66 #x6f #x72 #x20
                     #x74 #x68 #x65 #x20 #x66 #x75 #x74 #x75
                     #x72 #x65 #x2c #x20 #x73 #x75 #x6e #x73
                     #x63 #x72 #x65 #x65 #x6e #x20 #x77 #x6f
                     #x75 #x6c #x64 #x20 #x62 #x65 #x20 #x69
                     #x74 #x2e])
         (key-bytes [#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07
                     #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
                     #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17
                     #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f])
         (nonce-bytes [#x00 #x00 #x00 #x00 #x00 #x00
                       #x00 #x4a #x00 #x00 #x00 #x00])
         (counter 1)
         (output [#x6e #x2e #x35 #x9a #x25 #x68 #xf9 #x80
                  #x41 #xba #x07 #x28 #xdd #x0d #x69 #x81
                  #xe9 #x7e #x7a #xec #x1d #x43 #x60 #xc2
                  #x0a #x27 #xaf #xcc #xfd #x9f #xae #x0b
                  #xf9 #x1b #x65 #xc5 #x52 #x47 #x33 #xab
                  #x8f #x59 #x3d #xab #xcd #x62 #xb3 #x57
                  #x16 #x39 #xd6 #x24 #xe6 #x51 #x52 #xab
                  #x8f #x53 #x0c #x35 #x9f #x08 #x61 #xd8
                  #x07 #xca #x0d #xbf #x50 #x0d #x6a #x61
                  #x56 #xa3 #x8e #x08 #x8a #x22 #xb6 #x5e
                  #x52 #xbc #x51 #x4d #x16 #xcc #xf8 #x06
                  #x81 #x8c #xe9 #x1a #xb7 #x79 #x37 #x36
                  #x5a #xf9 #x0b #xbf #x74 #xa3 #x5b #xe6
                  #xb4 #x0b #x8e #xed #xf2 #x78 #x5e #x42
                  #x87 #x4d])
         (encrypted
          (elchacha-encrypt-decrypt key-bytes nonce-bytes msg-bytes counter))
         (decrypted
          (elchacha-encrypt-decrypt key-bytes nonce-bytes encrypted counter)))
    (should (equal encrypted output))
    (should (equal decrypted msg-bytes))))

(defun sha256 (thing)
  (string-to-vector (secure-hash 'sha256 thing nil nil t)))

(defun sha256file (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (sha256 (buffer-string))))

(ert-deftest elchacha-local-check ()
  (unless (getenv "ELCHACHA_PYTHON")
    (ert-skip "Local check"))
  (let* ((source "1.0.0")
         (key "This is a super-secret key, yes.")
         (nonce "abcdefghijkl")
         (key-bytes (string-to-vector key))
         (nonce-bytes (string-to-vector nonce))
         (tmp-name (make-temp-name "elchacha-"))
         (pre-check [#xfa #x28 #x0b #x08 #x9d #x34 #x90 #x77
                     #x3c #x27 #xac #xf8 #x98 #x77 #x5b #x3b
                     #xf1 #x47 #xf5 #xff #x1c #xd4 #xae #xec
                     #x9f #x75 #x5b #xef #x8c #x29 #x9b #x3d])
         (post-check [#x13 #xf8 #x79 #xb2 #x2a #xe1 #x75 #xb2
                      #xc1 #x6b #x6c #x56 #x1d #x24 #x77 #x5c
                      #xac #x5a #x38 #x67 #xdf #xa8 #x56 #x3d
                      #x13 #x94 #xbb #x52 #x60 #x66 #x13 #x21])
         (post-check-hex (apply 'concat (mapcar (lambda (x) (format "%02x" x))
                                                post-check)))
         (tests '(:elchacha :openssl :pycryptodome))
         (cases `(:elchacha
                  'noop
                  :openssl
                  ,(format "openssl enc -chacha20 -K %s -iv %s -in %s -out %s"
                           (apply 'concat (mapcar (lambda (x) (format "%0X" x))
                                                  key-bytes))
                           (concat "00000000"
                                   (apply 'concat
                                          (mapcar (lambda (x) (format "%0X" x))
                                                  nonce-bytes)))
                           tmp-name
                           "-|sha256sum")
                  :pycryptodome
                  ,(string-join
                    `("import Crypto.Cipher.ChaCha20 as C"
                      "import hashlib as h"
                      "s=h.sha256"
                      ,(format (concat "print("
                                       "s(C.new(key=b'%s', nonce=b'%s')"
                                       ".encrypt(open('%s', 'rb').read()))"
                                       ".hexdigest(), end='')")
                               key nonce tmp-name))
                    ";")))
         msg-bytes)
    (unless (file-exists-p source)
      (url-copy-file
       (format "https://github.com/KeyWeeUsr/elchacha/zipball/%s" source)
       source t))
    (dolist (test tests)
      (unwind-protect
          (progn
            (copy-file source tmp-name t)
            (cond ((eq test :elchacha)
                   (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally tmp-name)
                     (should (equal (sha256 (buffer-string)) pre-check))

                     (setq msg-bytes (elchacha-encrypt-decrypt
                                      key-bytes nonce-bytes
                                      (buffer-string) 0)))
                   (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (dotimes (idx (length msg-bytes))
                       (insert (aref msg-bytes idx)))
                     (should (equal (sha256 (buffer-string)) post-check))))
                  ((eq test :openssl)
                   (let ((raw (sha256file tmp-name))
                         (encrypted (shell-command-to-string
                                     (plist-get cases test))))
                     (should (equal raw pre-check))
                     (should (equal (car (split-string encrypted " "))
                                    post-check-hex))))
                  ((eq test :pycryptodome)
                   (with-temp-file (format "%s.py" tmp-name)
                     (insert (plist-get cases test)))
                   (let ((raw (sha256file tmp-name))
                         (encrypted (shell-command-to-string
                                     (format "%s \"%s.py\""
                                             (getenv "ELCHACHA_PYTHON")
                                             tmp-name))))
                     (should (equal raw pre-check))
                     (should (equal (car (split-string encrypted " "))
                                    post-check-hex))))))
        (when (file-exists-p tmp-name)
          (delete-file tmp-name))
        (when (file-exists-p (format "%s.py" tmp-name))
          (delete-file (format "%s.py" tmp-name)))
        (setq msg-bytes nil)))))

(ert-deftest elchacha-encrypt-decrypt-without-blocks ()
  (let ((expected "My_Password")
        (key [#x8A #xD7 #x07 #x71 #x0A #x9E #xD6 #x1F
              #x87 #x87 #x2D #x4C #x1C #xA6 #x8A #x09
              #xFB #x5E #x24 #x8A #x02 #x94 #x7D #xA6
              #x0A #x06 #x47 #x92 #x16 #x10 #xFC #x44])
        (nonce [#xE8 #xAC #x40 #x7A #x79 #x61
                #x55 #x3A #xB2 #xCC #xB2 #x0D])
        (data [#xAA #x21 #x72 #x7A #x99 #x9B #xDE #x2F #x36 #x12 #x27]))
    (should
     (string= expected
              (apply 'string
                     (mapcar (lambda (x) x)
                             (elchacha-encrypt-decrypt key nonce data)))))))

(provide 'elchacha-tests)
;;; elchacha-tests.el ends here
