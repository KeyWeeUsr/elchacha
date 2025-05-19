;;; elchacha-tests.el -- tests for Elchacha

;;; Code:

(require 'ert)
(require 'elchacha)
(setq ert-quiet t)

(ert-deftest elchacha-constants-expand ()
  (should (equal [#x61707865 #x3320646e #x79622d32 #x6b206574]
                 elchacha-constants)))

(provide 'elchacha-tests)
;;; elchacha-tests.el ends here
