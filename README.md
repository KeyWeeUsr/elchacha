# elchacha
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

ChaCha20 implementation in ELisp.

## How to

Clone and install manually, then:

1. `(require 'elchacha)`
2. `(elchacha-encrypt-decrypt key nonce data)`

Note that the implementation is obviously slower than with compiled languages
and, for example, running an `elisp-manual-21-2.8.tar.gz` (2455995 bytes)
through `elchacha-encrypt-decrypt` took 30s while with OpenSSL or PyCryptodome
it took under one second.

While there might be performance bottlenecks in the current implementation, if
you are looking for speed, there are better and safer implementations.

[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
[ci-badge]: https://github.com/KeyWeeUsr/elchacha/actions/workflows/test.yml/badge.svg
[ci-workflow]: https://github.com/KeyWeeUsr/elchacha/actions/workflows/test.yml
[cover-badge]: https://coveralls.io/repos/github/KeyWeeUsr/elchacha/badge.svg?branch=master
[cover-link]: https://coveralls.io/github/KeyWeeUsr/elchacha?branch=master
