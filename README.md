[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![Release](https://img.shields.io/github/tag/the-flx/cl-flx.svg?label=release&logo=github)](https://github.com/the-flx/cl-flx/releases/latest)
[![](https://api.quickdocs.org/badge/cl-flx.svg)](https://quickdocs.org/cl-flx)

# cl-flx
> Rewrite emacs-flx in Common Lisp

[![CI](https://github.com/the-flx/cl-flx/actions/workflows/test.yml/badge.svg)](https://github.com/the-flx/cl-flx/actions/workflows/test.yml)

This doesn't count as a rewrite since the original [Emacs][]' [flx][]
written in Emacs Lisp are very close to Common Lisp.
I only need to make tiny changes, which I consider effortless.

## 🔨 Usage

```lisp
(flx:score "buffer-file-name" "bfn")  ; '(237 0 7 12)
```

## 🛠 Development

To run tests, you need [Qob][] and execute the following commands:

```sh
$ qob install-deps --dev
$ qob test
```

## ⚜️ License

`cl-flx` is distributed under the terms of the MIT license.

See [`LICENSE`](./LICENSE) for details.


<!-- Links -->

[flx]: https://github.com/lewang/flx
[Emacs]: https://www.gnu.org/software/emacs/

[Qob]: https://github.com/cl-qob/cli
