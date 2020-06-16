pytest.el: run pytest in emacs
==============================

`pytest.el` allows running pytest from within emacs.

Note that there are two more mature packages:
- [pytest-el](https://github.com/ionrock/pytest-el) (the name clash is
  not intentional, see #1)
- [emacs-python-pytest](https://github.com/wbolster/emacs-python-pytest)

Features include:
* running tests on
  - the whole test suite
  - single test files (including the current buffer)
  - particular test cases (including the currently highlighted)
* saving in buffers based on the selected test
* a minor mode for the output, allowing rerunning the buffer

Installing
----------
There currently is no (robust) installation process whatsoever, but
adding `$(prefix)/pytest.el/lisp` to `load-path` works for now.
