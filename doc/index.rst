pytest.el: running pytest from within emacs
===========================================

Heavily inspired by magit, this module provides a interface to
run `pytest` from within emacs. It aims to allow running
the current test, the current test group, the current test file or the
whole test suite using keyboard shortcuts, with plans to provide a
selection window for selecting parametrized tests.

Documentation
-------------

Default keyboard bindings:

* :kbd:`f5`: run the current test
* :kbd:`S-f5`: run the current test group
* :kbd:`C-f5`: run the current test file
* :kbd:`M-f5`: run the whole test suite
