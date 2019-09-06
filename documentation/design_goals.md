This mode aims to simplify working with pytest from emacs just as
magit does for git. This document is used as a collection of ideas for features.

pytest status window
====================
Mode Name: pytest-status-mode
Buffer Name: `pytest: <project root>`

Status window displaying the status of the tests in the test suite. It first runs

$ CWD=/project/root python -m pytest --collect-only

to collect the tests and display them organized by package / file
using the characters from pytest's summary:
`-` Test did not run yet (new symbol, not used by pytest)
`.` Test passed
`s` Test was skipped
`x` Test xfailed
`X` Test xpassed
`F` Test failed
`E` Test encountered an error

There, the user can select or disselect the whole suite, one or more
packages, one or more test files, or one or more test cases (or one or
more variations of the test case). A custom filter (like selecting a
test group or the variations of a single test case) can also be set.

Shortcuts
---------
`SPC` mark the current symbol / file
`v` mark the other variations of the same case
`f` mark the other cases in the same file
`p` mark all tests in the same package
`c` mark tests of the same category
`C` manually select one or more categories
`a` mark the whole suite
`x` disselect all
`C-/` undo one operation
`RET` view the selected group / highlighted test case
`F5` run selected group

After running, the group stays selected.

test group list
===============
Mode Name: pytest-group-mode
Buffer Name: `pytest-group: <project> - <group name>` (by default `group#xx`)

This is basically the same as the summary buffer, but displays the
selected tests by name in a hierarchical order. Multiple group list buffers may
stay open, thus allowing to examine several runs at once.

Shortcuts
---------
`RET` examine test case (open report buffer)
`r` rename group
`q` close group buffer and delete test data
`a` interactively add one test case
`d`, `DEL` remove one test case

Reports
=======
Mode Name: pytest-report-mode
Buffer Name: `*pytest: <project> — <fqn of test>*`.

Pressing RET on any of the symbols opens this buffer (`q` closes)
where information on errors, warnings and output is displayed (even if
the test passed). Each of these categories may be collapsed, as well
as single errors / warnings. There, the user may also try to rerun the
test.

Shortcuts
---------
`q` close this buffer (information is not deleted)
`F5` (re)run this test (variation)
`TAB` collapse the selected report


Global shortcuts
=========
The common shortcut for opening the status window should be
`C-x p`.

python-mode
-----------
`F5` Run the current file
`S-F5` Run the current test case (including all variations)
`C-F5` Run the whole test suite
`M-F5` Rerun previously run tests
`M-S-F5` Rerun previously failed tests (failed and error categories)
