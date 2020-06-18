import unittest
import pytest


def test_case():
    raise ValueError("always fails")


@pytest.mark.skipif(
    False, reason="don't skip the whole group",
)
class TestGroup:
    def test_pass(self):
        pass

    @pytest.mark.skip(reason="a long and elaborate reason on why this test has to fail")
    def test_skip(self):
        pass

    @pytest.mark.xfail
    def test_xfail(self):
        assert False

    @pytest.mark.xfail
    def test_xpass(self):
        pass


class GroupTest(unittest.TestCase):
    def test_pass(self):
        pass

    def test_fail(self):
        raise ValueError("always fails")
