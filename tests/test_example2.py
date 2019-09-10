import pytest


def test_case():
    assert False

class TestGroup:
    def test_pass(self):
        pass

    @pytest.mark.skip
    def test_skip(self):
        pass

    @pytest.mark.xfail
    def test_xfail(self):
        assert False

    @pytest.mark.xfail
    def test_xpass(self):
        pass
