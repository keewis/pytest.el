import pytest

pytestmark = [pytest.mark.skipif(True, reason="skip for demonstration purposes")]


def test_pass():
    pass
