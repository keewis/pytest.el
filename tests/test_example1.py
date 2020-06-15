import warnings
import pytest

from example import example

def warn():
    warnings.warn("warning emitted", UserWarning)


@pytest.fixture
def failing():
    assert False

def test_pass():
    warn()
    assert True

def test_fail():
    warn()
    assert False

def test_error(failing):
    warn()

@pytest.mark.xfail
def test_xfail():
    warn()
    assert False

@pytest.mark.xfail
def test_xpass():
    warn()
    assert True

@pytest.mark.skip
def test_skip():
    warn()

@pytest.fixture(params=[
    pytest.param(0),
    pytest.param(1),
    pytest.param(2),
    pytest.param(3, marks=pytest.mark.xfail),
    pytest.param(4, marks=pytest.mark.xfail),
    pytest.param(5, marks=pytest.mark.skip),
])
@pytest.mark.xfail
@pytest.mark.skip
def variable(request):
    assert request.param != 2
    return request.param

def test_variations(variable):
    assert example(variable)
