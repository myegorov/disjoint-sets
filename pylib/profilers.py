 #!/usr/bin/env python3

"""
profilers.py

This module defines decorators used in timing/counting number of function
invocations.

Usage:
    python3 profilers.py

TODO:
    - test len_recursion
    - memory profiling

"""

__author__ = "Maksim Yegorov"
__date__ = "2016-04-06 Wed 03:56 PM"

import time

def len_recursion(func):
    """Decorator that counts the number of function invocations.

    Args:
        func:   decorated function
    Returns:
        decorated func that keeps number of invocations in scope
    """
    # count number of invocations
    invocations = 0

    def inner(*args, **kwargs):
        """Increments invocations and returns the callable unchanged."""

        nonlocal invocations
        invocations += 1
        return func(*args, **kwargs)
    return inner


def time_profiler(func):
    """Decorator that times the function invocation. Outputs

    Args:
        func:   decorated function
    Returns:
        decorated func
    """
    def inner(*args, **kwargs):
        """Sets timer and returns the elapsed time and result of original
        function.
        
        Returns:
            func.__name__, elapsed_time, original_return_value (tuple)
        """

        start = time.perf_counter()
        return_val = func(*args, **kwargs)
        finish = time.perf_counter()
        elapsed = finish - start

        return (func.__name__, elapsed, return_val)
    return inner


if __name__ == "__main__":
    """Test the decorators."""

    # test time_profiler
    @time_profiler
    def snooze(seconds):
        time.sleep(seconds)

    print("Testing snooze(0.1234567)")
    name, elapsed, res = snooze(0.1234567)
    print("[%0.7fs] %s() -> %r" %(elapsed, name, res))
