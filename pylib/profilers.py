 #!/usr/bin/env python3

"""
profilers.py

This module defines decorators that are designed to wrap various LCS
algorithsm to return not just length of LCS or LCS, but also
run time of the algorithm and/or count of the number of function
invocations.

Fabian Pedregosa's memory_profiler module is used for memory
profiling:
https://pypi.python.org/pypi/memory_profiler

Usage:
    python3 profilers.py
"""

__author__ = "Maksim Yegorov"
__date__ = "2016-04-28 Thu 02:38 PM"

import time, sys
#from memory_profiler import profile as mem_profiler
from memory_profiler import LineProfiler, show_results
from collections import defaultdict
import os.path

# keep track of recursive function calls
registry = defaultdict(int)

# keep track of memory usage
CURDIR = os.path.abspath(os.path.curdir)
LOGDIR = os.path.join(CURDIR, 'logs')
LOGFILE = os.path.join(LOGDIR, 'memory_profiler.log')
MEMLOG = open(LOGFILE, 'a')

def log_recursion(func):
    """Decorator that counts the number of function
    invocations.

    Args:
        func:   function to be decorated
    Returns:
        decorated func
    Caveats:
        does not account for repeated runs!
    """
    # count number of invocations
    def inner(*args, **kwargs):
        """Increments invocations and returns the
        callable unchanged."""

        registry[func.__name__] += 1
        return func(*args, **kwargs)
    return inner


def time_and_space_profiler(repeat = 1, stream = MEMLOG):
    """Decorator factory that times the function
    invocation. A function is timed over 'repeat' times
    and then runtime is averaged.

    Args:
        repeat (int):   number of repeat runs to average
                            runtime over.
    Returns:
        decorated func (in particular, rutime averaged over
                        number of repeat runs)
    """
    def decorate(func):
        """Decorator.

        Args:
            func:   function to be decorated
        """
        def inner(*args, **kwargs):
            """Sets timer and returns the elapsed time
            and result of original function.

            Returns:
                func.__name__, elapsed_time,
                    original_return_value (tuple)
            """

            mem_profiler = LineProfiler()
            start = time.perf_counter()
            for i in range(repeat):
                return_val = mem_profiler(func)(*args, **kwargs)
            finish = time.perf_counter()
            # log memory usage
            show_results(mem_profiler, stream=stream, precision=1)
            # return amortized average cost per run
            elapsed = (finish - start) / repeat

            return (func.__name__, elapsed, return_val)
        return inner
    return decorate


if __name__ == "__main__":
    """Test the decorators."""

    # (1) test time_profiler, run in a loop 10 times
    @time_and_space_profiler(repeat = 10)
    def snooze(seconds):
        time.sleep(seconds)

    print("\nTesting snooze(0.1234567) x 10")
    name, elapsed, res = snooze(0.1234567)
    print("[%0.7fs] %s(0.1234567)(repeat = 10) -> %r" \
                %(elapsed, name, res))

    # (2) test log_recursion
    @log_recursion
    def recur(num):
        if num < 2:
            return
        else:
            recur(num - 1)

    print("\nTesting recur(10)")
    recur(10)
    print("recur(10) -> %d" %registry['recur'])

    # (3) test both time & recursion depth profiling
    @time_and_space_profiler()
    def outer(num):
        helper(num)

    @log_recursion
    def helper(num):
        if num < 2:
            return
        else:
            helper(num - 1)

    print("\nTesting outer(10)")
    name, elapsed, waste = outer(10)
    print("[%0.7fs] %s(10) -> %d" \
            %(elapsed, name, registry['helper']))

    registry['helper'] = 0
    print("\nSystem recursion limit: ", \
            sys.getrecursionlimit())
    print("Testing outer(100)")
    name, elapsed, waste = outer(100)
    print("[%0.7fs] %s(100) -> %d" \
            %(elapsed, name, registry['helper']))

    #(4) test memory profiling
    @time_and_space_profiler()
    def mem_test():
        a = 'a'
        b = ['a'] * (10**6)
        del b
        return a
    mem_test()
