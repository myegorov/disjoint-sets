#!/bin/sh

python -m cProfile -s ncalls naive.py
python -m cProfile -s ncalls memoized.py
