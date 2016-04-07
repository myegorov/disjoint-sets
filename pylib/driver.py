#!/usr/bin/env python3

"""
driver.py

This driver performs all topmost logic: 
    - imports all algorithm modules;
    - runs the algorithms, takes time measurements & counts recursive calls;
    - post-processes the data to create tables and plots;

TODO:
    - add other algorithms
    - add memory profiling

Usage (meant to be run from a build script):
    python3 driver.py
"""

__author__ = "Maksim Yegorov"
__date__ = "2016-04-06 Wed 08:09 PM"

import os.path, sys
import importlib
from plot import plot_scatter
from generate_string import strgen

import naive
import memoized
import dynamic
#import hirschberg


# increase recursion limit
sys.setrecursionlimit(100000)

# set up directory refs
CURDIR = os.path.abspath(os.path.curdir)
FIGDIR = os.path.join(os.path.dirname(CURDIR),'docs/source/figures')
LOGDIR = os.path.join(CURDIR, 'logs')

# alphabets
ALPHAS = {'bin': ['0', '1'],
          'alpha': ['A','C','G','T']}


if __name__ == "__main__":

    ## (1) for each algorithm
    # plot CPU time vs string length for several inputs at each length
    # set legend by length of LCS or alphabet
    str_lens = [10, 100]
    title = 'CPU vs string length: naive algorithm'
    series = []

    set1 = {'x':[], 'y':[]}
    seq1_bin = [strgen(alphabet=ALPHAS['bin'], size=strlen) \
                    for i in range(5) for strlen in str_lens]
    seq2_bin = [strgen(alphabet=ALPHAS['bin'], size=strlen) \
                    for i in range(5) for strlen in str_lens]
    for (str1, str2) in zip(seq1_bin, seq2_bin):
        algo_name, time_elapsed, lcs = naive.lcs_naive(str1, str2)
        set1['x'].append(len(str1))
        set1['y'].append(time_elapsed)
    series.append('binary')

    set2 = {'x':[], 'y':[]} #store CPU vs length
    set3 = {'x':[], 'y':[]} #store CPU vs recursion depth for strlen = 10
    set4 = {'x':[], 'y':[]} #store CPU vs recursion depth for strlen = 100
    seq1_alpha = [strgen(alphabet=ALPHAS['alpha'], size=strlen) \
                    for i in range(5) for strlen in str_lens]
    seq2_alpha = [strgen(alphabet=ALPHAS['alpha'], size=strlen) \
                    for i in range(5) for strlen in str_lens] 

    for (str1, str2) in zip(seq1_alpha, seq2_alpha):
        strlen = len(str1)
        algo_name, time_elapsed, lcs = naive.lcs_naive(str1, str2)
        set2['x'].append(len(str1))
        set2['y'].append(time_elapsed)
        if strlen == 10:
            set3['x'].append(naive.registry['_lcs_naive'])
            set3['y'].append(time_elapsed)
        elif strlen == 100:
            set4['x'].append(naive.registry['_lcs_naive'])
            set4['y'].append(time_elapsed)
    series.append('alphabetic')
    plot_scatter(set1, set2, series, title, xlabel = 'length of string', \
                ylabel = 'CPU time (sec)')


    # (2) plot CPU time vs number of recursive invocations for several lengths
    plot_scatter(set3, set4, ['size 10', 'size 100'], \
            title = 'CPU vs recursion depth', \
            xlabel = 'number of recursive invocations',\
            ylabel = 'CPU time (sec)')

    # (3) TODO: plot memory use vs string length for several inputs at each length


    ## (4) algorithm comparison
    # for given string, plot CPU time for each algorithm
    # do this for several strings (vary length and alphabet)
    title = 'CPU vs input length: memoized vs dynamic'
    str1 = strgen(alphabet=ALPHAS['alpha'], size=1000)
    str2 = strgen(alphabet=ALPHAS['alpha'], size=1000)

    set1 = {'x':[], 'y':[]} #store CPU vs length: memoized
    labels = []
    algo_name, time_elapsed, lcs_len = memoized.lcs_memoized(str1, str2)
    set1['x'].append(len(str1))
    set1['y'].append(time_elapsed)
    labels.append(algo_name)

    set2 = {'x':[], 'y':[]} #store CPU vs length: bottom-up dynamic
    algo_name, time_elapsed, lcs_len = dynamic.lcs_bottomup(str1, str2)
    set2['x'].append(len(str1))
    set2['y'].append(time_elapsed)
    labels.append(algo_name)
    plot_scatter(set1, set2, labels, title, \
                xlabel = 'length of string', \
                ylabel = 'CPU time (sec)')

    
    ## (5) show time for strlen == 40000
    title = 'CPU vs input length: dynamic only'
    str1 = strgen(alphabet=ALPHAS['alpha'], size=40000)
    str2 = strgen(alphabet=ALPHAS['alpha'], size=40000)

    set1 = {'x':[], 'y':[]} #store CPU vs length: dynamic
    labels = []
    algo_name, time_elapsed, lcs_len = dynamic.lcs_bottomup(str1, str2)
    set1['x'].append(len(str1))
    set1['y'].append(time_elapsed)
    labels.append(algo_name)

    plot_scatter(set1, [], labels, title, \
                xlabel = 'length of string', \
                ylabel = 'CPU time (sec)')
