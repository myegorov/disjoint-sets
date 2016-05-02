#!/usr/bin/env python3

"""
driver.py

This driver performs all topmost logic:
    - imports all algorithm modules;
    - runs the algorithms, takes time measurements & counts recursive calls;
    - post-processes the data to create tables and plots;

Usage (meant to be run from a build script):
    python3 driver.py

TODO:
    - plot results collected in experiment list of dicts
"""

__author__ = "Maksim Yegorov"
__date__ = "2016-05-01 Sun 09:28 PM"

import os, sys
import importlib
from plot import plot_scatter
import csv
from generate_string import strgen

import naive
import memoized
import dynamic
import hirschberg


# increase recursion limit
sys.setrecursionlimit(100000)

# set up directory refs
CURDIR = os.path.abspath(os.path.curdir)
FIGDIR = os.path.join(os.path.dirname(CURDIR),\
            'docs/source/figures')
RESULTS = os.path.join(FIGDIR, 'results.csv')

# alphabets
ALPHAS = {'bin':   ['0', '1'],
          'alpha': ['A','C','G','T']}

# lengths of strings to consider
LENGTHS = {#'naive':      [5, 10, 15, 20],
           'naive':      [5, 10], # used for debugging
           'hirschberg': [100], # used for debugging
           'dynamic': [100], # used for debugging
           #'memoized':   [1000, 2000, 3000, 4000, 5000, 10000],
           #'dynamic':    [1000, 2000, 3000, 4000, 5000, 10000],
           #'hirschberg': [1000, 2000, 3000, 4000, 5000, 10000,\
           #                 20000, 30000, 40000, 50000, 60000]
            }

# key to memory log: line numbers to parse
LOG_LINES = {'memoized': {'size':['41', '49']},
            'dynamic': {'size':['39', '46']},
            'hirschberg': {'lcs':['102', '116']}
            }

MODULES = {
        'naive': naive,
        'memoized': memoized,
        'dynamic': dynamic,
        'hirschberg': hirschberg}

def parse_log(memlog, algorithm, target):
    start = LOG_LINES[algorithm][target][0]
    end = LOG_LINES[algorithm][target][1]
    missing_start = True
    missing_end = True

    for line in memlog.split('\n'):
        toks = line.split()
        if len(toks) > 1 and toks[0] == start and \
            missing_start:
            missing_start = False
            start_val = float(toks[1])
        elif len(toks) > 1  and toks[0] == end and \
                missing_end:
            missing_end = False
            end_val = float(toks[1])

    if (missing_start or missing_end):
        print('tried parsing mem log for: ' + algorithm)
        sys.exit('failed to parse memory log')
    else:
        return (end_val - start_val)

def run_experiments():

    # create a library of strings for each alphabet
    # on which algos will be tested:
    # dict(1='z', 3='yzx',...)
    test_lengths = \
        set([l for key in LENGTHS.keys() for l in LENGTHS[key]])
    strings_alpha = {l:[strgen(alphabet=ALPHAS['alpha'], size=l), \
                        strgen(alphabet=ALPHAS['alpha'], size=l)] \
                        for l in test_lengths}
    strings_bin = {l:[strgen(alphabet=ALPHAS['bin'], size=l), \
                    strgen(alphabet=ALPHAS['bin'], size=l)] \
                    for l in test_lengths}

    # list of experimental results (list of dicts)
    experiment = []

    # run tests for each algo for either alphabet
    for algorithm in LENGTHS.keys():
        module = MODULES[algorithm]
        for str_len in LENGTHS[algorithm]:
            for alphabet in ALPHAS.keys():
                if alphabet == 'bin':
                    strings = strings_bin
                else:
                    strings = strings_alpha

                # build up a table of LCS lengths
                if algorithm != 'naive':
                    algo_size, time_size, memlog_size, lcs_table = \
                            module.tabulate_lcs(strings[str_len][0],
                                    strings[str_len][1])
                    match = module.size_lcs(lcs_table)
                    recursion_depth_size = \
                            module.registry['_tabulate_lcs']
                    if algorithm != 'hirschberg':
                        space_size = parse_log(memlog_size,
                                                algorithm,
                                                'size')
                    else: # algorithm == 'hirschberg'
                        space_size = None # negligible for vector
                else:   # algorithm == 'naive'
                    time_size = None
                    space_size = None
                    recursion_depth_size = None

                # reconstruct actual LCS
                if algorithm in ('naive', 'hirschberg'):
                    algo_lcs, time_lcs, memlog_lcs, lcs = \
                            module.reconstruct_lcs(strings[str_len][0],
                                    strings[str_len][1])

                else:
                    algo_lcs, time_lcs, memlog_lcs, lcs = \
                        module.reconstruct_lcs(strings[str_len][0],
                                    strings[str_len][1],
                                    lcs_table,
                                    match)
                recursion_depth_lcs = \
                        module.registry['_reconstruct_lcs']

                if algorithm == 'naive':
                    space_lcs = None # not tracking for short strings
                    match = len(lcs)
                elif algorithm in ('memoized', 'dynamic'):
                    space_lcs = None
                else:
                    space_lcs = parse_log(memlog_lcs,
                                            algorithm,
                                            'lcs')

                experiment.append({ \
                    'algo':algorithm,
                    'alphabet':alphabet,
                    'time_sizing':time_size,
                    'time_reconstruct':time_lcs,
                    'space_sizing':space_size, # not reliable
                    'space_reconstruct':space_lcs,
                    'input_size':str_len,
                    'match_size':match,
                    'recursion_sizing':recursion_depth_size,
                    'recursion_reconstruct': recursion_depth_lcs})

    return experiment

if __name__ == "__main__":

    experiment = run_experiments()

    # write data to file
    header = experiment[0].keys()
    with open(RESULTS, 'w') as csvfile:
        dict_writer = csv.DictWriter(csvfile, fieldnames=header)
        dict_writer.writeheader()
        dict_writer.writerows(experiment)

    # TODO: plot data to .ps files








#     ## (1) for each algorithm
#     # plot CPU time vs string length for several
#     # inputs at each length
#     # set legend by length of LCS or alphabet
#     str_lens = [10, 20]
#     title = 'CPU vs string length: naive algorithm'
#     series = []

#     set1 = {'x':[], 'y':[]}
#     seq1_bin = [strgen(alphabet=ALPHAS['bin'], \
#                 size=strlen) for i in range(5) \
#                 for strlen in str_lens]
#     seq2_bin = [strgen(alphabet=ALPHAS['bin'], \
#                 size=strlen) for i in range(5) \
#                 for strlen in str_lens]
#     for (str1, str2) in zip(seq1_bin, seq2_bin):
#         algo_name, time_elapsed, lcs = \
#                 naive.lcs_naive(str1, str2)
#         set1['x'].append(len(str1))
#         set1['y'].append(time_elapsed)
#     series.append('binary')

#     #store CPU vs length
#     set2 = {'x':[], 'y':[]}
#     set3 = {'x':[], 'y':[]}
#     set4 = {'x':[], 'y':[]}
#     seq1_alpha = [strgen(alphabet=ALPHAS['alpha'], \
#                     size=strlen) for i in range(5) \
#                     for strlen in str_lens]
#     seq2_alpha = [strgen(alphabet=ALPHAS['alpha'], \
#                     size=strlen) for i in range(5) \
#                     for strlen in str_lens]

#     for (str1, str2) in zip(seq1_alpha, seq2_alpha):
#         strlen = len(str1)
#         algo_name, time_elapsed, lcs = \
#                 naive.lcs_naive(str1, str2)
#         set2['x'].append(len(str1))
#         set2['y'].append(time_elapsed)
#         if strlen == 10:
#             set3['x'].append(\
#                     naive.registry['_lcs_naive'])
#             set3['y'].append(time_elapsed)
#         elif strlen == 20:
#             set4['x'].append(\
#                     naive.registry['_lcs_naive'])
#             set4['y'].append(time_elapsed)
#     series.append('alphabetic')
#     plot_scatter(set1, set2, series, title, \
#             xlabel = 'length of string', \
#             ylabel = 'CPU time (sec)')

#     print("-> done with naive algorithm runs")

#     # (2) plot CPU time vs number of recursive
#     # invocations for several lengths
#     plot_scatter(set3, set4, ['size 10', 'size 20'], \
#             title = 'CPU vs recursion depth', \
#             xlabel = 'number of recursive invocations',\
#             ylabel = 'CPU time (sec)')

#     print("-> done with CPU vs recursion depth plots")

#     # (3) TODO: plot memory use vs string length for
#     # several inputs at each length


#     ## (4) algorithm comparison
#     # for given string, plot CPU time for each
#     # algorithm; do this for several strings
#     # (vary length and alphabet)
#     title = 'CPU vs input length: memoized vs dynamic'
#     str1 = strgen(alphabet=ALPHAS['alpha'], size=1000)
#     str2 = strgen(alphabet=ALPHAS['alpha'], size=1000)

#     set1 = {'x':[], 'y':[]} #store CPU vs length: memoized
#     labels = []
#     algo_name, time_elapsed, lcs_table = \
#             memoized.lcs_memoized(str1, str2)
#     lcs_len = memoized.size_lcs(lcs_table)
#     set1['x'].append(len(str1))
#     set1['y'].append(time_elapsed)
#     labels.append(algo_name)

#     #store CPU vs length: bottom-up dynamic
#     set2 = {'x':[], 'y':[]}
#     algo_name, time_elapsed, lcs_table = \
#             dynamic.lcs_bottomup(str1, str2)
#     lcs_len = size_lcs(lcs_table)
#     set2['x'].append(len(str1))
#     set2['y'].append(time_elapsed)
#     labels.append(algo_name)
#     plot_scatter(set1, set2, labels, title, \
#                 xlabel = 'length of string', \
#                 ylabel = 'CPU time (sec)')


#     print("-> done with CPU vs length plots")

#     ## (5) show time for strlen == 40000
#     title = 'CPU vs input length: dynamic only'
#     str1 = strgen(alphabet=ALPHAS['alpha'], \
#             size=10000)
#     str2 = strgen(alphabet=ALPHAS['alpha'], \
#             size=10000)

#     set1 = {'x':[], 'y':[]} #store CPU vs length: dynamic
#     labels = []
#     algo_name, time_elapsed, lcs_len = \
#             dynamic.lcs_bottomup(str1, str2)
#     set1['x'].append(len(str1))
#     set1['y'].append(time_elapsed)
#     labels.append(algo_name)

#     plot_scatter(set1, [], labels, title, \
#                 xlabel = 'length of string', \
#                 ylabel = 'CPU time (sec)')


#     print("-> done with plot for dynamic @ 10K long string")
