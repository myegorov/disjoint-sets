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
__date__ = "2016-04-30 Sat 12:07 AM"

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
#LOGDIR = os.path.join(CURDIR, 'logs')
#MEMPROFILE = os.path.join(LOGDIR, 'memory_profiler.log')
RESULTS = os.path.join(FIGDIR, 'results.csv')

# alphabets
ALPHAS = {'bin':   ['0', '1'],
          'alpha': ['A','C','G','T']}

# lengths of strings to consider
LENGTHS = {#'naive':      [5, 10, 15, 20],
           'naive':      [5, 10], # used for debugging
           'hirschberg': [1000], # used for debugging
           'dynamic': [1000], # used for debugging
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

def parse_log(memlog, algorithm, target):
        #print('looking at: ' + algorithm + ' ' + target)
    #with open(MEMPROFILE, 'r') as log:
        start = LOG_LINES[algorithm][target][0]
        end = LOG_LINES[algorithm][target][1]
        missing_start = True
        missing_end = True

        #print("memprofile = ", MEMPROFILE)
        #print(log.readlines())

        for line in memlog.split('\n'):
            toks = line.split()
            #print(str(toks))
            if len(toks) > 1 and toks[0] == start and \
                missing_start:
                missing_start = False
                start_val = float(toks[1])
                #print('found start')
                #print(str(toks))
            elif len(toks) > 1  and toks[0] == end and \
                    missing_end:
                missing_end = False
                end_val = float(toks[1])
                #print('found end')
                #print(str(toks))

        if (missing_start or missing_end):
            #print(str(missing_start))
            #print(str(missing_end))
            print('parsing mem log for: ' + algorithm)
            print('target: ' + target)
            print('start: ' + str(start))
            print('end: ' + str(end))
            sys.exit('failed to parse memory log')
        else:
            return (end_val - start_val)



if __name__ == "__main__":


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
        for str_len in LENGTHS[algorithm]:
            for alphabet in ALPHAS.keys():
                for target in ['size','lcs']: #wasteful, but works

                    if alphabet == 'bin':
                        strings = strings_alpha
                    else:
                        strings = strings_bin

                    if algorithm == 'naive':
                        algo, time, memlog, lcs = \
                            naive.lcs_naive(strings[str_len][0],
                                            strings[str_len][1])
                        recursion_depth = naive.registry['_lcs_naive']
                        space = None # not tracking for short strings
                        match = len(lcs)

                    elif algorithm == 'memoized':
                        algo_size, time_size, memlog, lcs_table = \
                            memoized.lcs_memoized(strings[str_len][0],
                                                strings[str_len][1])
                        match = memoized.size_lcs(lcs_table)
                        recursion_depth_size = \
                                memoized.registry['_lcs_memoized']

                        # delete memory profile log
                        #if os.path.exists(MEMPROFILE):
                        #    os.remove(MEMPROFILE)

                        algo_lcs, time_lcs, waste, lcs = \
                            memoized.reconstruct_lcs_memoized(
                                    lcs_table,
                                    match,
                                    strings[str_len][0],
                                    strings[str_len][1])
                        recursion_depth_lcs = \
                            memoized.registry['_reconstruct_lcs_memoized']
                        #space_lcs = parse_log(memlog, algorithm, target)

                        if target == 'size':
                            time = time_size
                            recursion_depth = \
                                    recursion_depth_size
                            space = parse_log(memlog, algorithm, target)
                        else:   #target == 'lcs'
                            time = time_lcs
                            recursion_depth = \
                                    recursion_depth_lcs
                            #space = space_lcs
                            space = None # same as for lcs_memoized

                    elif algorithm == 'dynamic':
                        algo_size, time_size, memlog, lcs_table = \
                            dynamic.lcs_bottomup(strings[str_len][0],
                                                strings[str_len][1])
                        match = dynamic.size_lcs(lcs_table)
                        recursion_depth_size = \
                                dynamic.registry['_lcs_bottomup']

                        # delete memory profile log
                        #if os.path.exists(MEMPROFILE):
                        #    os.remove(MEMPROFILE)

                        algo_lcs, time_lcs, waste, lcs = \
                            dynamic.reconstruct_lcs_dynamic(
                                    lcs_table,
                                    match,
                                    strings[str_len][0],
                                    strings[str_len][1])
                        recursion_depth_lcs = \
                            dynamic.registry['_reconstruct_lcs_dynamic']
                        #space_lcs = parse_log(memlog, algorithm, target)

                        if target == 'size':
                            time = time_size
                            recursion_depth = \
                                    recursion_depth_size
                            space = parse_log(memlog, algorithm, target)
                        else:   #target == 'lcs'
                            time = time_lcs
                            recursion_depth = \
                                    recursion_depth_lcs
                            #space = space_lcs
                            space = None # same as for lcs_bottomup

                    else: #algorithm == 'hirschberg'
                        algo_size, time_size, waste, lcs_vector = \
                            hirschberg.algB(strings[str_len][0],
                                            strings[str_len][1])
                        match = hirschberg.size_lcs(lcs_vector)
                        recursion_depth_size = \
                                hirschberg.registry['_algB']
                        #space_size = parse_log(memlog, algorithm, target)

                        # delete memory profile log
                        #if os.path.exists(MEMPROFILE):
                        #    os.remove(MEMPROFILE)

                        algo_lcs, time_lcs, memlog, lcs = \
                            hirschberg.algC(
                                    strings[str_len][0],
                                    strings[str_len][1])
                        recursion_depth_lcs = \
                            hirschberg.registry['_algC']

                        if target == 'size':
                            time = time_size
                            recursion_depth = \
                                    recursion_depth_size
                            #space = space_size
                            space = None # negligible (for vector)
                        else:   #target == 'lcs'
                            time = time_lcs
                            recursion_depth = \
                                    recursion_depth_lcs
                            space = parse_log(memlog, algorithm, target)

                    experiment.append({'algo':algorithm,
                                        'target':target,
                                        'alphabet':alphabet,
                                        'time':time,
                                        'space':space, # not reliable
                                        'input_size':str_len,
                                        'match_size':match,
                                        'recursion_depth':recursion_depth})

                    # delete memory profile log
                    #if os.path.exists(MEMPROFILE):
                    #    os.remove(MEMPROFILE)


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
