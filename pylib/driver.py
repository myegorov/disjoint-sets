#!/usr/bin/env python3

"""
driver.py

This driver performs all topmost logic:
    - imports all algorithm modules;
    - runs the algorithms, takes time measurements & counts recursive calls;
    - post-processes the data to create tables and plots;

Usage (meant to be run from a build script):
    python3 driver.py

Caveat:
    - prior to running the script, increase stack size allocated
        to the python process & run it in background:
            $ ulimit -s 120000
            $ nohup python3 ./driver.py &
            $ ps aux | grep driver.py
            $ jobs
            $ fg 1

"""

__author__ = "Maksim Yegorov"
__date__ = "2016-05-06 Fri 09:18 PM"

import os, sys
from datetime import datetime
import importlib
from subprocess import call
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
LENGTHS = {'naive':      [5, 10, 15, 20],
           'memoized':   [5, 10, 15, 20, 1000, 2000, \
                   3000, 4000, 5000, 10000],
           'dynamic':    [5, 10, 15, 20, 1000, 2000, \
                   3000, 4000, 5000, 10000],
           'hirschberg': [5, 10, 15, 20, 1000, 2000, \
                   3000, 4000, 5000, 10000,\
                   20000, 30000, 40000, 50000, 60000]
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

def echo(memo):
    """Prints time stamped debugging message to std out.

    Args:
        memo (str): a message to be printed to screen
    """
    print("[%s] %s" %(datetime.now().strftime("%m/%d/%y %H:%M:%S"), memo))

def run_experiments():

    # create a library of strings for each alphabet
    # on which algos will be tested:
    # dict(1='z', 3='yzx',...)
    echo("Compiling a library of test strings...")
    test_lengths = \
        set([l for key in LENGTHS.keys() for l in LENGTHS[key]])
    strings_alpha = {l:[strgen(alphabet=ALPHAS['alpha'], size=l), \
                        strgen(alphabet=ALPHAS['alpha'], size=l)] \
                        for l in test_lengths}
    strings_bin = {l:[strgen(alphabet=ALPHAS['bin'], size=l), \
                    strgen(alphabet=ALPHAS['bin'], size=l)] \
                    for l in test_lengths}

    # list of experimental results (list of dicts)
    experiments = []

    # run tests for each algo for either alphabet
    echo("About to run each algorithm in turn on each test string...")
    for algorithm in LENGTHS.keys():
        module = MODULES[algorithm]
        echo("Running algorithm module " + module.__name__)

        for str_len in LENGTHS[algorithm]:
            echo("\__. for input string length " + str(str_len))

            for alphabet in ALPHAS.keys():
                echo("   \__. for alphabet " + alphabet)

                if alphabet == 'bin':
                    strings = strings_bin
                else:
                    strings = strings_alpha

                # build up a table of LCS lengths
                echo("      --> calculating LCS length...")
                sys.stdout.flush()
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
                echo("      --> reconstructing an LCS...")
                sys.stdout.flush()
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

                echo("      --> saving results of the run...")
                sys.stdout.flush()
                experiments.append({ \
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

    return experiments

def plot_sanity_check(experiments, fname = "sanity_check.ps"):
    """Plot input string length vs LCS length to verify
    all algorithms agree on length of LCS for each input.

    Args:
        experiments (list of dicts): experiment data
        fname (str): filename for plot
    """

    # compile a dict of dicts organized by algo + alphabet
    # {'label': {'x':[...], 'y':[...]},...}
    data = {}
    input_lens = [5, 10, 15, 20]
    for experiment in experiments:
        label = experiment['algo'] + "_" + \
                experiment['alphabet']
        if experiment['input_size'] in input_lens:
            if label in data.keys():
                data[label]['x'].append(experiment['input_size'])
                data[label]['y'].append(experiment['match_size'])
            else:
                data[label] = {}
                data[label]['x'] = [experiment['input_size']]
                data[label]['y'] = [experiment['match_size']]
    plot_scatter(data, title = "LCS length vs input str length",
                xlabel = "input string length",
                ylabel = "LCS string length",
                fname = fname)

def plot_all(experiments, attrx, attry,\
        xlabel, ylabel, title, fname):
    """Plot attrx vs attry for each algorithm and alphabet.

    Args:
        experiments (list of dicts): experiment data
        attrx (str): type (size LCS or reconstruct LCS)
        attry (str): type (size LCS or reconstruct LCS)
        title (str): plot title
        fname (str): file name for plot
    """

    # compile a dict of dicts organized by algo + alphabet
    # {'label': {'x':[...], 'y':[...]},...}
    data = {}
    for experiment in experiments:
        label = experiment['algo'] + "_" + \
                experiment['alphabet']
        if experiment[attry]: # if we kept track
            if label in data.keys():
                data[label]['x'].append(experiment[attrx])
                data[label]['y'].append(experiment[attry])
            else:
                data[label] = {}
                data[label]['x'] = [experiment[attrx]]
                data[label]['y'] = [experiment[attry]]
    plot_scatter(data, title = title,
                xlabel = xlabel,
                ylabel = ylabel,
                fname = fname)

def plot_memory_vs_input(experiments, attrx, attry,\
        xlabel, ylabel, title, fname='mem_usage.ps'):
    """Plot memory usage for the most memory expensive operation
    (sizing LCS or reconstructing LCS) vs input string length.

    Args:
        experiments (list of dicts): experiment data
        attrx (str): type (size LCS or reconstruct LCS)
        attry (list of str): type (size LCS or reconstruct LCS)
        title (str): plot title
        fname (str): file name for plot
    """
    data = {}
    for experiment in experiments:
        label = experiment['algo'] + "_" + \
                experiment['alphabet']

        if experiment[attry[0]] is not None: # if we kept track
            attr = attry[0]
        elif experiment[attry[1]] is not None:
            attr = attry[1]
        else:
            attr = None

        if attr is not None:
            if label in data.keys():
                data[label]['x'].append(experiment[attrx])
                data[label]['y'].append(experiment[attr])
            else:
                data[label] = {}
                data[label]['x'] = [experiment[attrx]]
                data[label]['y'] = [experiment[attr]]
    plot_scatter(data, title = title,
                xlabel = xlabel,
                ylabel = ylabel,
                fname = fname)

if __name__ == "__main__":

    experiments = run_experiments()

    # write data to file
    header = experiments[0].keys()
    with open(RESULTS, 'w') as csvfile:
        dict_writer = csv.DictWriter(csvfile, fieldnames=header)
        dict_writer.writeheader()
        dict_writer.writerows(experiments)

    # plot data
    echo("Done with algorithm runs. About to plot data...")

    ## (1) as a sanity check, plot LCS length vs. input str length
    ##      for all test strings of length 5 <= len <= 20
    plot_sanity_check(experiments)

    ## (2) plot input string length vs. CPU time for each algorithm
    plot_all(experiments, attrx = 'input_size',
            attry = 'time_sizing',
            xlabel = "input string length",
            ylabel = "CPU time (sec)",
            title = "Sizing LCS: CPU time vs input str length",
            fname = 'cpu_input_sizing.ps')
    plot_all(experiments, attrx = 'input_size',
            attry = 'time_reconstruct',
            xlabel = "input string length",
            ylabel = "CPU time (sec)",
            title = "Reconstructing LCS: CPU time vs input str length",
            fname = 'cpu_input_reconstruct.ps')

    ## (3) plot recursion depth vs. CPU time for each algorithm
    plot_all(experiments, attrx = 'recursion_sizing',
            attry = 'time_sizing',
            xlabel = "number of recursive calls",
            ylabel = "CPU time (sec)",
            title = "Sizing LCS: CPU time vs recursion depth",
            fname = 'cpu_recursion_sizing.ps')
    plot_all(experiments, attrx = 'recursion_reconstruct',
            attry = 'time_reconstruct',
            xlabel = "number of recursive calls",
            ylabel = "CPU time (sec)",
            title = "Reconstructing LCS: CPU time vs recursion depth",
            fname = 'cpu_recursion_reconstruct.ps')

    ## (4) plot recursion depth vs input string length for each algo
    plot_all(experiments, attrx = 'input_size',
            attry = 'recursion_sizing',
            xlabel = "input string length",
            ylabel = "number of recursive calls",
            title = "Sizing LCS: recursion depth vs input str length",
            fname = 'recursion_input_sizing.ps')
    plot_all(experiments, attrx = 'input_size',
        attry = 'recursion_reconstruct',
        xlabel = "input string length",
        ylabel = "number of recursive calls",
        title = "Reconstructing LCS: recursion depth vs input str length",
        fname = 'recursion_input_reconstruct.ps')

    ## (5) plot input string lnegth vs memory usage for each algo
    plot_memory_vs_input(experiments, attrx = 'input_size',
            attry = ['space_sizing', 'space_reconstruct'],
            xlabel = "input string length",
            ylabel = "memory usage (MiB)",
            title = "Memory usage vs input string length")

    echo("All done. Exiting...")

