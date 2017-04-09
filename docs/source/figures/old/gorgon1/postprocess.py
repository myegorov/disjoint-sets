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
__date__ = "2016-05-07 Sat 04:26 PM"

import os, sys
from datetime import datetime
import importlib
from subprocess import call
from plot import plot_scatter
import csv



# set up directory refs
CURDIR = os.path.abspath(os.path.curdir)
FIGDIR = os.path.join(os.path.dirname(CURDIR),\
            'gorgon/gorgon')
RESULTS = os.path.join(FIGDIR, 'results.csv')

# alphabets
ALPHAS = {'bin':   ['0', '1'],
          'alpha': ['A','C','G','T']}

# lengths of strings to consider
LENGTHS = {'naive':      [5, 10, 15, 20],
           'memoized':   [5, 10, 15, 20, 1000, 2000, \
                   3000, 4000, 5000],
           'dynamic':    [5, 10, 15, 20, 1000, 2000, \
                   3000, 4000, 5000],
           'hirschberg': [5, 10, 15, 20, 1000, 2000, \
                   3000, 4000, 5000, 10000,\
                   40000]
            }


# key to memory log: line numbers to parse
LOG_LINES = {'memoized': {'size':['41', '49']},
            'dynamic': {'size':['39', '46']},
            'hirschberg': {'lcs':['102', '116']}
            }

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
    input_lens = [5, 10, 15, 20, 1000, 2000, 3000, 4000, 5000]
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


    with open(RESULTS, 'r') as csvfile:
        dict_reader = csv.DictReader(csvfile, skipinitialspace=True)

        experiments = [{k:v for k, v in row.items()}
            for row in dict_reader]

        for dct in experiments:
            dct['recursion_reconstruct'] = int(dct['recursion_reconstruct'])
            dct['match_size'] = int(dct['match_size'])
            try:
                dct['space_sizing'] = float(dct['space_sizing'])
            except ValueError:
                dct['space_sizing'] = None
            try:
                dct['recursion_sizing'] = int(dct['recursion_sizing'])
            except ValueError:
                dct['recursion_sizing'] = None
            try:
                dct['time_sizing'] = float(dct['time_sizing'])
            except ValueError:
                dct['time_sizing'] = None
            try:
                dct['time_reconstruct'] = float(dct['time_reconstruct'])
            except ValueError:
                dct['time_reconstruct'] = None
            try:
                dct['space_reconstruct'] = float(dct['space_reconstruct'])
            except:
                dct['space_reconstruct'] = None
            dct['input_size'] = int(dct['input_size'])

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

