#!/usr/bin/env python3

"""
plot.py

Plot experimental results.

Usage (meant to be run from a build script):
    python3 .py
"""
__author__ = "Maksim Yegorov"
__date__ = "2016-05-06 Fri 01:30 AM"


import matplotlib.pyplot as plt
import os.path, itertools

CURDIR = os.path.abspath(os.path.curdir)
DOCDIR = os.path.join(os.path.dirname(CURDIR), \
            'docs/source/figures')

def plot_scatter(data, title, xlabel, ylabel, fname):
    """Save 2D scatter plots of data.

    Args:
        data (dict of dicts):   dict of x and y series;
                        data['algo_label'] =
                        {'x':[list of x-coords],
                        'y':[list of y-coords]}
        title (string):     plot title
        xlabel, ylabel (string):    axes' labels
        fname (string): file name to save plot to
    """

    fig = plt.figure()
    axes = plt.gca()

    ax = plt.subplot(111)
    box = ax.get_position()
    ax.set_position([box.x0, box.y0, \
            box.width * 0.7, box.height])

    fig.suptitle(title, fontsize=20)
    plt.xlabel(xlabel, fontsize=14)
    plt.ylabel(ylabel, fontsize=14)
    labels = ax.get_xticklabels()
    plt.setp(labels, rotation=30, fontsize = 14)

    colors = itertools.cycle(['b', 'g', 'r', \
            'lavender', 'm', 'crimson', 'k', 'plum'])
    markers = itertools.cycle(['+', 'o', 'x', '.', \
            '|', 'v', '_', 's', '*'])

    for algo in data.keys():
        color = next(colors)
        marker = next(markers)

        plt.scatter(data[algo]['x'], data[algo]['y'], \
                s=60, c=color, marker=marker, label=algo)
    plt.grid()
    plt.legend(loc='center left', \
            bbox_to_anchor=(1, 0.5),
                  ncol=1, fancybox=True, shadow=True,
                  scatterpoints = 1)
    fig.savefig(os.path.join(DOCDIR,fname), \
            bbox_inches = 'tight')
    #plt.show()
