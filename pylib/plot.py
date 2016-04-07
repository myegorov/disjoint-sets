import matplotlib.pyplot as plt
import os.path

CURDIR = os.path.abspath(os.path.curdir)
DOCDIR = os.path.join(os.path.dirname(CURDIR), \
            'docs/source/figures')

def plot_scatter(set1, set2, labels, title, xlabel, ylabel):
    """Save 2D scatter plot of numplots sets of data.

    Args:
        set1 (dict):   dict of coordinate lists; set1 = 
                        {'x':[list of x-coords], 
                        'y':[list of y-coords]}
        set2:   ditto
        labels (list of strings):   series labels 
                                    for each data set
        title (string):     plot title
        xlabel, ylabel (string):    axes labels
    """

    fig = plt.figure()
    axes = plt.gca()
    axes.set_xlim([set1['x'][0] - 5, \
            set1['x'][-1] + 5])
    plt.ticklabel_format(style='plain', axis='both', \
            useOffset = False)
    fig.suptitle(title, fontsize=20)
    plt.xlabel(xlabel, fontsize=14)
    plt.ylabel(ylabel, fontsize=14)
    if set2:
        plt.scatter(set1['x'], set1['y'], s=20,
                c='mediumslateblue', 
                marker='+', label = labels[0])
        plt.scatter(set2['x'], set2['y'], s=20, c='crimson', 
                marker='o', label = labels[1])
    else:
        plt.scatter(set1['x'], set1['y'], s=20, 
                c='mediumslateblue', 
                marker='+', label = labels[0])

    plt.legend(loc='upper left')
    fname = '_'.join(title.replace(':','_').split()) + '.ps'
    fig.savefig(os.path.join(DOCDIR,fname))
    #plt.show()
