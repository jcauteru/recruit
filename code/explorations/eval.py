__author__ = 'joe'

"""
Supporting MAP functions
"""

class MAP:

    import numpy as np

    def __int__(self, act, pred):
        self.actual = act
        self.predicted = pred

    def apk(self, k=10):

        if len(self.predicted)>k:
            predicted = self.predicted[:k]

        score = 0.0
        num_hits = 0.0

        for i,p in enumerate(self.predicted):
            if p in self.actual and p not in self.predicted[:i]:
                num_hits += 1.0
                score += num_hits / (i+1.0)

        if not actual:
            return 0.0

        return score / min(len(actual), k)

    def mapk(self, actual, predicted, k=10):

        return np.mean([self.apk(a,p,k) for a,p in zip(self.actual, self.predicted)])



"""
Parse the arguments provided by the user. Required arguments are time frame and sample proportion.

optional arguments are iterations. If multiple iterations are requested. Average values are reported by default. To instead
provide the list of scores, use the verbose option.


"""

import argparse