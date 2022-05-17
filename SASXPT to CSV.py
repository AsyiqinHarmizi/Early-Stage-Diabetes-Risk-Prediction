# -*- coding: utf-8 -*-
"""
Created on Thu Dec 23 12:02:17 2021

@author: Erica
"""

import pandas as pd
FILE_PATH ="add your file path to SAS XPT file directory"
FILE ="LLCP2019"  # filename itself (without suffix)

# Note: might need to substitute the column name of the index (in quotes) for "None" here

df = pd.read_sas(FILE_PATH + FILE + '.XPT', index=None)

df.to_csv(FILE_PATH + FILE + '.csv')
