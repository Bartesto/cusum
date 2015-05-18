# Make TS Data
# This code:
#         imports from cloud QA'd csv
#         constructs daily ts to 'regularise' ts
#         compresses ts to monthly observations
#         interpolates (linear) missing values
#
# by Bart Huntley 18/05/2015

rm(list=ls())

