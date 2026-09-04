## This script is for running spm. information regarding spm can be found on the official website: https://spm1d.org/index.html

# Using this script is very simple, just ensure youre in the right folder of your computer where all your .csv files (processed from the 
# R script) are saved, then change the names of the csvs in the script where marked. There are also a few customisation options available
# if wanted, such as adding threshold labels to the graph, or normalising the data beforehand either regularly or by z-score (see official
# website for reasoning behind this)

# import libraries

from matplotlib import pyplot
import spm1d
import numpy as np
import csv

# load in control **CHANGE NAME OF FILE HERE**
with open('control_spm_nuclei.csv', 'r') as file:
    medium_data = csv.reader(file)
    medium_data = list(medium_data)

# remove first row to clean data
medium_data = medium_data[1:len(medium_data)]

# remove first item from each list to clean data
medium_data = [sublist[1:] for sublist in medium_data]

# switch to np array type
medium_data = np.array(medium_data, dtype=float)

# try normalising by z score
#for sublist in medium_data:
    
 #   mean = np.mean(sublist)
    
  #  sd_dev = np.std(sublist)
    
   # sublist = (sublist - mean) / sd_dev
    
# try normalising regularly

#for sublist in medium_data:
    
    #medium_data = (medium_data - np.min(medium_data)) / (np.max(medium_data) - np.min(medium_data))
  

# load in test data **CHANGE NAME OF FILE HERE**
with open('nN_2_700_spm_nuclei.csv', 'r') as file:
    nN_data = csv.reader(file)
    nN_data = list(nN_data)

# remove first row to clean data
nN_data = nN_data[1:len(nN_data)]

# remove first item from each list to clean data
nN_data = [sublist[1:] for sublist in nN_data]

# change to np array
nN_data = np.array(nN_data, dtype=float)

# try normalising by z score
#for sublist in nN_data:
    
 #   mean = np.mean(sublist)
    
  #  sd_dev = np.std(sublist)
    
   # sublist = (sublist - mean) / sd_dev
    
# try normalising regularly

#for sublist in nN_data:
    
    #nN_data = (nN_data - np.min(nN_data)) / (np.max(nN_data) - np.min(nN_data))
   

# Conduct t test:
t          = spm1d.stats.ttest2(nN_data, medium_data, equal_var=False)
ti         = t.inference(alpha = 0.05)

ti.plot()
#ti.plot_threshold_label(pos = (50, 3.0)) # adds threshold label if wanted

# extract axis
ax = pyplot.gca()

# axis for whole range
# edit axis ticks from 0 to 500 in five equal increments
#ax.set_xticks([0, 125, 250, 375, 500])
# relabel each increment to match orientation angle
#ax.set_xticklabels([-90, -45, 0, 45, 90])

# axis for folded range
# edit axis ticks from 0 to 500 in five equal increments
ax.set_xticks([0, 166, 333, 500])
# relabel each increment to match orientation angle
ax.set_xticklabels([0, 30, 60, 90])


pyplot.show()

