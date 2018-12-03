# Project: OCR (Optical Character Recognition) 

### Data folder

The data directory contains data used in the analysis. This is treated as read only; in paricular the R/python files are never allowed to write to the files in here. 

In this project, there are two subfolders -- ground_truth and tesseract. Each folder contains 100 text files with same file names correspondingly. In addition, the files featureSet.csv contains the data generated for fitting an SVM using a rough set of correct and incorrect tokens generated from the given data. featureSetMatched.csv contains the data generated for fitting an SVM using matched correct/incorrect tokens. matched_pairs is a file containing a subset of the tokens in the ground_truth and tesseract files that have been matched to one another. This is data that would be used to compute the ultimate measures of precision/recall of our correction model. One of my teammates produced this data file, but is does not appear to contain the full set of matched pairs in the given data; I have attempted to contact former teammate to see if they will provide the script that produced this data, but they have not responded to me. 


