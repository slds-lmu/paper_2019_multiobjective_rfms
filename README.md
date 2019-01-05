# paper_2018_fmoms
- pay attention to those issues labeled with "HelpWanted"
- do not look at the code before you know what is **open-box, lock-box and curator-box**
- source test.R before submit your code
- if error like ("not a matrix") in Kriging occur, just run the experiment again with another seed. 


# How to run
- parallel to the R directory, make a directory called output
- in file pre_bt.R, execute the data split function to generate data in Data/temp folder (written in code already), the repository only contain the geo dataset, other dataset need to be downloaed from openml and split into several parts. 
- source test.R to test if the 1st job could be executed. 
- pay attention to R/batchtools.conf.R, the budgets are set there. Each job need at least 3 hours to be finished. 
- first submit only 5 replications according to getJobTable()[, repl] 
