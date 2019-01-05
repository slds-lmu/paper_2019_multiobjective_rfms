# paper_2018_fmoms
- pay attention to those issues labeled with "HelpWanted"
- do not look at the code before you know what is **open-box, lock-box and curator-box**
- source test.R before submit your code
- if error like ("not a matrix") in Kriging occur, just run the experiment again with another seed. 


# How to run
- parallel to the R directory, make a directory called output
- in file pre_bt.R, execute the data split function to generate data in Data/temp folder (written in code already), the repository only contain the geo dataset, other dataset need to be downloaed from openml and split into several parts. 
- source test.R to test if the 1st job could be executed. 
- pay attention to R/batchtools.conf.R, the budgets are set there. Each job need at least 3 hours to be finished. For openml dataset, it needs much much longer (5 hours) (14424.119 /3600 = 4.03)
- first submit only 5 replications according to getJobTable()[, repl] 


# Runing on lido cluster in Dortmund
- read "how to run" above first
- delete the batchtools.conf.R file since that file is for lrz
- regenerate data/temp folder with pre_bt.R, note that this is to generate the RData file for several oml dataset. 
- comment out list.data$oml14966 = prepareDataSite in bt_problem.R if only geo dataset needs to be run. 
