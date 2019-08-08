# High Dimensional Restrictive Federated Model Selection with multi-objective Bayesian Optimization over shifted distributions

## Terminology
- **open-box, lock-box and curator-box**: openbox is data you can use as you want for training, lock-box is left over independent dataset for testing,  curator-box is the data you can send your model to it to query how good it is but can not carry out training on it. 

## How to run
- basic test: source bt_test_geo.R before submit your code
- advanced test: oml: source bt_test_oml.R 

### run on cluster
- pay attention to R/batchtools.conf.R, the budgets are set there. Test how long it needs locally (> 8 hours) first
- first submit only 10 replications according to getJobTable()[, repl], then submit the rest replications.

#### Run on lido cluster in Dortmund
- delete the batchtools.conf.R file since that file is for lrz

## Problem with experiments
- if error like ("not a matrix") in Kriging occur, that means mbo always see a constant value, which is not yet effectively resolved.

## results
- https://drive.google.com/open?id=1NQ99DNIh6vY5Z9bte5d-SKFH4ZYc5L7N
