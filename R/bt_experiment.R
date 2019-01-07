#' @title  Initilize a registry with problems and algorithms
#' @description
#' The input to algo_funs[["name1"]] is the "instance" argument of the algorithm function definition
#' The "fun" of the algorithm need extra parameters, which will be provided in algodesigns
#' @param prob_names list of problem names
#' @param prob_inputs_data list of anytype
#' @param prob_funs list of functions
#' @param algo_names vector of characters
#' @param algo_funs list of functions
#' @param reg_input The batchtools::Registry class
#' @param algo_designs named list of dataframes
#' @param repls value Number of replications for each problem algorithm combination
#' @return returndes
#' @examples
#' x=c(1,2,3)
init = function(prob_names, prob_inputs_data, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls) {
  for (pname in prob_names) {
    addProblem(name = pname,  data = prob_inputs_data, fun = prob_funs[[pname]], seed = 1L, reg = reg_input)
  }
  for (algoname in algo_names) {
    addAlgorithm(name = algoname, fun = algo_funs[[algoname]], reg = reg_input)
    # algo.designs [named list of data.table or data.frame] Named list of data frames (or data.table). The name must match the algorithm name while the column names correspond to parameters of the algorithm. If NULL, experiments for all defined algorithms without any parameters are added.
  }
  addExperiments(prob.design = prob_designs, algo.design = algo_designs, repls = repls)
}
#' getJobTable()  # better than summarizeExperiments
#' testJob(1)
