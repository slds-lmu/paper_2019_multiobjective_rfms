library(R6)
library(mlrMBO)
source("bt_helpers.R")
# separation of Algorithm and Datasite
Algo = R6Class(
  classname = "Algo",
  public = list(
    initialize = function(lrn, budget, des, par.set) {
      private$lrn = lrn
      private$budget = budget
      private$des = des
      private$par.set = par.set
    }
  ),

  private = list(
    list_ds = list(),  # list of data site
    lrn = NULL,
    budget = NULL,
    des = NULL,
    par.set = NULL
  )
)


AlgoFSO = R6Class(
  classname = "AlgoFSO",
  inherit = Algo,
  public = list(
    opt = function(parset) {
      budget = private$budget
      control = NULL
      surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))
      obj.fun = smoof::makeSingleObjectiveFunction(name = "fso",
          fn = function(x) {
            self$ds$getFeedBack(x)
          },
          par.set = private$par.set,
          minimize = TRUE)
      mbo(obj.fun, design = private$des, learner = surr.km, control = control, show.info = TRUE)
    }
    )
)

AlgoFMO = R6Class(
  classname = "AlgoFMO",
  public = list(
    initialize = function() {
    },
    opt = function(parset) {
      control = init_mbo_ctrl_mo(n.obj = 2, iters = 10)
      surr.km = mlr::makeLearner("regr.km", predict.type = "se", config = list(show.learner.output = FALSE, on.learner.error = "warn", on.par.out.of.bounds = "warn"), jitter = TRUE, nugget = 1e-6)
      obj.fun = function(par.set, lrn) {
        smoof::makeMultiObjectiveFunction(
          name = "fmo",
          fn = function(x) unlist(self$ds$getFeedBack(x)),
          n.objectives = 2L,
          par.set = private$par.set
          )
      }
      mbo(obj.fun, design = private$des, learner = surr.km, control = control, show.info = TRUE)
    }
    )
)

DataSite = R6Class(
  classname = "DataSite",
  public = list(
    initialize = function(data, algo) {
      private$data = data
      private$algo = algo
    },

    getFeedBack = function(para) {
      local_perf = self$getHyperParLocalPerf(para)
      model = self$getModelFromDs(private$data)
      remote_perf = self$sendModelQuery(model = model, datasite = private$remote)
      return(list(local_perf = local_perf, remote_perf = remote_perf))
    },

    opt = function() {
      self$algo$opt()
    },

    getModelFromDs = function(ds) {
    },

    getHyperParLocalPerf = function() {

    },

    getModelLocalPerf = function() {

    },

    sendModelQuery = function(model, datasite) {
    }
    ),
  private = list(
    data = NULL,  # mlr task
    algo = algo
    )
  )
