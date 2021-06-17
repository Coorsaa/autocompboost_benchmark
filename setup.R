# Datasets
oml_tasks = mlr3oml::list_oml_tasks(tag = "study_218")
OML_TASK_IDS = oml_tasks$task_id

# Measures
TUNING_MEASURE = "classif.acc"
SCORE_MEASURES = c(
  "classif.acc", "classif.auc", "classif.logloss"
)

# Learners
LEARNER_IDS = c(
  "classif.cv_glmnet",
  "classif.ranger",
  "classif.gamboost", # multiclasswrapper für gamboost
  "classif.autocompboost"
)

# Resampling
RESAMPLING_OUTER = rsmp("cv", folds = 10L)#rsmp("cv", folds = 10L)


# Tuning
TUNING_BUDGET = 60L
RESAMPLING_INNER = rsmp("cv", folds = 5L)
TUNER = tnr("intermbo") #tnr("hyperband", eta = 2L)
TERMINATOR = trm("run_time", secs = TUNING_BUDGET) #trm("none")


# getLearnerParamValues = function(learner_id, task) {
  # if (learner_id =
# }


# helper functions
# graphlearner
getGraphLearner = function(learner_id) {
  as_learner(
    po("removeconstants") %>>%
      # po("extremwerte_raus") 3*sd %>>%
      po("imputesample") %>>%
      po("fixfactors") %>>%
      po("encode") %>>% #learner abhängig
      lrn(learner_id, predict_type = "prob")
  )
}

# autotuner/graphlearner/autocompboost
getFinalLearner = function(learner_id, task) {
  if (learner_id == "classif.autocompboost") {
    return(AutoCompBoost(
      task = task,
      tuning_time = TUNING_BUDGET,
      tuning_iters = 500,
      measure = msr(ifelse(length(task$class_names) == 2, "classif.auc", "classif.logloss")
    )$learner)
  } else {
    gl = getGraphLearner(learner_id)
    if (learner_id == "classif.ranger") {
      return(gl)
    } else {
      return(
        AutoTuner$new(
          learner = gl,
          search_space = getTuningParams(learner_id, task),
          resampling = RESAMPLING_INNER,
          tuner = TUNER,
          terminator = TERMINATOR,
          measure = msr(ifelse(length(task$class_names) == 2, "classif.auc", "classif.logloss"))
        )
      )
    }
  }
}

# search space
getTuningParams = function(learner_id, task) {
  if (learner_id == "classif.cv_glmnet") {
    search_space = ps(
      classif.cv_glmnet.alpha = p_dbl(lower = 0, upper = 1)#,
    )
  } else if (learner_id == "classif.gamboost") {
    search_space = ps(
      classif.gamboost.dfbase = p_int(lower = 1, upper = min(15, sapply(task$levels(), length))),
      classif.gamboost.nu = p_dbl(lower = 0.001, upper = 0.4),
      classif.gamboost.mstop = p_int(lower = 100, upper = 10000)
    )
  }
  return(search_space)
}


