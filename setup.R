# debug
options("mlr3.debug" = TRUE)

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
  # "classif.cv_glmnet",
  # "classif.ranger",
  # "classif.gamboost", # multiclass pipeline für gamboost
  "classif.autocompboost",
  "classif.autocompboost.with_trees"
)

# Resampling
# RESAMPLING_OUTER = rsmp("cv", folds = 10L)#rsmp("cv", folds = 10L)


# Tuning
TUNING_BUDGET = 3600L * 3
RESAMPLING_INNER = rsmp("cv", folds = 5L)
TUNER = tnr("intermbo") #tnr("hyperband", eta = 2L)
TERMINATOR = trm("run_time", secs = TUNING_BUDGET) #trm("none")


# getLearnerParamValues = function(learner_id, task) {
  # if (learner_id =
# }


# helper functions
# graphlearner
getGraphLearner = function(learner_id, task) {
  pipeline = po("removeconstants") %>>%
    # po("extremwerte_raus") 3*sd %>>%
    po("imputesample") %>>%
    po("fixfactors") %>>%
    po("encode")

  if (learner_id == "classif.gamboost" && length(task$class_names) > 2) {
    as_learner(
      pipeline %>>%
        pipeline_ovr(lrn(learner_id, predict_type = "prob"))
    )
  } else {
    as_learner(pipeline %>>%
      lrn(learner_id, predict_type = "prob")
    )
  }
}

# autotuner/graphlearner/autocompboost
getFinalLearner = function(learner_id, task) {
  if ("multiclass" %in% task$properties) {
    m = msr("classif.auc")
  } else {
    m = msr("classif.logloss")
  }
  if (learner_id == "classif.autocompboost") {
    return(AutoCompBoost(
      task = task,
      tuning_time = TUNING_BUDGET,
      tuning_iters = 500,
      measure = m
    )$learner$learner)
  } else if (learner_id == "classif.autocompboost.with_trees") {
    at = AutoCompBoost(
      task = task,
      param_values = list(add_deeper_interactions = TRUE),
      tuning_time = TUNING_BUDGET,
      tuning_iters = 500,
      measure = m
    )$learner$learner
    at$id = learner_id
    return(at)
  } else {
    gl = getGraphLearner(learner_id, task)
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


