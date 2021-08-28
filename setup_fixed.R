# debug
options("mlr3.debug" = TRUE)

# Datasets
# oml_tasks = mlr3oml::list_oml_tasks(tag = "study_218")
# OML_TASK_IDS = oml_tasks$task_id
OML_TASK_IDS = c(
  168337,
  168338,
  168908,
  146825,
  168909,
  168335

# # deep
#  168911,
#  146212,
#  146822,
#  168329,
#  168331

# 31 # missings
# c++ ,3917
#, 146212 # missings
#, 146606 # missings
#, 146818 # missings
#, 146821 # missings
#,
# 146822 # missings
# c++ , 146825
# bad alloc , 168329
# , 168335 # missings
# c++ , 168337
# c++ , 168338
# c++ , 168908
# internal error , 168909
# , 189354 # missings
# c++ , 3945
)


# Measures
TUNING_MEASURE = "classif.acc"
SCORE_MEASURES = c(
  "classif.acc", "classif.auc", "classif.logloss"
)

# Learners
LEARNER_IDS = c(
  # "classif.ranger",
  # "classif.gamboost", # multiclass pipeline für gamboost
  "classif.autocompboost",
  "classif.autocompboost.with_trees"
)

# Resampling
# RESAMPLING_OUTER = rsmp("cv", folds = 10L)#rsmp("cv", folds = 10L)


# Tuning
# TUNING = FALSE
TUNING_BUDGET = 3600L * 1
RESAMPLING_INNER = rsmp("cv", folds = 5L)
TUNER = tnr("random_search") #tnr("hyperband", eta = 2L)
TERMINATOR = trm("run_time", secs = TUNING_BUDGET) #trm("none")


# getLearnerParamValues = function(learner_id, task) {
  # if (learner_id =
# }

autocompboost_preproc_pipeline = function(task, max_cardinality = 100) {
  has_type_feats = function(types, if_null = TRUE) {
    if (is.null(task)) if_null else any(types %in% task$feature_types$type)
  }

  if (!is.null(task)) assert_task(task)

  pos = list()
  pos = c(pos, po("removeconstants"))

  # recode hidden missings
  pos = c(pos, po("colapply", id = "recode_hidden_missings", param_vals = list(affect_columns = selector_type(c("numeric", "integer")),
    applicator = function(x) {
      x[x == -999] = NA
      return(x)
    }
  )))

  # remove outliers
  # pos = c(pos, po("colapply", id = "remove_outliers", param_vals = list(affect_columns = selector_type(c("numeric", "integer")),
  #   applicator = function(x) {
  #     x[abs(x) > 3 * sd(x)] = NA
  #     return(x)
  #   }
  # )))


  if (has_type_feats("character")) {
    pos = c(pos, po("colapply", id = "char_to_fct", param_vals = list(affect_columns = selector_type("character"), applicator = function(x) as.factor(x))))
  }

  if (has_type_feats("logical")) {
    pos = c(pos, po("colapply", id = "lgl_to_fct", param_vals = list(affect_columns = selector_type("logical"), applicator = function(x) as.factor(x))))
  }

  # if (has_type_feats("integer")) {
    # pos = c(pos, po("colapply", id = "int_to_dbl", param_vals = list(affect_columns = selector_type("integer"), applicator = function(x) as.numeric(x))))
  # }

  if (has_type_feats("POSIXct")) {
    pos = c(pos, po("datefeatures", param_vals = list(affect_columns = selector_type("POSIXct"))))
  }

  if (has_type_feats(c("numeric", "integer"))) {
    pos = c(pos,
      gunion(list(
        po("imputehist", affect_columns = selector_type(c("numeric", "integer"))),
        po("missind", param_vals = list(affect_columns = selector_type(c("numeric", "integer")), type = "factor")))) %>>%
      po("featureunion"))
  }

  # Impute factors
  if (has_type_feats(c("factor", "ordered", "character"))) {
    pos = c(pos, po("imputesample", affect_columns = selector_type(c("factor", "ordered", "character"))))
  }

  # Fix extra factor levels
  if (has_type_feats(c("factor", "ordered"))) {
    pos = c(pos, po("fixfactors"))
  }

  # Ensure all factor levels are encoded during predict FIXME: should we sample those or drop features with NA ratio above x%?
  if (has_type_feats(c("factor", "ordered", "character"))) {
    pos = c(pos, po("imputesample", id = "imputesample_after_fixfactors", affect_columns = selector_type(c("factor", "ordered", "character"))))
  }

  # Collapse factors over 1000 levels
  # FIXME: Can be improved after #330 is solved
  if (is.null(task)) {
    pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
  } else {
    if (any(mlr3misc::map_lgl(task$levels(task$feature_types$id[task$feature_types$type == "factor"]), function(x) length(x) > max_cardinality))) {
      pos = c(pos, po("collapsefactors", param_vals = list(target_level_count = max_cardinality)))
    }
  }
  pos = c(pos, po("removeconstants", id = "removeconstants_end"))

  as_graph(Reduce(`%>>%`, pos))
}



# helper functions
# graphlearner
getGraphLearner = function(learner_id, task) {
  pipeline = autocompboost_preproc_pipeline(task) %>>%
    po("encode", method = "treatment")

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
    m = msr("classif.logloss")
  } else {
    m = msr("classif.auc")
  }
  if (learner_id == "classif.autocompboost") {
    at = AutoCompBoost(
      task = task,
      tuning_method = "hyperband",
      measure = m,
    )$learner
    at$id = paste(learner_id, task$id, sep = "_")
    return(at)
  } else if (learner_id == "classif.autocompboost.with_trees") {
    at = AutoCompBoost(
      task = task,
      param_values = list(add_deeper_interactions = TRUE),
      tuning_method = "hyperband",
      measure = m
    )$learner
    at$id = paste(learner_id, task$id, sep = "_")
    return(at)
  } else {
    gl = getGraphLearner(learner_id, task)
    if (learner_id == "classif.ranger") {
      gl$id = paste(learner_id, task$id, sep = "_")
      return(gl)
    } else {
      at =  AutoTuner$new(
          learner = gl,
          search_space = getTuningParams(learner_id, task),
          resampling = RESAMPLING_INNER,
          tuner = TUNER,
          terminator = TERMINATOR,
          measure = m
        )
      at$id = paste(learner_id, task$id, sep = "_")
      return(at)
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
      classif.gamboost.dfbase = p_int(lower = 2, upper = min(15, max(sapply(task$levels(), length), 2))),
      classif.gamboost.nu = p_dbl(lower = 0.001, upper = 0.4),
      classif.gamboost.mstop = p_int(lower = 100, upper = 10000)
    )
  }
  return(search_space)
}


