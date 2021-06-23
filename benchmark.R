library(mlr3)
library(mlr3oml)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3misc)
library(paradox)
library(batchtools)
library(mlr3batchmark)
library(data.table)
library(autocompboost)

source("setup.R", local = TRUE)

OMLTasks = lapply(OML_TASK_IDS, function(oid) tsk("oml", oid))
resamplings = lapply(OML_TASK_IDS, function(oid) rsmp("oml", oid))
learners = unlist(lapply(LEARNER_IDS, function(lid) lapply(tasks, function(t) getFinalLearner(lid, t))))


design = data.table(
  task = rep(tasks, times = length(LEARNER_IDS)),
  learner = learners,
  resampling = rep(resamplings, times = length(LEARNER_IDS))
)

# unlink("autocompboost-single-resampling-benchmark", recursive = TRUE)

# reg = batchtools::makeExperimentRegistry(
#   file.dir = "autocompboost-single-resampling-benchmark",
#   packages = c("mlr3", "mlr3learners", "mlr3extralearners",
#     "mlr3pipelines", "mlr3tuning", "mlrintermbo",
#     "mlr3proba", "paradox", "dplyr"),
#   source = c("setup.R"),
#   seed = 123
# )
reg$default.resources = list(
  walltime = 3600L * 8,
  memory = 1024L * 16L,
  ntasks = 1L,
  ncpus = 1L,
  nodes = 1L,
  clusters = "serial"
)

batchmark(design, reg = reg)#, store_models = TRUE)

getStatus()
