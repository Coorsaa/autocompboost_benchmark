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

OMLTasks = lapply(OML_TASK_IDS, function(oid) OMLTask$new(oid))
tasks = lapply(OMLTasks, function(t) t$task)
resamplings = lapply(OMLTasks, function(t) t$resampling)

learners = unlist(lapply(LEARNER_IDS, function(lid) lapply(tasks, function(t) getFinalLearner(lid, t))))


design = data.table(
  task = rep(tasks, times = length(LEARNER_IDS)),
  learner = learners,
  resampling = rep(resamplings, times = length(LEARNER_IDS))
)

unlink("autocompboost-benchmark", recursive = TRUE)

reg = batchtools::makeExperimentRegistry(
  file.dir = "autocompboost-benchmark",
  packages = c("mlr3", "mlr3learners", "mlr3extralearners",
    "mlr3pipelines", "mlr3tuning", "mlrintermbo",
    "mlr3proba", "paradox", "dplyr"),
  source = c("setup.R"),
  seed = 123
)
reg$default.resources = list(
  walltime = 3600L * 6L,
  memory = 1024L * 16L,
  ntasks = 1L,
  ncpus = 8L,
  nodes = 1L,
  clusters = "cm2_tiny"
)

batchmark(design, reg = reg)#, store_models = TRUE)

getStatus()
