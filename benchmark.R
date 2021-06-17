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

source("setup.R", local = TRUE)

tasks = lapply(OML_TASK_IDS, function(oid) tsk("oml", task_id = oid))

learners = lapply(LEARNER_IDS, function(lid) lapply(tasks, function(t) getFinalLearner(lid, t)))

design = benchmark_grid(
  task = tasks,
  learner = learners,
  resampling = RESAMPLING_OUTER
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
  walltime = 3600L * 12L,
  memory = 1024L * 12L,
  ntasks = 1L,
  ncpus = 1L,
  nodes = 1L,
  clusters = "serial"
)

batchmark(design, reg = reg)#, store_models = TRUE)

getStatus()
