library(mlr3)
library(mlr3oml)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3hyperband)
library(mlr3misc)
library(paradox)
library(batchtools)
library(mlr3batchmark)
library(data.table)
library(autocompboost)

source("setup_expired.R", local = TRUE)

tasks = lapply(OML_TASK_IDS, function(oid) tsk("oml", oid))
resamplings = lapply(OML_TASK_IDS, function(oid) rsmp("oml", oid))
learners = unlist(lapply(LEARNER_IDS, function(lid) lapply(tasks, function(t) getFinalLearner(lid, t))))


design = data.table(
  task = rep(tasks, times = length(LEARNER_IDS)),
  learner = learners,
  resampling = rep(resamplings, times = length(LEARNER_IDS))
)

unlink("autocompboost-benchmark-expired", recursive = TRUE)

reg = batchtools::makeExperimentRegistry(
  file.dir = "autocompboost-benchmark-expired",
  packages = c("mlr3", "mlr3learners", "mlr3extralearners",
    "mlr3pipelines", "mlr3tuning", "mlr3hyperband",
    "mlr3proba", "paradox", "dplyr", "autocompboost"),
  source = c("setup_expired.R"),
  seed = 123
)
reg$default.resources = list(
  walltime = 3600L * 48,
  memory = 1024L * 48L,
  ntasks = 1L,
  ncpus = 28L,
  nodes = 1L,
  clusters = "cm2_tiny",
  partition = "cm2_tiny"
)

batchmark(design, reg = reg)#, store_models = TRUE)

getStatus()

