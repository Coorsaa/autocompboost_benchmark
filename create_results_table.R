library(batchtools)
library(data.table)

bmr = reduceResultsBatchmark(findDone())
rrs = bmr$resample_results$resample_result

scores = lapply(rrs, function(rr) {
  if (rr$task$properties[1] == "twoclass") {
    data.table(task_id = rr$task$id, learner_id = rr$learner$id, auc = rr$aggregate(msr("classif.auc")))
  } else {
    data.table(task_id = rr$task$id, learner_id = rr$learner$id, logloss = rr$aggregate(msr("classif.logloss")))
  }
})


tab = scores[[1]]
for (i in 2:length(scores)) {
  tab = dplyr::full_join(tab, as.data.table(scores[[i]]))
}

tab[grepl("with_trees", tab$learner_id), "learner_id"] = "ACWB_deep"
tab[grepl("autocompboost", tab$learner_id), "learner_id"] = "ACWB"
tab[grepl("glmnet", tab$learner_id), "learner_id"] = "glmnet"


tn = strsplit(tab$task_id, " ")
tab$task_id = sapply(tn, function(x) x[3])
