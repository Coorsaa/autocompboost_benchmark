library(ggplot2)
library(dplyr)
# devtools::install_github("zeehio/facetscales")
library(facetscales)
library(ggh4x)
library(ggthemes)
# folds = 1:10
# tasks = paste0("t", LETTERS[1:15])
# framework = paste0("f", LETTERS[1:8])
# type = data.frame(type = sample(c("auc", "logloss"), length(tasks), TRUE), task = tasks)
#
# dat = expand.grid(task = tasks, fold = folds, framework = framework)
#
# dat = dat %>%
#   arrange(task, framework, fold) %>%
#   mutate(score = runif(nrow(dat), 0.8, 1)) %>%
#   left_join(type, by = "task")


dat = read.csv("scores_complete.csv")
head(dat)
#### Figure code:

getBest = function(score, type) {
  minmax = min
  if (type[1] == "auc")
    minmax = max
  ifelse(score == minmax(score), "best", "other")
}
getDiffToBest = function(score, type) {
  minmax = min
  if (type[1] == "auc")
    minmax = max
  idx = minmax(score) == score
  score_best = score[idx]
  abs(score_best - score) / abs(score_best)
}
dat_plt = dat %>%
  group_by(task, framework) %>%
  summarize(score = mean(score), type = type[1]) %>%
  group_by(task) %>%
  mutate(best = getBest(score, type), sdiff = getDiffToBest(score, type)) %>%
  group_by(type) %>%
  mutate(task_num = as.integer(as.factor(task)), framework_num = as.integer(as.factor(framework)))

idx_binary = dat_plt$type == "auc"
scales_x = list(
  "Binary classification" = scale_x_continuous(breaks = unique(dat_plt$task_num[idx_binary]), labels = as.character(unique(dat_plt$task[idx_binary]))),
  "Multiclass classification" = scale_x_continuous(breaks = unique(dat_plt$task_num[! idx_binary]), labels = as.character(unique(dat_plt$task[! idx_binary])))
)

dat_plt$ptype = ifelse(dat_plt$type == "auc", "Binary classification", "Multiclass classification")
gg_bm = ggplot() +
  geom_tile(data = dat_plt, aes(y = framework_num, x = task_num, fill = sdiff), width = 0.95) +
  geom_tile(data = dat_plt %>% filter(best == "best"), aes(y = framework_num, x = task_num), fill = "#003000", width = 0.95) +
  geom_text(data = dat_plt, aes(x = task_num, y = framework_num, label = round(score, 2)), size = 2) +
  geom_text(data = dat_plt %>% filter(best == "best"), aes(x = task_num, y = framework_num, label = round(score, 2)),
    color = "white", fontface = "bold", size = 2) +
  scale_y_continuous(breaks = unique(dat_plt$framework_num), labels = unique(dat_plt$framework)) +
  xlab("") +
  ylab("") +
  labs(fill = "Difference to\nbest framework") +
  colorspace::scale_fill_continuous_sequential(palette = "Greens", rev = FALSE) +
  #colorspace::scale_fill_continuous_sequential(palette = "Viridis") +
  facetscales::facet_grid_sc(cols = vars(ptype), scales = list(x = scales_x)) +
  ggh4x::force_panelsizes(cols = max(table(idx_binary) / table(idx_binary))) +
  theme_minimal(base_family = "Arial") +
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(color = "black", size = 7),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    plot.title = element_text(size = 9),
    plot.subtitle = element_text(size = 8),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# ggsave(
#   plot = gg_bm,
#   filename = "figures/fig-bm.png",
#   width = dinA4width,
#   height = dinA4width * 0.6,
#   units = "mm")