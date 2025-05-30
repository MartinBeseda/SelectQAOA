# Librerie necessarie
library(dplyr)
library(tidyr)
library(data.table)
library(FSA)         # Per dunnTest
library(rcompanion)  # Per p-value adjustment

# Configurazioni
selectqaoa_configs <- c("statevector_sim", "aer_sim", "fake_vigo",
                        "depolarizing_sim/01", "depolarizing_sim/02", "depolarizing_sim/05")
igdec_qaoa_elevator2_configs <- c("ideal/qaoa_1/elevator_three", "noise/qaoa_1/elevator_one")
igdec_configs <- c("ideal", "noise")
results <- data.frame()

# Funzione A12 personalizzata
a12 <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  sum(outer(x, y, FUN = function(xi, yj) ifelse(xi > yj, 1, ifelse(xi == yj, 0.5, 0)))) / (nx * ny)
}

# Funzione Cohen's d personalizzata
cohen_d <- function(x, y) {
  s_pooled <- sqrt((var(x) + var(y)) / 2)
  (mean(x) - mean(y)) / s_pooled
}

# Parser di vettori in formato "[1,2,3]"
parse_vector <- function(s) {
  s <- gsub("\\[|\\]", "", s)
  as.numeric(unlist(strsplit(s, ",")))
}

# Caricamento da CSV
load_csv_column <- function(filepath, metric) {
  if (!file.exists(filepath)) return(NULL)
  df <- fread(filepath)
  parse_vector(df[[metric]][1])
}

# ------------------ ELEVATOR2 ------------------
dataset <- "elevator2"
dataset_file <- "elevator2-effectiveness-focus"
metrics_s <- c("final_test_suite_costs", "final_pcounts", "final_dists")
metrics_i <- c("final_test_suite_costs", "final_suite_pcounts", "final_suite_dists")

for (i in seq_along(metrics_s)) {
  for (sel_config in selectqaoa_configs) {
    sel_path <- file.path("..", "results", "selectqaoa", sel_config, paste0(dataset_file, ".csv"))
    sel_values <- load_csv_column(sel_path, metrics_s[i])
    if (is.null(sel_values)) next
    for (ig_config in igdec_qaoa_elevator2_configs) {
      ig_path <- file.path("..", "results", "igdec_qaoa", ig_config, "size_7", "10", "solution.csv")
      ig_values <- load_csv_column(ig_path, metrics_i[i])
      if (is.null(ig_values)) next
      mw <- wilcox.test(sel_values, ig_values, exact = FALSE)
      results <- rbind(results, data.frame(
        Dataset = dataset,
        Metric = metrics_s[i],
        SelectQAOA_Config = sel_config,
        IgDec_QAOA_Config = ig_config,
        P_Value = mw$p.value,
        A12 = round(a12(sel_values, ig_values), 3)
      ))
    }
  }
}

# ------------------ ELEVATOR ------------------
dataset <- "elevator"
dataset_file <- "elevator-effectiveness-focus"
metrics_s <- c("final_test_suite_costs", "final_effectivenesses")
metrics_i <- c("final_test_suite_costs", "final_suite_input_divs")

for (i in seq_along(metrics_s)) {
  for (sel_config in selectqaoa_configs) {
    sel_path <- file.path("..", "results", "selectqaoa", sel_config, paste0(dataset_file, ".csv"))
    sel_values <- load_csv_column(sel_path, metrics_s[i])
    if (is.null(sel_values)) next
    for (ig_config in igdec_configs) {
      ig_path <- file.path("..", "results", "igdec_qaoa", ig_config, "qaoa_1", "elevator_two", "size_7", "10", "solution.csv")
      ig_values <- load_csv_column(ig_path, metrics_i[i])
      if (is.null(ig_values)) next
      mw <- wilcox.test(sel_values, ig_values, exact = FALSE)
      results <- rbind(results, data.frame(
        Dataset = dataset,
        Metric = metrics_s[i],
        SelectQAOA_Config = sel_config,
        IgDec_QAOA_Config = ig_config,
        P_Value = mw$p.value,
        A12 = round(a12(sel_values, ig_values), 3)
      ))
    }
  }
}

# ------------------ IOFROL ------------------
dataset <- "iofrol"
dataset_file <- "iofrol-effectiveness-focus"
metrics_s <- c("final_test_suite_costs", "final_effectivenesses")
metrics_i <- c("final_test_suite_costs", "final_failure_rates")

for (i in seq_along(metrics_s)) {
  for (sel_config in selectqaoa_configs) {
    sel_path <- file.path("..", "results", "selectqaoa", sel_config, paste0(dataset_file, ".csv"))
    sel_values <- load_csv_column(sel_path, metrics_s[i])
    if (is.null(sel_values)) next
    for (ig_config in igdec_configs) {
      ig_path <- file.path("..", "results", "igdec_qaoa", ig_config, "qaoa_1", dataset, "size_7", "10", "solution.csv")
      ig_values <- load_csv_column(ig_path, metrics_i[i])
      if (is.null(ig_values)) next
      mw <- wilcox.test(sel_values, ig_values, exact = FALSE)
      results <- rbind(results, data.frame(
        Dataset = dataset,
        Metric = metrics_s[i],
        SelectQAOA_Config = sel_config,
        IgDec_QAOA_Config = ig_config,
        P_Value = mw$p.value,
        A12 = round(a12(sel_values, ig_values), 3)
      ))
    }
  }
}

# ------------------ GSDTSR (Kruskal-Wallis + Dunn + d) ------------------
print("------------------ GSDTSR (Kruskal-Wallis + Dunn + d) ------------------")
dataset <- "gsdtsr"
metrics <- c("final_test_suite_costs", "final_effectivenesses")

for (metric in metrics) {
  groups <- list()
  
  # SelectQA
  val <- load_csv_column("../results/selectqa/gsdtsr.csv", metric)
  if (!is.null(val)) groups[["SelectQA"]] <- val
  
  # BootQA
  alt_metric <- ifelse(metric == "final_effectivenesses", "final_failure_rates", metric)
  val <- load_csv_column("../results/bootqa/gsdtsr.csv", alt_metric)
  if (!is.null(val)) groups[["BootQA"]] <- val
  
  # IgDec
  for (ig_config in igdec_configs) {
    path <- file.path("..", "results", "igdec_qaoa", ig_config, "qaoa_1", "gsdtsr", "size_7", "10", "solution.csv")
    val <- load_csv_column(path, alt_metric)
    if (!is.null(val)) groups[[paste0("IgDec_", ig_config)]] <- val
  }
  
  # SelectQAOA
  for (sel_config in selectqaoa_configs) {
    path <- file.path("..", "results", "selectqaoa", sel_config, "gsdtsr-effectiveness-focus.csv")
    val <- load_csv_column(path, metric)
    if (!is.null(val)) groups[[paste0("SelectQAOA_", sel_config)]] <- val
  }
  
  # Kruskal-Wallis
  if (length(groups) < 2) next
  flat_values <- unlist(groups)
  group_labels <- rep(names(groups), times = sapply(groups, length))
  
  kw <- kruskal.test(flat_values ~ as.factor(group_labels))
  print(kruskal_test)
  dunn <- dunnTest(flat_values ~ as.factor(group_labels), method = "bonferroni")$res
  
  # Cohen's d for each pair
  for (i in 1:(length(groups)-1)) {
    for (j in (i+1):length(groups)) {
      g1 <- names(groups)[i]
      g2 <- names(groups)[j]
      d <- round(cohen_d(groups[[g1]], groups[[g2]]), 3)
      p_row <- dunn[dunn$Comparison == paste(g1, "-", g2), ]
      if (nrow(p_row) == 0) {
        p_row <- dunn[dunn$Comparison == paste(g2, "-", g1), ]
      }
      if (nrow(p_row) > 0) {
        cat(sprintf("Dataset: %s | Metric: %s | %s vs %s | p=%.4f | adj.p=%.4f | d=%.3f\n",
                    dataset, metric, g1, g2, p_row$P.unadj, p_row$P.adj, d))
      }
    }
  }
}

# ---------- paint control: ANOVA + Tukey HSD + Cohen's d ----------
print("--------- paint control: ANOVA + Tukey HSD + Cohen's d ----------")
dataset <- "paintcontrol"
metrics <- c("final_test_suite_costs", "final_effectivenesses")

for (metric in metrics) {
  groups <- list()
  
  # SelectQA
  val <- load_csv_column("../results/selectqa/paintcontrol.csv", metric)
  if (!is.null(val)) groups[["SelectQA"]] <- val
  
  # BootQA
  alt_metric <- ifelse(metric == "final_effectivenesses", "final_failure_rates", metric)
  val <- load_csv_column(file.path("..", "results", "bootqa", "paintcontrol.csv"), alt_metric)
  if (!is.null(val)) groups[["BootQA"]] <- val
  
  # IgDec
  for (ig_config in igdec_configs) {
    path <- file.path("..", "results", "igdec_qaoa", ig_config, "qaoa_1", "paintcontrol", "size_7", "10", "solution.csv")
    val <- load_csv_column(path, alt_metric)
    if (!is.null(val)) groups[[paste0("IgDec_", ig_config)]] <- val
  }
  
  # SelectQAOA
  for (sel_config in selectqaoa_configs) {
    path <- file.path("..", "results", "selectqaoa", sel_config, "paintcontrol-effectiveness-focus.csv")
    val <- load_csv_column(path, metric)
    if (!is.null(val)) groups[[paste0("SelectQAOA_", sel_config)]] <- val
  }
  
  # ANOVA + Tukey
  if (length(groups) < 2) next
  df <- data.frame(Value = unlist(groups),
                   Group = rep(names(groups), times = sapply(groups, length)))
  aov_model <- aov(Value ~ Group, data = df)
  print(summary(aov_model))
  tukey <- TukeyHSD(aov_model)
  tukey_df <- as.data.frame(tukey$Group)
  
  comparisons <- rownames(tukey_df)
  
  # Calcolo d per ogni coppia
  for (comp in comparisons) {
    comps <- unlist(strsplit(comp, "-"))
    g1 <- comps[1]
    g2 <- comps[2]
    if (!(g1 %in% names(groups)) || !(g2 %in% names(groups))) next
    d <- round(cohen_d(groups[[g1]], groups[[g2]]), 3)
    p <- tukey_df[comp, "p adj"]
    cat(sprintf("Dataset: %s | Metric: %s | %s vs %s | p=%.4f | adj.p=%.4f | d=%.3f\n",
                dataset, metric, g1, g2, tukey_df[comp, "p adj"], tukey_df[comp, "p adj"], d))
  }
}

# ----------- Risultati Mann-Whitney + A12 -----------
cat("\n=== RISULTATI MANN-WHITNEY + A12 ===\n")
print(results)
