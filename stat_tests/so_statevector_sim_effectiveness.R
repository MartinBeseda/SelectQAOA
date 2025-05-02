# Load required libraries
library(dunn.test)        # For Dunn's test
library(PMCMRplus)        # Alternative package for post-hoc tests
library(effsize)          # For A12 effect size
library(tidyverse)        # For data manipulation
library(rstudioapi)


setwd(dirname(getActiveDocumentContext()$path))
getwd() 

# Define datasets and repetitions
data_names <- c("gsdtsr", "paintcontrol", "iofrol", "elevator", "elevator2")
reps <- c(1, 2, 4, 8, 16)

# Define which metrics to extract per dataset
metrics_map <- list(
  gsdtsr = c("final_test_suite_costs", "final_failure_rates"),
  paintcontrol = c("final_test_suite_costs", "final_failure_rates"),
  iofrol = c("final_test_suite_costs", "final_failure_rates"),
  elevator = c("final_test_suite_costs", "final_input_divs"),
  elevator2 = c("final_test_suite_costs", "final_pcounts", "final_dists")
)

parse_vector_string <- function(s) {
  # Rimuove parentesi quadre, spazi, poi converte in numerico
  s <- gsub("\\[|\\]", "", s)
  as.numeric(strsplit(s, ",\\s*")[[1]])
}

# Function to load data for a single dataset and metric
load_data_for_metric <- function(dataset, metric) {
  df_list <- list()
  for (r in reps) {
    file_path <- sprintf("../results/selectqaoa/statevector_sim/%s-rep-%d.csv", dataset, r)
    if (file.exists(file_path)) {
      df <- read.csv(file_path, stringsAsFactors = FALSE)
      if (metric %in% colnames(df)) {
        values_list <- lapply(df[[metric]], parse_vector_string)
        values <- unlist(values_list)  # Flatten to numeric vector
        reps_vec <- rep(paste0("rep_", r), length(values))
        df_list[[paste0("rep_", r)]] <- data.frame(
          value = values,
          repetition = reps_vec
        )
      }
    }
  }
  bind_rows(df_list)
}

# Function to compute Kruskal-Wallis, Dunn and A12
analyze_metric <- function(data, metric_name, dataset_name) {
  cat("\n=== Dataset:", dataset_name, "| Metric:", metric_name, "===\n")
  
  if (length(unique(data$value)) <= 1) {
    cat("\n⚠️ All values are identical. Skipping statistical tests.\n")
    return()
  }
  
  # Kruskal-Wallis Test
  kruskal <- kruskal.test(value ~ repetition, data = data)
  cat("\n> Kruskal-Wallis Test\n")
  cat("H =", round(kruskal$statistic, 3), 
      " | df =", kruskal$parameter, 
      " | p-value =", format.pval(kruskal$p.value, digits = 4), "\n")
  
  # Post-hoc Dunn's Test
  dunn_result <- dunn.test(data$value, g = data$repetition, method = "bh", kw = FALSE, list = TRUE)
  
  # Prepare summary table
  summary_table <- data.frame(
    Comparison = character(),
    P_value = numeric(),
    Adjusted_P = numeric(),
    A12 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (k in seq_along(dunn_result$comparisons)) {
    comp <- dunn_result$comparisons[k]
    raw_p <- dunn_result$P[k]
    adj_p <- dunn_result$P.adjusted[k]
    
    # estrai gruppi x e y
    parts <- unlist(strsplit(comp, " - "))
    rep_x <- parts[1]
    rep_y <- parts[2]
    
    group_x <- data %>% filter(repetition == rep_x) %>% pull(value)
    group_y <- data %>% filter(repetition == rep_y) %>% pull(value)
    
    a12_val <- cliff.delta(group_x, group_y)$estimate
    
    summary_table <- rbind(summary_table, data.frame(
      Comparison = sprintf("%s > %s", rep_x, rep_y),
      P_value = round(raw_p, 4),
      Adjusted_P = round(adj_p, 4),
      A12 = round(a12_val, 3)
    ))
  }
  
  cat("\n> Post-hoc Dunn’s Test + A12 Effect Size:\n")
  print(summary_table, right = FALSE, row.names = FALSE)
}

# Main execution loop
for (dataset in data_names) {
  metrics <- metrics_map[[dataset]]
  for (metric in metrics) {
    data_metric <- load_data_for_metric(dataset, metric)
    if (nrow(data_metric) > 0) {
      analyze_metric(data_metric, metric, dataset)
    } else {
      cat(sprintf("No data found for %s - %s\n", dataset, metric))
    }
  }
}
