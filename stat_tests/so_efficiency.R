library(dplyr)
library(readr)
library(jsonlite)
library(stringr)
library(coin)
library(FSA) # per dunnTest
library(effsize) # per cohen's d

datasets <- c("gsdtsr", "paintcontrol", "iofrol", "elevator", "elevator2")
selectqaoa_configs <- c("statevector_sim", "aer_sim", "fake_vigo",
                        "depolarizing_sim/01", "depolarizing_sim/02", "depolarizing_sim/05")
igdec_qaoa_configs <- c("ideal/", "noise/")
igdec_qaoa_elevator2_configs <- c("ideal/qaoa_1/elevator_three", "noise/qaoa_1/elevator_one")

# Funzioni per A12 e Cohen's d
a12 <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  sum(outer(x, y, FUN = function(xi, yj) ifelse(xi > yj, 1, ifelse(xi == yj, 0.5, 0)))) / (nx * ny)
}

cohen_d <- function(x, y) {
  s_pooled <- sqrt((var(x) + var(y)) / 2)
  (mean(x) - mean(y)) / s_pooled
}

# Helper per parse dei valori listati come stringa "[1,2,3]"
parse_list_column <- function(s) {
  as.numeric(str_extract_all(gsub("\\[|\\]", "", s), "[0-9.]+")[[1]])
}

for (dataset in datasets) {
  cat("\n=== Dataset:", dataset, "===\n")
  
  select_lists <- list()
  for (config in selectqaoa_configs) {
    file_path <- paste0("../results/selectqaoa/", config, "/", dataset, ".csv")
    if (!file.exists(file_path)) next
    data <- read_csv(file_path, show_col_types = FALSE)
    if (!"qpu_run_times(ms)" %in% names(data)) next
    times <- parse_list_column(data$`qpu_run_times(ms)`[1])
    means <- sapply(0:9, function(i) mean(times[(i * 10 ):(((i + 1) * 10)-1)]))
    select_lists[[config]] <- means
  }
  
  if (dataset == "elevator2") {
    igdec_lists <- list()
    for (conf in igdec_qaoa_elevator2_configs) {
      file_path <- paste0("../result/igdec_qaoa/", conf, "/size_7/10/solution.csv")
      if (!file.exists(file_path)) next
      data <- read_csv(file_path, show_col_types = FALSE)
      igdec_lists[[conf]] <- parse_list_column(data$execution_times[1])
    }
    
    # Pairwise comparison
    keys <- names(select_lists)
    for (i in 1:(length(keys)-1)) {
      for (j in (i+1):length(keys)) {
        x <- select_lists[[keys[i]]]
        y <- select_lists[[keys[j]]]
        pval <- wilcox_test(x ~ factor(c(rep("A", 10), rep("B", 10))))@distribution[[1]]
        cat(sprintf("SelectQAOA %s vs %s -> p = %.4f, A12 = %.4f\n",
                    keys[i], keys[j], pval, a12(x, y)))
      }
    }
    keys <- names(igdec_lists)
    for (i in 1:(length(keys)-1)) {
      for (j in (i+1):length(keys)) {
        x <- igdec_lists[[keys[i]]]
        y <- igdec_lists[[keys[j]]]
        pval <- wilcox_test(x ~ factor(c(rep("A", 10), rep("B", 10))))@distribution[[1]]
        cat(sprintf("IgDec %s vs %s -> p = %.4f, A12 = %.4f\n",
                    keys[i], keys[j], pval, a12(x, y)))
      }
    }
    
  } else if (dataset %in% c("elevator", "iofrol")) {
    igdec_lists <- list()
    for (conf in igdec_qaoa_configs) {
      file_path <- paste0("../result/igdec_qaoa/", conf, dataset, "/size_7/10/solution.csv")
      if (!file.exists(file_path)) next
      data <- read_csv(file_path, show_col_types = FALSE)
      igdec_lists[[conf]] <- parse_list_column(data$execution_times[1])
    }
    
    keys <- names(igdec_lists)
    for (i in 1:(length(keys)-1)) {
      for (j in (i+1):length(keys)) {
        x <- igdec_lists[[keys[i]]]
        y <- igdec_lists[[keys[j]]]
        pval <- wilcox_test(x ~ factor(c(rep("A", 10), rep("B", 10))))@distribution[[1]]
        cat(sprintf("IgDec %s vs %s -> p = %.4f, A12 = %.4f\n",
                    keys[i], keys[j], pval, a12(x, y)))
      }
    }
    
  } else if (dataset %in% c("gsdtsr", "paintcontrol")) {
    igdec_lists <- list()
    bootqa_file <- paste0("../results/bootqa/", dataset, ".csv")
    selectqa_file <- paste0("../results/selectqa/", dataset, ".csv")
    
    # bootQA
    bootqa_data <- read_csv(bootqa_file, show_col_types = FALSE)
    boot_list <- parse_list_column(bootqa_data$`exectution_times(ms)`[1])
    
    # selectQA
    selectqa_data <- read_csv(selectqa_file, show_col_types = FALSE)
    selectqa_time <- rep(selectqa_data$`average_qpu_access_time(ms)`[1], 10)
    
    # igdec
    for (conf in igdec_qaoa_configs) {
      file_path <- paste0("../result/igdec_qaoa/", conf, dataset, "/size_7/10/solution.csv")
      if (!file.exists(file_path)) next
      data <- read_csv(file_path, show_col_types = FALSE)
      igdec_lists[[conf]] <- parse_list_column(data$execution_times[1])
    }
    
    combined <- c(select_lists, list(BootQA = boot_list, SelectQA = selectqa_time), igdec_lists)
    group <- rep(names(combined), each = 10)
    values <- unlist(combined)
    
    kw_result <- kruskal.test(values ~ group)
    cat("Kruskal-Wallis p =", kw_result$p.value, "\n")
    
    # Dunn's test
    dunn_result <- dunnTest(values, group, method = "bonferroni")
    print(dunn_result)
    
    # Cohen's d
    for (i in 1:(length(combined) - 1)) {
      for (j in (i + 1):length(combined)) {
        d <- cohen_d(combined[[i]], combined[[j]])
        cat(sprintf("Cohen's d between %s and %s: %.4f\n",
                    names(combined)[i], names(combined)[j], d))
      }
    }
  }
}
