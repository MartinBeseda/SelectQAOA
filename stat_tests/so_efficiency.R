library(jsonlite)
library(coin)
library(FSA)       # for dunnTest
library(effsize)   # for cohen's d fallback
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
getwd() 

# A12 Effect Size
a12 <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  sum(outer(x, y, FUN = function(xi, yj) ifelse(xi > yj, 1, ifelse(xi == yj, 0.5, 0)))) / (nx * ny)
}

# Cohen's d custom
cohen_d <- function(x, y) {
  s_pooled <- sqrt((var(x) + var(y)) / 2)
  (mean(x) - mean(y)) / s_pooled
}

datasets <- c("elevator2", "elevator", "iofrol", "gsdtsr", "paintcontrol")
selectqaoa_configs <- c("statevector_sim", "aer_sim", "fake_vigo",
                        "depolarizing_sim/01", "depolarizing_sim/02", "depolarizing_sim/05")
igdec_qaoa_elevator2_configs <- c("ideal/qaoa_1/elevator_three", "noise/qaoa_1/elevator_one")
igdec_qaoa_configs <- c("ideal/qaoa_1/", "noise/qaoa_1/")

for (dataset in datasets) {
  cat(paste0("\n=========================\n", toupper(dataset), "\n=========================\n\n"))
  
  all_lists <- list()
  names_lists <- c()
  
  # SelectQAOA
  for (config in selectqaoa_configs) {
    file_path <- paste0("../results/selectqaoa/", config, "/", dataset, ".csv")
    if (!file.exists(file_path)) next
    data <- read.csv(file_path)
    qaoa_times <- fromJSON(data[["qpu_run_times.ms."]][1])
    len_qaoa <- length(qaoa_times)
    step_size <- len_qaoa / 10
    qaoa_times2 <- sapply(0:9, function(i) {
      start_idx <- i * step_size + 1
      end_idx <- start_idx + step_size - 1
      mean(qaoa_times[start_idx:end_idx])
    })
    all_lists[[paste0("SelectQAOA_", config)]] <- qaoa_times2
    names_lists <- c(names_lists, paste0("SelectQAOA_", config))
  }
  
  # IgDec_QAOA
  if (dataset == "elevator2") {
    for (config in igdec_qaoa_elevator2_configs) {
      file_path <- paste0("../results/igdec_qaoa/", config, "/size_7/10/solution.csv")
      if (!file.exists(file_path)) next
      data <- read.csv(file_path)
      exec_times <- fromJSON(data[["execution_times"]][1])
      all_lists[[paste0("IgDecQAOA_", config)]] <- exec_times
      names_lists <- c(names_lists, paste0("IgDecQAOA_", config))
    }
  } else if (dataset %in% c("elevator", "iofrol", "gsdtsr", "paintcontrol")) {
    for (config in igdec_qaoa_configs) {
      file_path <- paste0("../results/igdec_qaoa/", config, dataset, "/size_7/10/solution.csv")
      if (!file.exists(file_path)) next
      data <- read.csv(file_path)
      exec_times <- fromJSON(data[["execution_times"]][1])
      all_lists[[paste0("IgDecQAOA_", config, dataset)]] <- exec_times
      names_lists <- c(names_lists, paste0("IgDecQAOA_", config, dataset))
    }
  }
  
  # Solo per gsdtsr e paintcontrol
  if (dataset %in% c("gsdtsr", "paintcontrol")) {
    # SelectQA
    file_selectqa <- paste0("../results/selectqa/", dataset, ".csv")
    if (file.exists(file_selectqa)) {
      val <- as.numeric(read.csv(file_selectqa)[["average_qpu_access_time.ms."]][1])
      all_lists[["SelectQA"]] <- rep(val, 10)
      names_lists <- c(names_lists, "SelectQA")
    }
    
    # BootQA
    file_bootqa <- paste0("../results/bootqa/", dataset, ".csv")
    if (file.exists(file_bootqa)) {
      val <- fromJSON(read.csv(file_bootqa)[["exectution_times.ms."]][1])
      all_lists[["BootQA"]] <- val
      names_lists <- c(names_lists, "BootQA")
    }
  }
  
  if (dataset %in% c("gsdtsr", "paintcontrol")) {
    cat(">>> KRUSKAL-WALLIS + DUNN + COHEN'S D <<<\n\n")
    
    # Prepare data frame
    full_data <- data.frame(value = numeric(), group = character())
    for (i in seq_along(all_lists)) {
      full_data <- rbind(full_data,
                         data.frame(value = all_lists[[i]],
                                    group = rep(names_lists[i], length(all_lists[[i]]))))
    }
    
    # Kruskal-Wallis
    kruskal <- kruskal.test(value ~ group, data = full_data)
    cat("Kruskal-Wallis: p =", format.pval(kruskal$p.value), 
        ", H =", round(kruskal$statistic, 3), "\n\n")
    
    # Dunn test with Bonferroni
    dunn <- dunnTest(value ~ group, data = full_data, method = "bonferroni")
    dunn_result <- dunn$res
    
    for (row in 1:nrow(dunn_result)) {
      groups <- unlist(strsplit(dunn_result$Comparison[row], " - "))
      g1 <- all_lists[[groups[1]]]
      g2 <- all_lists[[groups[2]]]
      d <- cohen_d(g1, g2)
      cat(paste0("[", groups[1], "] vs [", groups[2], "]: p = ",
                 format.pval(dunn_result$P.unadj[row], digits = 3),
                 ", adj p = ", format.pval(dunn_result$P.adj[row], digits = 3),
                 ", d = ", round(d, 3), "\n"))
    }
  } else {
    cat(">>> MANN-WHITNEY TEST + A12 EFFECT SIZE <<<\n\n")
    for (i in 1:(length(all_lists) - 1)) {
      for (j in (i + 1):length(all_lists)) {
        x <- all_lists[[i]]
        y <- all_lists[[j]]
        df <- data.frame(
          value = c(x, y),
          group = factor(c(rep("x", length(x)), rep("y", length(y))))
        )
        mw_test <- wilcox_test(value ~ group, data = df, distribution = "exact")
        a12_val <- a12(x, y)
        cat(paste0("[", names_lists[i], "] vs [", names_lists[j], "]: p-value = ",
                   format.pval(pvalue(mw_test), digits = 4),
                   ", A12 = ", round(a12_val, 3), "\n"))
      }
    }
  }
}
