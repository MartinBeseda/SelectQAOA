# Load necessary libraries
library(jsonlite)
library(FSA)
library(PMCMRplus)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
getwd() 

# Define function to read QAOA execution times from JSON
read_qaoa_times <- function(program_name, config) {
  file_path <- paste0("../results/selectqaoa/", config, "/", program_name, "-data.json")
  
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  json_data <- fromJSON(file_path)  
  qaoa_times <- as.numeric(json_data$`all_qpu_run_times(ms)`)
  
  len_qaoa <- length(qaoa_times)
  
  # Compute qaoa_times2 (10 values averaging over each segment)
  step_size <- len_qaoa / 10
  qaoa_times2 <- sapply(0:9, function(i) {
    start_idx <- i * step_size
    end_idx <- start_idx + step_size - 1
    mean(qaoa_times[start_idx:end_idx])
  })
  
  return(qaoa_times2)
}

read_qtcs_times <- function(program) {
  file_path <- paste0("../results/selectqa/", program, "-data.json")
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  json_data <- fromJSON(file_path)
  return(as.numeric(json_data$`run_times(ms)`))
}

read_divga_times <- function(program) {
  file_path <- paste0("../results/divga/", program, "_pareto_fronts_divga.json")
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  json_data <- fromJSON(file_path)
  return(as.numeric(json_data$execution_times))
}

read_add_greedy_times <- function(program) {
  file_path <- paste0("../results/add-greedy/", program, "_data.json")
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  json_data <- fromJSON(file_path)
  resolution_time <- as.numeric(json_data$`resolution_time(ms)`)
  return(rep(resolution_time, 10))  # replico 10 volte per avere vettore compatibile
}

programs <- c("flex", "grep", "gzip", "sed")
execution_times <- list()

for (program in programs) {
  qtcs <- read_qtcs_times(program)
  div_ga <- read_divga_times(program)
  add_greedy <- read_add_greedy_times(program)

  execution_times[[program]] <- list(
    qtcs = qtcs,
    div_ga = div_ga,
    add_greedy = add_greedy
  )
}


# Define QAOA configurations
qaoa_configs <- list(
  statevector_sim = "statevector_sim",
  aer_sim = "aer_sim",
  fake_vigo = "fake_vigo",
  depolarizing_1 = "depolarizing_sim/01",
  depolarizing_2 = "depolarizing_sim/02",
  depolarizing_5 = "depolarizing_sim/05"
)

# Function to calculate A12 effect size (x < y)
A12_effect_size <- function(x, y) {
  n_x <- length(x)
  n_y <- length(y)
  count <- sum(outer(x, y, ">")) + 0.5 * sum(outer(x, y, "=="))
  return(count / (n_x * n_y))
}

# Process each program (flex, grep, gzip, sed)
for (program in names(execution_times)) {
  cat("\n=============================\n")
  cat("PROGRAM:", program, "\n")
  cat("=============================\n")
  
  # Extract execution times
  exec_data <- execution_times[[program]]
  
  # Read QAOA execution times from JSON files
  for (config_name in names(qaoa_configs)) {
    qaoa_times <- read_qaoa_times(program, qaoa_configs[[config_name]])
    
    if (!is.null(qaoa_times)) {
      exec_data[[paste0("qaoa_", config_name)]] <- qaoa_times
    }
  }
  
  # --- Summary: Print mean and standard deviation for each algorithm ---
  cat("\nSummary statistics (mean ± sd) for each algorithm:\n")
  for (alg_name in names(exec_data)) {
    alg_data <- as.numeric(exec_data[[alg_name]]) / 1000 # Convert to seconds
    m <- mean(alg_data)
    s <- sd(alg_data)
    cat(sprintf("  %s: %.2f ± %.2f\n", alg_name, m, s))
  }
  
  # Convert to dataframe for analysis
  data <- data.frame(
    value = unlist(exec_data),
    group = rep(names(exec_data), each = 10)
  )
  
  # Perform pairwise Kolmogorov-Smirnov test
  group_list <- split(data$value, data$group)
  ks_results <- data.frame()
  
  for (i in 1:(length(group_list) - 1)) {
    for (j in (i + 1):length(group_list)) {
      ks_test <- ks.test(group_list[[i]], group_list[[j]])
      ks_results <- rbind(ks_results, 
                          data.frame(Group1 = names(group_list)[i], 
                                     Group2 = names(group_list)[j], 
                                     Statistic = ks_test$statistic, 
                                     P_Value = ks_test$p.value))
    }
  }
  
  cat("\nKolmogorov-Smirnov Test Results:\n")
  print(ks_results)
  
  # Perform Kruskal-Wallis test
  kruskal_test <- kruskal.test(value ~ group, data = data)
  print(kruskal_test)
  
  # Perform Dunn's test
  dunn_test <- tryCatch({
    dunnTest(value ~ group, data = data, method = "bh")
  }, error = function(e) {
    warning("Dunn's test could not be performed: ", e$message)
    return(NULL)
  })
  
  if (is.null(dunn_test)) {
    cat("Skipping Dunn's test due to insufficient data\n")
    next
  }
  
  dunn_results <- dunn_test$res
  
  # Compute A12 for each pair of groups
  group_list <- lapply(exec_data, as.numeric)
  group_names <- names(group_list)
  
  results <- data.frame()
  
  for (i in 1:(length(group_list) - 1)) {
    for (j in (i + 1):length(group_list)) {
      group1 <- group_names[i]
      group2 <- group_names[j]
      a12_value <- A12_effect_size(group_list[[i]], group_list[[j]])
      
      # Determine direction based on A12 value
      if (a12_value > 0.5) {
        direction <- paste(group1, ">", group2)
      } else {
        direction <- paste(group1, "<", group2)
      } 
      
      # Check if comparison exists in Dunn's test
      comparison_str1 <- paste(group1, "-", group2)
      comparison_str2 <- paste(group2, "-", group1)
      
      dunn_row <- dunn_results[dunn_results$Comparison %in% c(comparison_str1, comparison_str2), ]
      
      # If no valid row found, skip the comparison
      if (nrow(dunn_row) == 0) {
        cat("\n⚠️ Skipping missing Dunn's test comparison:", group1, "-", group2, "\n")
        next  # Skip this iteration if no comparison found
      }
      
      # Extract p-value safely
      p_value <- dunn_row$P.unadj
      adj_p_value <- dunn_row$P.adj
      significant <- ifelse(adj_p_value < 0.05, "YES", "NO")
      
      # Append valid results
      results <- rbind(results, data.frame(
        Group1 = group1,
        Group2 = group2,
        A12_Value = round(a12_value, 3),  # Round A12 for better readability
        P_Value = p_value,
        ADJ_P_Value = adj_p_value,
        Significant = significant,
        Direction = direction,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Print Dunn test results and A12 effect sizes
  cat("\nPairwise Comparisons - Dunn's Test + A12 Effect Size:\n")
  print(results)
}

