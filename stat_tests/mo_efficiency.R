# Load necessary libraries
library(jsonlite)
library(FSA)
library(PMCMRplus)
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
getwd() 

# Define function to read QAOA execution times from JSON
read_qaoa_times <- function(program_name, config) {
  if (config == "ideal") {
    if (program_name == "grep") {
      file_path <- paste0("../results/selectqaoa/ideal/", program_name, "-data-rep-16.json")
    } else {
      file_path <- paste0("../results/selectqaoa/ideal/", program_name, "-data-rep-1.json")
    }
  } else {
    file_path <- paste0("../results/selectqaoa/", config, "/", program_name, "-data.json")
  }
  
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

# Define execution times for classical algorithms
# THIS VALUES HAVE BEEN COPIED FROM THE .JSON RESULTS OF THE DIVGA/AG/SELECTQA 
execution_times <- list(
  flex = list(
    qtcs = c(2997.246, 2990.427, 2994.078, 2990.127, 2996.457, 2999.987, 2996.602, 2999.604, 2996.952, 2998.930),
    div_ga = c(203222.63,204047.16,210975.58,225586.77,231729.42,211299.48,222097.04,214766.56,217488.39,252346.23),
    add_greedy = c(8335,8335,8335,8335,8335,8335,8335,8335,8335,8335)
  ),
  grep = list(
    qtcs = c(2988.608, 2993.446, 2996.067, 2993.917, 2989.430, 2996.594, 3000.109, 2987.973, 2991.871, 2993.156),
    div_ga = c(99435.02,82829.09,92250.62,85914.50,86121.72,85333.98,88159.41,82616.48,84917.17,92114.53),
    add_greedy = c(8884.02,8884.02,8884.02,8884.02,8884.02,8884.02,8884.02,8884.02,8884.02,8884.02)
  ),
  gzip = list(
    qtcs = c(2987.110, 2986.404, 2989.508, 2985.612, 2991.988, 2990.904, 2988.741, 2985.020, 2995.317, 2994.872),
    div_ga = c(17102.16, 17278.33, 17811.58, 18235.34, 18301.97, 18632.74, 20023.70, 20778.76, 22085.81, 23753.74),
    add_greedy = c(232,232.7,232.7,232.7,232.7,232.7,232.7,232.7,232.7,232.7)
  ),
  sed = list(
    qtcs = c(2997.188, 2998.320, 2988.932, 2998.150, 2989.302, 2991.186, 2992.056, 2994.666, 2998.397, 2998.455),
    div_ga = c(68588.03,74543.25,75144.77,81220.53,70520.88,72081.48,88311.10,85676.48,91617.75,84372.59),
    add_greedy = c(1896.88,1896.88,1896.88,1896.88,1896.88,1896.88,1896.88,1896.88,1896.88,1896.88)
  )
)

# Define QAOA configurations
qaoa_configs <- list(
  statevector_sim = "statevector_sim",
  ideal = "ideal",
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
        direction <- paste(group1, "<", group2)
      } else if (a12_value > 0.5) {
        direction <- paste(group1, ">", group2)
      } else {
        direction <- paste(group1, "≈", group2)  # If A12 = 0.5, groups are approximately equal
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
  cat("\n🔹 Pairwise Comparisons - Dunn's Test + A12 Effect Size:\n")
  print(results)
}

