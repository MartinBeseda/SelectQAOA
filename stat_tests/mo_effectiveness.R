# Load necessary libraries
library(FSA)
library(PMCMRplus)
library(coin)

# Define the configurations
configs <- list(
  statevector = list(
    flex = list(
      qtcs = c(150, 150, 150, 150, 150, 150, 150, 150, 150, 150),
      div_ga = c(135, 140, 138, 139, 140, 139, 130, 138, 137, 139),
      qaoa = c(364, 327, 399, 335, 350, 358, 347, 356, 319, 343),
      add_greedy = c(104, 104, 104, 104, 104, 104, 104, 104, 104, 104)
    ),
    grep = list(
      qtcs = c(3, 3, 4, 3, 4, 3, 4, 3, 4, 4),
      div_ga = c(70, 70, 66, 70, 70, 68, 65, 69, 64, 69),
      qaoa = c(475, 437, 476, 486, 484, 464, 472, 474, 485, 467),
      add_greedy = c(97, 97, 97, 97, 97, 97, 97, 97, 97, 97)
    ),
    gzip = list(
      qtcs = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
      div_ga = c(55, 77, 62, 96, 51, 44, 56, 99, 79, 21),
      qaoa = c(108, 51, 26, 72, 85, 66, 75, 80, 18, 90),
      add_greedy = c(62, 62, 62, 62, 62, 62, 62, 62, 62, 62)
    ),
    sed = list(
      qtcs = c(66, 66, 66, 66, 66, 66, 66, 66, 66, 66),
      div_ga = c(103, 62, 104, 105, 98, 105, 101, 97, 105, 103),
      qaoa = c(107, 112, 125, 112, 104, 112, 130, 123, 115, 125),
      add_greedy = c(75, 75, 75, 75, 75, 75, 75, 75, 75, 75)
    )
  ),

  aer = list(
    flex = list(
      qtcs = c(187, 187, 187, 187, 187, 187, 187, 187, 187, 187),
      div_ga = c(140, 140, 140, 140, 140, 140, 140, 140, 140, 139),
      qaoa = c(483, 493, 482, 490, 491, 492, 490, 500, 490, 495),
      add_greedy = c(205, 205, 205, 205, 205, 205, 205, 205, 205, 205)
    ),
    grep = list(
      qtcs = c(1, 1, 2, 1, 2, 1, 2, 1, 2, 2),
      div_ga = c(70, 70, 70, 70, 70, 70, 70, 70, 70, 70),
      qaoa = c(335, 338, 337, 338, 336, 332, 336, 337, 336, 337),
      add_greedy = c(171, 171, 171, 171, 171, 171, 171, 171, 171, 171)
    ),
    gzip = list(
      qtcs = c(41, 40, 39, 40, 41, 41, 40, 41, 39, 41),
      div_ga = c(105, 105, 105, 105, 105, 105, 105, 105, 105, 105),
      qaoa = c(86, 128, 128, 127, 84, 86, 86, 84, 84, 86),
      add_greedy = c(26, 26, 26, 26, 26, 26, 26, 26, 26, 26)
    ),
    sed = list(
      qtcs = c(93, 93, 93, 93, 93, 93, 93, 93, 93, 93),
      div_ga = c(88, 62, 103, 103, 89, 98, 97, 94, 102, 103),
      qaoa = c(180, 175, 183, 183, 180, 180, 183, 180, 183, 180),
      add_greedy = c(80, 80, 80, 80, 80, 80, 80, 80, 80, 80)
    )
  ),
  
  fake_vigo = list(
    flex = list(
      qtcs = c(187, 187, 187, 187, 187, 187, 187, 187, 187, 187),
      div_ga = c(140, 140, 140, 140, 140, 140, 140, 140, 140, 140),
      qaoa = c(491, 491, 491, 492, 491, 492, 492, 492, 491, 491),
      add_greedy = c(205, 205, 205, 205, 205, 205, 205, 205, 205, 205)
    ),
    grep = list(
      qtcs = c(192, 192, 193, 192, 193, 192, 193, 192, 193, 193),
      div_ga = c(70, 70, 70, 70, 70, 70, 70, 70, 70, 70),
      qaoa = c(321, 321, 321, 321, 321, 321, 321, 321, 321, 321),
      add_greedy = c(177, 177, 177, 177, 177, 177, 177, 177, 177, 177)
    ),
    gzip = list(
      qtcs = c(41, 40, 39, 40, 41, 41, 40, 41, 39, 41),
      div_ga = c(105, 105, 105, 105, 105, 105, 105, 105, 105, 105),
      qaoa = c(102, 102, 76, 76, 76, 75, 75, 76, 101, 102),
      add_greedy = c(26, 26, 26, 26, 26, 26, 26, 26, 26, 26)
    ),
    sed = list(
      qtcs = c(86, 86, 86, 86, 86, 86, 86, 86, 86, 86),
      div_ga = c(89, 62, 103, 101, 89, 97, 97, 94, 101, 103),
      qaoa = c(183, 183, 183, 183, 183, 183, 183, 183, 183, 181),
      add_greedy = c(80, 80, 80, 80, 80, 80, 80, 80, 80, 80)
    )
  ), 
  
  noise_1 = list(
    flex = list(
      qtcs = c(187, 187, 187, 187, 187, 187, 187, 187, 187, 187),
      div_ga = c(140, 140, 140, 140, 140, 140, 140, 140, 140, 140),
      qaoa = c(491, 492, 491, 491, 491, 491, 491, 492, 491, 492),
      add_greedy = c(205, 205, 205, 205, 205, 205, 205, 205, 205, 205)
    ),
    grep = list(
      qtcs = c(192, 192, 193, 192, 193, 192, 193, 192, 193, 193),
      div_ga = c(70, 70, 70, 70, 70, 70, 70, 70, 70, 70),
      qaoa = c(321, 321, 321, 321, 321, 321, 321, 321, 321, 321),
      add_greedy = c(177, 177, 177, 177, 177, 177, 177, 177, 177, 177)
    ),
    gzip = list(
      qtcs = c(41, 40, 39, 40, 41, 41, 40, 41, 39, 41),
      div_ga = c(105, 105, 105, 105, 105, 105, 105, 105, 105, 105),
      qaoa = c(77, 75, 101, 101, 75, 102, 101, 74, 103, 76),
      add_greedy = c(26, 26, 26, 26, 26, 26, 26, 26, 26, 26)
    ),
    sed = list(
      qtcs = c(86, 86, 86, 86, 86, 86, 86, 86, 86, 86),
      div_ga = c(89, 62, 103, 101, 89, 97, 97, 94, 101, 103),
      qaoa = c(183, 183, 183, 183, 183, 183, 181, 183, 183, 183),
      add_greedy = c(80, 80, 80, 80, 80, 80, 80, 80, 80, 80)
    )
  ),
  
  noise_2 = list(
    flex = list(
      qtcs = c(187, 187, 187, 187, 187, 187, 187, 187, 187, 187),
      div_ga = c(140, 140, 140, 140, 140, 140, 140, 140, 140, 140),
      qaoa = c(491, 491, 492, 491, 492, 492, 492, 491, 491, 491),
      add_greedy = c(205, 205, 205, 205, 205, 205, 205, 205, 205, 205)
    ),
    grep = list(
      qtcs = c(192, 192, 193, 192, 193, 192, 193, 192, 193, 193),
      div_ga = c(70, 70, 70, 70, 70, 70, 70, 70, 70, 70),
      qaoa = c(321, 321, 321, 321, 321, 321, 321, 321, 321, 321),
      add_greedy = c(177, 177, 177, 177, 177, 177, 177, 177, 177, 177)
    ),
    gzip = list(
      qtcs = c(41, 40, 39, 40, 41, 41, 40, 41, 39, 41),
      div_ga = c(105, 105, 105, 105, 105, 105, 105, 105, 105, 105),
      qaoa = c(102, 103, 76, 101, 75, 102, 76, 76, 76, 76),
      add_greedy = c(26, 26, 26, 26, 26, 26, 26, 26, 26, 26)
    ),
    sed = list(
      qtcs = c(86, 86, 86, 86, 86, 86, 86, 86, 86, 86),
      div_ga = c(89, 62, 103, 101, 89, 97, 97, 94, 101, 103),
      qaoa = c(183, 183, 183, 183, 181, 183, 181, 183, 183, 183),
      add_greedy = c(80, 80, 80, 80, 80, 80, 80, 80, 80, 80)
    )
  ),
  
  noise_5 = list(
    flex = list(
      qtcs = c(187, 187, 187, 187, 187, 187, 187, 187, 187, 187),
      div_ga = c(140, 140, 140, 140, 140, 140, 140, 140, 140, 140),
      qaoa = c(492, 491, 492, 491, 492, 491, 491, 491, 491, 492),
      add_greedy = c(205, 205, 205, 205, 205, 205, 205, 205, 205, 205)
    ),
    grep = list(
      qtcs = c(192, 192, 193, 192, 193, 192, 193, 192, 193, 193),
      div_ga = c(70, 70, 70, 70, 70, 70, 70, 70, 70, 70),
      qaoa = c(321, 321, 321, 321, 321, 321, 321, 321, 321, 321),
      add_greedy = c(177, 177, 177, 177, 177, 177, 177, 177, 177, 177)
    ),
    gzip = list(
      qtcs = c(41, 40, 39, 40, 41, 41, 40, 41, 39, 41),
      div_ga = c(105, 105, 105, 105, 105, 105, 105, 105, 105, 105),
      qaoa = c(76, 75, 75, 101, 101, 102, 76, 76, 74, 103),
      add_greedy = c(26, 26, 26, 26, 26, 26, 26, 26, 26, 26)
    ),
    sed = list(
      qtcs = c(86, 86, 86, 86, 86, 86, 86, 86, 86, 86),
      div_ga = c(89, 62, 103, 101, 89, 97, 97, 94, 101, 103),
      qaoa = c(183, 183, 183, 183, 183, 183, 183, 183, 183, 183),
      add_greedy = c(80, 80, 80, 80, 80, 80, 80, 80, 80, 80)
    )
  )
)

# Function to compute A12 effect size
A12_effect_size <- function(x, y) {
  n_x <- length(x)
  n_y <- length(y)
  count <- sum(outer(x, y, ">")) + 0.5 * sum(outer(x, y, "=="))
  return(count / (n_x * n_y))
}

# Iterate through each configuration
for (config_name in names(configs)) {
  cat("\n=============================\n")
  cat("CONFIGURATION:", config_name, "\n")
  cat("=============================\n")
  
  # Iterate through each measurement group in the configuration
  for (group_name in names(configs[[config_name]])) {
    cat("\n--- Measurement Group:", group_name, "---\n")
    
    # Extract the data for the current group
    current_data <- configs[[config_name]][[group_name]]
    
    # Convert to dataframe for analysis
    data <- data.frame(
      value = unlist(current_data),
      group = rep(names(current_data), each = 10) # Each group has 10 values
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
    dunn_test <- dunnTest(value ~ group, data = data, method = "bh")
    cat("Dunn's Test (Benjamini-Hochberg correction):\n")
    print(dunn_test$res)  # Print only p-values
    
    # Compute A12 for each pair of groups
    group_list <- lapply(current_data, as.numeric)
    group_names <- names(group_list)
    
    a12_results <- data.frame()
    for (i in 1:(length(group_list) - 1)) {
      for (j in (i + 1):length(group_list)) {
        a12_value <- A12_effect_size(group_list[[i]], group_list[[j]])
        a12_results <- rbind(a12_results, data.frame(Group1 = group_names[i], Group2 = group_names[j], A12 = a12_value))
      }
    }
    
    # Print A12 effect size results
    cat("\nA12 Effect Size (probability that x > y)\n")
    print(a12_results)
  }
}

