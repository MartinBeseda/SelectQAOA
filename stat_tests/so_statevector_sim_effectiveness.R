library(jsonlite)
library(dunn.test)
library(effsize)
library(PMCMRplus)

# Load data
data <- fromJSON("../normalized_fvals.json")

# Helper: A12 effect size
a12 <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  sum(outer(x, y, FUN = function(xi, yj) ifelse(xi > yj, 1, ifelse(xi == yj, 0.5, 0)))) / (nx * ny)
}

# Helper: Cohen's d
cohen_d <- function(x, y) {
  n_x <- length(x)
  n_y <- length(y)
  s_pooled <- sqrt(( var(x) +  var(y)) / 2)
  (mean(x) - mean(y)) / s_pooled
}

for (program in names(data)) {
  cat("\n=== Program:", program, "===\n")
  
  reps <- names(data[[program]])
  df <- data.frame(
    value = unlist(data[[program]]),
    rep = factor(rep(rep(reps, each = 10), times = 1))
  )

  if (program %in% c("gsdtsr", "iofrol")) {
    # ANOVA
    aov_res <- aov(value ~ rep, data = df)
    print(summary(aov_res))
    
    # Tukey HSD
    tukey <- TukeyHSD(aov_res)
    print(tukey)

    # Cohen's d for each pair
    for (i in 1:(length(reps)-1)) {
      for (j in (i+1):length(reps)) {
        group1 <- as.numeric(data[[program]][[reps[i]]])
        group2 <- as.numeric(data[[program]][[reps[j]]])
        d <- cohen_d(group1, group2)
        cat(sprintf("Cohen's d (%s - %s): %.3f\n", reps[i], reps[j], d))
      }
    }

  } else {
    # Kruskal-Wallis
    kw <- kruskal.test(value ~ rep, data = df)
    cat(sprintf("Kruskal-Wallis H = %.4f, p-value = %.4f\n", as.numeric(kw$statistic), kw$p.value))
    
    # Dunn's test (use PMCMRplus version for data.frame support)
    dunn_res <- dunnTest(value ~ rep, data = df, method = "bonferroni")
    
    # Extract results
    results <- dunn_res$res
    names(results) <- tolower(names(results))  # for consistency
    
    # Extract effect size A12
    get_group_vals <- function(group_label) {
      df$value[df$rep == group_label]
    }
    
    # Split into Group1 and Group2
    pairs <- strsplit(results$comparison, " - ")
    results$group1 <- sapply(pairs, `[`, 1)
    results$group2 <- sapply(pairs, `[`, 2)
    
    # Calculate A12 for each pair
    results$a12 <- mapply(function(g1, g2) {
      x <- get_group_vals(g1)
      y <- get_group_vals(g2)
      a12(x, y)
    }, results$group1, results$group2)
    
    # Reorder and print nicely
    results <- results[, c("group1", "group2", "p.unadj", "p.adj", "a12")]
    colnames(results) <- c("Group1", "Group2", "p-value", "adj. p-value", "A12 effect size (x > y)")
    print(results, row.names = FALSE)
  }
}
