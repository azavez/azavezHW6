
#Function List

#1 Make first row of matrix column headers
col_head <-function(x){
  col.names <- x[1,]
  y <- x[-1,]
  colnames(y) <- col.names
  y <- as.data.frame(y)
  return(y)
}

#2 Barplot That Compares Averages for Two Groups
compare.plot <- function(x, compare_column, group1){
  group1_sub = x[compare_column == group1,5:9]
  group2_sub = x[!compare_column == group1,5:9]
  group1_means <- colMeans(group1_sub)
  group1_means_t <- data.frame(t(data.frame(group1_means)))
  group2_means <- colMeans(group2_sub)
  group2_means_t <- data.frame(t(data.frame(group2_means)))
  all_means <- rbind(group1_means_t, group2_means_t)
  row.names(all_means) <- c(group1, "Avg for Other Days")
  all_means <- as.matrix(all_means)
  colnames(all_means) <- c("Studio", "Squash", "Gymnasium", "Cardio", "Weights")
  barplot(all_means, legend = c(group1, "Other Days"), main = "Average Fitness Center Member Count", col = c("navyblue","firebrick3"), beside = TRUE, ylab = "Avg. Number of Members", xlab = "Areas of Fitness Center")
}

#3 Creates Frequency Table from Character/Factor Vector
freq.plot <- function(x, xtitle, ytitle, mtitle){
  x <- as.factor(x)
  f_table <- as.data.frame(table(x))
  barplot(f_table$Freq, names.arg= f_table$x, xlab = xtitle, ylab = ytitle, main = mtitle, col = "grey", las = 3)
}

