#Load Packages
library(dplyr)
library(ggplot2)
# Load Data
file_list <- list.files(pattern = "\\.csv$")
data_list <- list()
for (file in file_list) {
  data <- read.csv(file)
  df_name <- gsub("\\.csv$", "", file)
  assign(df_name, data)
  data_list[[df_name]] <- df_name
}
# Classify for Stratified sampling
for (df_name in data_list) {
  if (exists(df_name) && is.data.frame(get(df_name))) {
    df <- get(df_name)
    df$Classify <- ifelse(df$name %in% c('Row 1', 'Row 2', 'Row 3'), 'L',
                          ifelse(df$name %in% c('Row 4', 'Row 5', 'Row 6'), 'R', NA))
    assign(df_name, df)
  }
}
# Clean code
rm(file, data, df_name, df)
#=================================================
sample_frame_list <- list()
for (df_name in data_list) {
  middle_row1 <- 3
  middle_row2 <- 4
  df <- get(df_name)
  a <- df[middle_row1, ]$count.in.Row
  b <- df[middle_row2, ]$count.in.Row
  sample_df <- data.frame(
    SampleFrom = df_name,  
    RandomrowL = a,
    RandomrowR = b
  )
  sample_frame_list <- c(sample_frame_list, list(sample_df))
}
final_sample_frame4 <- do.call(rbind, sample_frame_list)
final_sample_frame4$Average <- final_sample_frame4$RandomrowL + final_sample_frame4$RandomrowR
Systematic_mean <- (sum(final_sample_frame4$Average)/(6*2))*50*6
rm(df_name, sample_frame_list, middle_row1, middle_row2, df, a, b, sample_df,final_sample_frame4)
#======================================================
results_df1 <- data.frame(Iteration = numeric(0), Sum_Average = numeric(0))
# Not change the randomly select results
set.seed(2023)
for (iteration in 1:1000) {
  sample_frame_list <- list()
  for (df_name in data_list) {
    if (exists(df_name) && "count.in.Row" %in% colnames(get(df_name))) {
      # 2 Simple random sample without replacement
      sample_values <- sample(get(df_name)$count.in.Row, 2, replace = FALSE)
      # Record
      sample_df <- data.frame(
        SampleFrom = df_name,
        Randomrow1 = sample_values[1],
        Randomrow2 = sample_values[2]
      )
      # Save Record
      sample_frame_list <- c(sample_frame_list, list(sample_df))
    }
  }
  # Save all Record and Get Averages
  final_sample_frame1 <- do.call(rbind, sample_frame_list)
  final_sample_frame1$Sum <- final_sample_frame1$Randomrow1 + final_sample_frame1$Randomrow2
  result <- (sum(final_sample_frame1$Sum)/(6*2))*50*6
  results_df1 <- rbind(results_df1, data.frame(Iteration = iteration, Sum_Average = result))
}
# Clean code
rm(iteration, df_name, sample_values, sample_df, sample_frame_list, final_sample_frame1, result)
# Estimate performance
count_values1 <- table(results_df1$Sum_Average)
count_df1 <- data.frame(Sum_Average = as.numeric(names(count_values1)),
                        Count = as.numeric(count_values1))
SRS_WO_converge_mean <- mean(count_df1$Sum_Average)
SRS_WO_std <- sd(count_df1$Sum_Average)
confidence_level <- 0.90
margin_of_error1 <- qt((1 + confidence_level) / 2, df = nrow(count_df1) - 1) * (SRS_WO_std / sqrt(nrow(count_df1)))
SRS_WO_lower_bound <- SRS_WO_converge_mean - margin_of_error1
SRS_WO_upper_bound <- SRS_WO_converge_mean + margin_of_error1

gg<-ggplot(count_df1, aes(x = Sum_Average, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(aes(xintercept = SRS_WO_converge_mean, linetype = "Converge Mean"), color = "red") +  # 添加红线
  labs(title = "Simple random sample without replacement from 6 plots",
       x = "Predict_Total",
       y = "Count") +
  scale_linetype_manual(name = "Legend", values = c("Converge Mean" = "dashed")) +
  theme_minimal()
# Add a smooth curve
gg <- gg + geom_smooth(method = "loess", color = "green", linetype = "dotted")
gg <- gg + geom_vline(xintercept = SRS_WO_lower_bound, linetype = "dashed", color = "yellow")
gg <- gg + geom_vline(xintercept = SRS_WO_upper_bound, linetype = "dashed", color = "yellow")
# Print the ggplot
print(gg)
# Clean code
rm(count_values1, count_df1)
#===============================================================================
results_df2 <- data.frame(Iteration = numeric(0), Sum_Average = numeric(0))
set.seed(2023)
for (iteration in 1:1000) {
  sample_frame_list <- list()
  for (df_name in data_list) {
    if (exists(df_name) && "count.in.Row" %in% colnames(get(df_name))) {
      # 2 Simple random sample without replacement
      sample_values <- sample(get(df_name)$count.in.Row, 2, replace = TRUE)
      # Record
      sample_df <- data.frame(
        SampleFrom = df_name,
        Randomrow1 = sample_values[1],
        Randomrow2 = sample_values[2]
      )
      # Save Record
      sample_frame_list <- c(sample_frame_list, list(sample_df))
    }
  }
  # Save all Record and Get Averages
  final_sample_frame2 <- do.call(rbind, sample_frame_list)
  final_sample_frame2$Sum <- final_sample_frame2$Randomrow1 + final_sample_frame2$Randomrow2
  result <- (sum(final_sample_frame2$Sum)/(6*2)) * 6 *50
  results_df2 <- rbind(results_df2, data.frame(Iteration = iteration, Sum_Average = result))
}
# Clean code
rm(iteration, df_name, sample_values, sample_df, sample_frame_list, final_sample_frame2, result)
# Estimate performance
count_values2 <- table(results_df2$Sum_Average)
count_df2 <- data.frame(Sum_Average = as.numeric(names(count_values2)),
                        Count = as.numeric(count_values2))
SRS_W_converge_mean <- mean(count_df2$Sum_Average)
SRS_W_std <- sd(count_df2$Sum_Average)
confidence_level <- 0.90
margin_of_error2 <- qt((1 + confidence_level) / 2, df = nrow(count_df2) - 1) * (SRS_W_std / sqrt(nrow(count_df2)))
SRS_W_lower_bound <- SRS_W_converge_mean - margin_of_error2
SRS_W_upper_bound <- SRS_W_converge_mean + margin_of_error2

gg2<-ggplot(count_df2, aes(x = Sum_Average, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(aes(xintercept = SRS_W_converge_mean, linetype = "Converge Mean"), color = "red") +  
  labs(title = "Simple random sample with replacement from 6 plots",
       x = "Predict_Total",
       y = "Count") +
  scale_linetype_manual(name = "Legend", values = c("Converge Mean" = "dashed")) +
  theme_minimal()
# Add a smooth curve
gg2 <- gg2 + geom_smooth(method = "loess", color = "green", linetype = "dotted")
gg2 <- gg2 + geom_vline(xintercept = SRS_W_lower_bound, linetype = "dashed", color = "yellow")
gg2 <- gg2 + geom_vline(xintercept = SRS_W_upper_bound, linetype = "dashed", color = "yellow")
# Print the ggplot
print(gg2)
# Clean code
rm(count_values2, count_df2)
#===============================================================================

set.seed(2023)
results_df3 <- data.frame(Iteration = numeric(0), Sum_Average = numeric(0))
for (i in 1:1000) {
  sample_frame_list <- list()
  for (df_name in data_list) {
    df <- get(df_name)
    df$Classify <- as.character(df$Classify)
    stratified_sample <- df %>%
      group_by(Classify) %>%
      sample_n(1)
    a <- stratified_sample$count.in.Row[stratified_sample$Classify == "L"]
    b <- stratified_sample$count.in.Row[stratified_sample$Classify == "R"]
    sample_df <- data.frame(
      SampleFrom = df_name,
      RandomrowL = a,
      RandomrowR = b
    )
    sample_frame_list <- c(sample_frame_list, list(sample_df))
  }
  final_sample_frame3 <- do.call(rbind, sample_frame_list)
  final_sample_frame3$Sum <- final_sample_frame3$RandomrowL + final_sample_frame3$RandomrowR
  result <- (sum(final_sample_frame3$Sum)/(6*2)) * 6 * 50
  results_df3 <- rbind(results_df3, data.frame(Iteration = i, Sum_Average = result))
}
rm(result, final_sample_frame3, sample_frame_list, sample_df, a, b, stratified_sample, df, df_name, i)
# Estimate performance
count_values3 <- table(results_df3$Sum_Average)
count_df3 <- data.frame(Sum_Average = as.numeric(names(count_values3)),
                        Count = as.numeric(count_values3))
Stra_converge_mean <- mean(count_df3$Sum_Average)
Stra_std <- sd(count_df3$Sum_Average)
confidence_level <- 0.90
margin_of_error3 <- qt((1 + confidence_level) / 2, df = nrow(count_df3) - 1) * (Stra_std / sqrt(nrow(count_df3)))
Stra_lower_bound <- Stra_converge_mean - margin_of_error3
Stra_upper_bound <- Stra_converge_mean + margin_of_error3

gg3<-ggplot(count_df3, aes(x = Sum_Average, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(aes(xintercept = Stra_converge_mean, linetype = "Converge Mean"), color = "red") +  
  labs(title = "Stratified sampling 1 from 6 plots",
       x = "Predict_Total",
       y = "Count") +
  scale_linetype_manual(name = "Legend", values = c("Converge Mean" = "dashed")) +
  theme_minimal()
gg3 <- gg3 + geom_smooth(method = "loess", color = "green", linetype = "dotted")
gg3 <- gg3 + geom_vline(xintercept = Stra_lower_bound, linetype = "dashed", color = "yellow")
gg3 <- gg3 + geom_vline(xintercept = Stra_upper_bound, linetype = "dashed", color = "yellow")
# Print the ggplot
print(gg3)
rm(count_values3, count_df3)







