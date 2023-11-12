# Load Packages
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
    df$Classify <- c(rep('L', 12), rep('R', 12))
    assign(df_name, df)
  }
}
# Clean code
rm(file, data, df_name, df)

set.seed(2023)
results_df3W <- data.frame(Iteration = numeric(0), Sum_Average = numeric(0))
for (i in 1:1000) {
  sample_frame_list <- list()
  for (df_name in data_list) {
    df <- get(df_name)
    df$Classify <- as.character(df$Classify)
    stratified_sample <- df %>%
      group_by(Classify) %>%
      slice(sample(n(), 2, replace = TRUE))
    counts_L <- filter(stratified_sample, Classify == "L")$Count
    counts_R <- filter(stratified_sample, Classify == "R")$Count
    sample_df <- data.frame(
      SampleFrom = df_name,
      RandomrowL1 = counts_L[1],
      RandomrowL2 = counts_L[2],
      RandomrowR1 = counts_R[1],
      RandomrowR2 = counts_R[2]
    )
    sample_frame_list <- c(sample_frame_list, list(sample_df))
  }
  final_sample_frame3 <- do.call(rbind, sample_frame_list)
  final_sample_frame3$Sum <- final_sample_frame3$RandomrowL1 + final_sample_frame3$RandomrowR1+final_sample_frame3$RandomrowL2 + final_sample_frame3$RandomrowR2
  result <- (sum(final_sample_frame3$Sum)/(12*4)) * 6 * 4 * 50
  results_df3W <- rbind(results_df3W, data.frame(Iteration = i, Sum_Average = result))
}
rm(result, final_sample_frame3, sample_frame_list, sample_df, a, b, stratified_sample, df, df_name, i)
# Estimate performance
count_values3W <- table(results_df3W$Sum_Average)
count_df3W <- data.frame(Sum_Average = as.numeric(names(count_values3W)),
                         Count = as.numeric(count_values3W))
Stra3W_converge_mean <- mean(count_df3W$Sum_Average)
Stra3W_std <- sd(count_df3W$Sum_Average)
confidence_level <- 0.90
margin_of_error3W <- qt((1 + confidence_level) / 2, df = nrow(count_df3W) - 1) * (Stra3W_std / sqrt(nrow(count_df3W)))
Stra3W_lower_bound <- Stra3W_converge_mean - margin_of_error3W
Stra3W_upper_bound <- Stra3W_converge_mean + margin_of_error3W

gg3W<-ggplot(count_df3W, aes(x = Sum_Average, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(aes(xintercept = Stra3W_converge_mean, linetype = "Converge Mean"), color = "red") +  
  labs(title = "Stratified sampling3 with replacement from 12 plots",
       x = "Predict_Total",
       y = "Count") +
  scale_linetype_manual(name = "Legend", values = c("Converge Mean" = "dashed")) +
  theme_minimal()
gg3W <- gg3W + geom_smooth(method = "loess", color = "green", linetype = "dotted")
gg3W <- gg3W + geom_vline(xintercept = Stra3W_lower_bound, linetype = "dashed", color = "yellow")
gg3W <- gg3W + geom_vline(xintercept = Stra3W_upper_bound, linetype = "dashed", color = "yellow")
# Print the ggplot
print(gg3W)
rm(count_values3W, count_df3W)

#===============================================================================

set.seed(2023)
results_df3Wo <- data.frame(Iteration = numeric(0), Sum_Average = numeric(0))
for (i in 1:1000) {
  sample_frame_list <- list()
  for (df_name in data_list) {
    df <- get(df_name)
    df$Classify <- as.character(df$Classify)
    stratified_sample <- df %>%
      group_by(Classify) %>%
      slice(sample(n(), 2, replace = FALSE))
    counts_L <- filter(stratified_sample, Classify == "L")$Count
    counts_R <- filter(stratified_sample, Classify == "R")$Count
    sample_df <- data.frame(
      SampleFrom = df_name,
      RandomrowL1 = counts_L[1],
      RandomrowL2 = counts_L[2],
      RandomrowR1 = counts_R[1],
      RandomrowR2 = counts_R[2]
    )
    sample_frame_list <- c(sample_frame_list, list(sample_df))
  }
  final_sample_frame3 <- do.call(rbind, sample_frame_list)
  final_sample_frame3$Sum <- final_sample_frame3$RandomrowL1 + final_sample_frame3$RandomrowR1+final_sample_frame3$RandomrowL2 + final_sample_frame3$RandomrowR2
  result <- (sum(final_sample_frame3$Sum)/(12*4)) * 6 * 4 * 50
  results_df3Wo <- rbind(results_df3Wo, data.frame(Iteration = i, Sum_Average = result))
}
rm(result, final_sample_frame3, sample_frame_list, sample_df, a, b, stratified_sample, df, df_name, i)
# Estimate performance
count_values3Wo <- table(results_df3Wo$Sum_Average)
count_df3Wo <- data.frame(Sum_Average = as.numeric(names(count_values3Wo)),
                          Count = as.numeric(count_values3Wo))
Stra3Wo_converge_mean <- mean(count_df3Wo$Sum_Average)
Stra3Wo_std <- sd(count_df3Wo$Sum_Average)
confidence_level <- 0.90
margin_of_error3Wo <- qt((1 + confidence_level) / 2, df = nrow(count_df3Wo) - 1) * (Stra3Wo_std / sqrt(nrow(count_df3Wo)))
Stra3Wo_lower_bound <- Stra3Wo_converge_mean - margin_of_error3Wo
Stra3Wo_upper_bound <- Stra3Wo_converge_mean + margin_of_error3Wo

gg3Wo<-ggplot(count_df3Wo, aes(x = Sum_Average, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(aes(xintercept = Stra3Wo_converge_mean, linetype = "Converge Mean"), color = "red") +  
  labs(title = "Stratified sampling3 without replacement from 12 plots",
       x = "Predict_Total",
       y = "Count") +
  scale_linetype_manual(name = "Legend", values = c("Converge Mean" = "dashed")) +
  theme_minimal()
gg3Wo <- gg3Wo + geom_smooth(method = "loess", color = "green", linetype = "dotted")
gg3Wo <- gg3Wo + geom_vline(xintercept = Stra3Wo_lower_bound, linetype = "dashed", color = "yellow")
gg3Wo <- gg3Wo + geom_vline(xintercept = Stra3Wo_upper_bound, linetype = "dashed", color = "yellow")
# Print the ggplot
print(gg3Wo)
rm(count_values3Wo, count_df3Wo)