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

results_df4 <- data.frame(Iteration = numeric(0), Sum_Average = numeric(0))
set.seed(2023)
for (iteration in 1:1000) {
  sample_frame_list <- list()
  for (df_name in data_list) {
    if (exists(df_name) && "count.in.Row" %in% colnames(get(df_name))) {
      # 2 Simple random sample without replacement
      sample_values <- sample(get(df_name)$count.in.Row, 1, replace = FALSE)
      # Record
      sample_df <- data.frame(
        SampleFrom = df_name,
        Randomrow1 = sample_values[1]
      )
      # Save Record
      sample_frame_list <- c(sample_frame_list, list(sample_df))
    }
  }
  # Save all Record and Get Averages
  final_sample_frame1 <- do.call(rbind, sample_frame_list)
  final_sample_frame1$Sum <- final_sample_frame1$Randomrow1
  result <- (sum(final_sample_frame1$Sum)/(12)) * 6 * 50
  results_df4 <- rbind(results_df4, data.frame(Iteration = iteration, Sum_Average = result))
}
# Clean code
rm(iteration, df_name, sample_values, sample_df, sample_frame_list, final_sample_frame1, result)
# Estimate performance
count_values1 <- table(results_df4$Sum_Average)
count_df1 <- data.frame(Sum_Average = as.numeric(names(count_values1)),
                        Count = as.numeric(count_values1))
Stra2_converge_mean <- mean(count_df1$Sum_Average)
Stra2_std <- sd(count_df1$Sum_Average)
confidence_level <- 0.90
margin_of_error4 <- qt((1 + confidence_level) / 2, df = nrow(count_df1) - 1) * (Stra2_std / sqrt(nrow(count_df1)))
Stra2_lower_bound <- Stra2_converge_mean - margin_of_error4
Stra2_upper_bound <- Stra2_converge_mean + margin_of_error4

gg4<-ggplot(count_df1, aes(x = Sum_Average, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_vline(aes(xintercept = Stra2_converge_mean, linetype = "Converge Mean"), color = "red") +  # 添加红线
  labs(title = "Stratified sampling 2 from 12 plots",
       x = "Predict_Total",
       y = "Count") +
  scale_linetype_manual(name = "Legend", values = c("Converge Mean" = "dashed")) +
  theme_minimal()
# Add a smooth curve
gg4 <- gg4 + geom_smooth(method = "loess", color = "green", linetype = "dotted")
gg4 <- gg4 + geom_vline(xintercept = Stra2_lower_bound, linetype = "dashed", color = "yellow")
gg4 <- gg4 + geom_vline(xintercept = Stra2_upper_bound, linetype = "dashed", color = "yellow")
# Print the ggplot
print(gg4)
# Clean code
rm(count_values1, count_df1)