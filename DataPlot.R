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
    df$Classify <- ifelse(df$name %in% c('Row 1', 'Row 2', 'Row 3'), 'L',
                          ifelse(df$name %in% c('Row 4', 'Row 5', 'Row 6'), 'R', NA))
    assign(df_name, df)
  }
}
# Clean
rm(file, data, df_name, df)
# Get plots' row mean and std
sample_frame_list <- list()
for (df_name in data_list) {
  if (exists(df_name) && "count.in.Row" %in% colnames(get(df_name))) {
    plot_row_mean <- mean(get(df_name)$count.in.Row)
    plot_std <- sd(get(df_name)$count.in.Row)
    # Record
    sample_df <- data.frame(
      SampleFrom = df_name,
      row_mean = plot_row_mean,
      plot_std = plot_std
      
    )
    # Save Record
    sample_frame_list <- c(sample_frame_list, list(sample_df))
  }
}
plots_message <- do.call(rbind, sample_frame_list)
rm(df_name,plot_row_mean,plot_std,sample_df,sample_frame_list)
# Get total of 50 plot count and real mean
Real_total <- 0
for (df_name in data_list) {
  df <- get(df_name)
  Real_total <- Real_total + sum(df$count.in.Row)
}
rm(df_name,df)
Real_row_mean <- Real_total/(50*6)
#====================================================================================
# Graph for "Row Means for each of 50 Plots (Row)" vs 'real mean'
ggplot(data = plots_message, aes(x = SampleFrom, y = row_mean)) +
  geom_point() +
  labs(x = "Plots", y = "Plot_Row_Mean", title = "Row Means for each of 50 Plots (Row)") +
  geom_hline(aes(yintercept = Real_row_mean), linetype = "dashed", color = "red") +
  geom_segment(aes(x = SampleFrom, y = row_mean, xend = SampleFrom, yend = Real_row_mean), linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#====================================================================================
# Graph for "Standard Deviation for each of 50 Plots (Row)"
ggplot(data = plots_message, aes(x = SampleFrom, y = plot_std)) +
  geom_point() +
  labs(x = "Plots", y = "Plot_std", title = "Standard Deviation for each of 50 Plots (Row)") +
  geom_segment(aes(x = SampleFrom, y = plot_std, xend = SampleFrom, yend = 0), linetype = "solid", color = "Gold") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#====================================================================================
# Get total of 50 plot count and real mean
set.seed(2023)
results_df2 <- data.frame(Iteration = numeric(0), Sum_Average = numeric(0))
for (i in 1:100) {
  sample_frame_list <- list()
  for (df_name in data_list) {
    
    if (exists(df_name) && "count.in.Row" %in% colnames(get(df_name))) {
      sample_values <- sample(get(df_name)$count.in.Row, i, replace = TRUE)
      
      sample_df <- data.frame(
        SampleFrom = df_name,
        Randomrowmean = mean(sample_values)
      )
      sample_frame_list <- c(sample_frame_list, list(sample_df))
    }
    
  }
  final_sample_frame2 <- do.call(rbind, sample_frame_list)
  result <- var(final_sample_frame2$Randomrowmean)
  results_df2 <- rbind(results_df2, data.frame(Iteration = i, Variance = result))
}
rm(final_sample_frame2,sample_df,sample_frame_list,df_name,i,result,sample_values)

plot(results_df2$Iteration, results_df2$Variance, 
     xlab = "The_number_of_rows_taken_from_each_plot", ylab = "Variance",
     main = "Sample size vs. Variance (SRS with replacement)")

