
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

sample_frame_list <- list()
for (df_name in data_list) {
  if (exists(df_name) && "Count" %in% colnames(get(df_name))) {
    plot_row_cut_mean <- mean(get(df_name)$Count)
    plot_std <- sd(get(df_name)$Count)
    # Record
    sample_df <- data.frame(
      SampleFrom = df_name,
      row_mean = plot_row_cut_mean,
      plot_std = plot_std
      
    )
    # Save Record
    sample_frame_list <- c(sample_frame_list, list(sample_df))
  }
}
plots_message <- do.call(rbind, sample_frame_list)
rm(df_name,plot_row_cut_mean,plot_std,sample_df,sample_frame_list)

Real_total <- 0
for (df_name in data_list) {
  df <- get(df_name)
  Real_total <- Real_total + sum(df$Count)
}
rm(df_name,df)
Real_row__cut_mean <- Real_total/(50*6*4)
#====================================================================================
ggplot(data = plots_message, aes(x = SampleFrom, y = row_mean)) +
  geom_point() +
  labs(x = "Plots", y = "Plot_Row_Cut_Mean", title = "Split Row Means for each of 50 Plots (Row/4)") +
  geom_hline(aes(yintercept = Real_row__cut_mean), linetype = "dashed", color = "red") +
  geom_segment(aes(x = SampleFrom, y = row_mean, xend = SampleFrom, yend = Real_row__cut_mean), linetype = "dashed", color = "blue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#====================================================================================
ggplot(data = plots_message, aes(x = SampleFrom, y = plot_std)) +
  geom_point() +
  labs(x = "Plots", y = "Plot_std", title = "Standard Deviation for each of 50 Plots (Row/4)") +
  geom_segment(aes(x = SampleFrom, y = plot_std, xend = SampleFrom, yend = 0), linetype = "solid", color = "Gold") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






