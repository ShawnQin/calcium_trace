# Statistical comparison between AD and Ctr
library(ggplot2)
library(readxl)
library("viridis") 
library("ggpubr")

# load the data

# define the function to read the data
read_data <- function(AD_file_path, target_column){
  sheets_AD <- excel_sheets(AD_file_path)
  
  volume <- numeric(0)
  for (sheet in 2:length(sheets_AD)){
    data <- read_excel(AD_file_path,sheet)
    num_row = nrow(data)
    # column names
    column_index <- which(colnames(data) == target_column)
    if (length(column_index) == 0) {
      cat("Column not found.\n")
    } 
    else {
      volume <- c(volume, data[2:num_row,column_index+1])
    }
  }
  volume <- unlist(volume)
  return(volume)
}

### READ TWO DATASETS
AD_file_path <- "./data/Filter_AD_Paranode.xlsx"
ctr_file_path <- "./data/Fitler_Ctrl_Paranode.xlsx"

# Get sheet names
target_column <- "Volume (px³)"
AD_data <- read_data(AD_file_path,target_column)
ctr_data <- read_data(ctr_file_path,target_column)

### COMPARE THE DISTRIBUTION OF SPHEROIDS SIZE
# df_volume <- data.frame(AD = AD_data, Control = ctr_data)
combined_volume <- c(AD_data, ctr_data)
combined_labels <- c(rep("AD", length(AD_data)), rep("Control", length(ctr_data)))
df <- data.frame(
  volume = combined_volume,
  source = factor(combined_labels)  # Convert to a factor
)
# print(df)

# ECDF
g1<-ggplot(df, aes(x = volume, color = source)) +
  stat_ecdf(geom = "step", linewidth = 1.2) +
  labs(
    title = "ECDF of AD and Ctr",
    x = "Value",
    y = "ECDF",
    color = "Source"
  ) +
  theme_minimal()
g1

# PROBABILITY DENSITY
g2 <- ggplot(df, aes(x = volume, color = source)) +
  geom_density() +
  labs(title = "Probability Density",
       x = "Volune", y = "Density", color = "Condition") +
  theme_minimal()
g2

### Q-Q plot to compare the distribution
my.theme = list(theme(axis.line.y.left = element_line(linewidth = 0.5, color = "black"),
                      axis.line.y.right = element_line(linewidth = 0.5, color = "black"),
                      axis.line.x.top =  element_line(linewidth = 0.5, color ="black"),
                      axis.line.x.bottom = element_line(linewidth = 0.5, color = "black")))

qq.out <- qqplot(x=ctr_data, y=AD_data, plot.it=FALSE)
qq.out <- as.data.frame(qq.out)

names(qq.out) <- c("Control", "AD")  # Add batch names to output
# Set the x and y limits
xylim <- range( c(qq.out$Control, qq.out$AD) )

gp <- ggscatter(qq.out, x = "Control", y = "AD",
                size = 1.5, alpha = 0.5
                #add = "reg.line",                                 # Add regression line
                #conf.int = TRUE,                                  # Add confidence interval
) + xlim(0, 250) + ylim(0, 250) + coord_fixed() +
  geom_abline(intercept = 0, slope = 1) + 
  geom_hline(yintercept = 10,linetype = "dashed", size = 0.2) + 
  geom_vline(xintercept = 10,linetype = "dashed", size = 0.2) +
  font("xlab", size = 12) + font("ylab", size = 12) + 
  my.theme
# ggsave("qqplot_mTOR_5XF.pdf",dpi = 600, width = 3.5, height = 3.5)


# Using fewer data point to avoid cluter

# n = max(length(AD_data), length(ctr_data))
# p = (1:n - 1)/(n - 1)
p = (1:1000)/1000
threshold <- 55
q_c = quantile(ctr_data[ctr_data<=threshold],p,na.rm=TRUE)
q_AD = quantile(AD_data[AD_data <= threshold],p,na.rm=TRUE)
qs = data.frame(Control = q_c, AD = q_AD, Quantile = p*100)
#plot(q_m, q_f)
new_qq <- ggplot(qs,aes(x = Control, y= AD, colour = Quantile)) + geom_point(size = 1) + 
  coord_fixed() +
  xlim(0, 60) + ylim(0,60)  +
  scale_color_viridis(option = "D") +
  # scale_fill_distiller(palette = "Spectral") + 
  geom_abline(intercept = 0, slope = 1, linewidth = 0.3, colour = "gray") +
  # geom_hline(yintercept = 10,linetype = "dashed", linewidth = 0.2) +
  # geom_vline(xintercept = 10,linetype = "dashed", linewidth = 0.2) +
  font("xlab", size = 12) + font("ylab", size = 12) +
  theme_classic() + 
  theme(axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "top")
# new_qq
# save as a pdf
ggsave("nat_neuro_qq_plot_colored.pdf",dpi = 300, width = 4, height = 3.5)


### Statistical Test
max_size <- 55
ctr_subset <- ctr_data[ctr_data <= max_size]
AD_subset <- AD_data[AD_data <= max_size]

# one-sided t_test
# Perform Welch's t-test (one-sided)
t_test_result <- t.test(AD_subset, ctr_subset, 
                        alternative = "greater",  # One-sided test for mean(X) > mean(Y)
                        var.equal = FALSE)        # Welch's t-test

# Print the result
print(t_test_result)

# thresold = 10 
# ctr_proportion <- mean(ctr_subset > thresold, na.rm=TRUE)
# AD_proportion <- mean(AD_subset > thresold, na.rm=TRUE)
# 
# # Count successes and totals for z-test
# ctr_success <- sum(ctr_subset > thresold, na.rm = TRUE)
# ctr_total <- length(!is.na(ctr_subset))
# 
# AD_success <- sum(AD_subset > thresold, na.rm = TRUE)
# AD_total <- length(!is.na(AD_subset))
# 
# # Perform two-proportion z-test
# prop_test <- prop.test(c(ctr_success, AD_success), c(ctr_total, AD_total), alternative = "greater")

# Print results
# cat("Proportion of values > 10:\n")
# cat("mTOR Group: ", ctr_proportion, "\n")
# cat("5XF Group: ", AD_proportion, "\n\n")
# cat("Two-proportion z-test results:\n")
# print(prop_test)




### Logarithm the data and compare
df_log <- data.frame(
    volume = log10(combined_volume),
    source = factor(combined_labels)  # Convert to a factor
  )
print(df_log)

g3 <- ggplot(df_log, aes(x = volume, color = source)) +
  geom_density() +
  labs(title = "Probability Density",
       x = "Volune", y = "Density", color = "Condition") +
  theme_minimal()
g3


pdf_plot <- ggplot() +
  # Individual mTOR experiment curves (light gray)
  geom_density(data = df_log, aes(x = volume, color = source),  linewidth = 1) +
  xlab("log10(Size)") + ylab("pdf") + 
  theme_pubr() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) + 
  scale_color_manual(values=c("#E69F00", "#56B4E9"))
# pdf_plot
ggsave("nat_neuro_lognorma_pdf.pdf",dpi = 600, width = 4, height = 3)