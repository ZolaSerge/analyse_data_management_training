# Load the necessary packages
library(readr)
library(ggplot2)
library(tidyr)
library(plotly)


# Load data
data <- read_csv("data/lubumbashi.csv")
data

# Transform data into long format
data_long <- data %>%
  pivot_longer(cols = c(pre_test, post_test), 
               names_to = "Test", 
               values_to = "Score")

# Specify the order of the Test factor levels
data_long$Test <- factor(data_long$Test, levels = c("pre_test", "post_test"))

# Perform t-test for paired samples
t_test_result <- t.test(data$pre_test, data$post_test, paired = TRUE)
print(t_test_result)

# Scatter plot with ggplot2
scatterplot_score <- ggplot(data_long, aes(x = Test, y = Score, color = Test)) +
  geom_point(position = position_jitter(width = 0.2), size = 3) +
  labs(title = "Nuage de points des scores du pre-test & post-test",
       x = "Test",
       y = "Score") +
  theme_minimal()

ggsave(filename = "plots/bcz/lubumbashi/scatterplot_score.pdf", plot = scatterplot_score)

# Create a whisker box diagram with ggplot2
whisker_box_score <- ggplot(data_long, aes(x = Test, y = Score, fill = Test)) +
  geom_boxplot() +
  labs(title = "Diagramme de Moustache des scores du pre-test & post-test ",
       x = "Test",
       y = "Score") +
  theme_minimal()

ggsave(filename = "plots/bcz/lubumbashi/whisker_box_score.pdf", plot = whisker_box_score)

# Convert to interactive graphics with plotly
ggplotly(whisker_box_score)

# Set Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Calculate mode for pre-test and post-test scores
mode_pre_test <- Mode(data$pre_test)
mode_post_test <- Mode(data$post_test)

# Show results
print(paste("le mode du Pre-test :", mode_pre_test))
print(paste("le mode du Post-test :", mode_post_test))
