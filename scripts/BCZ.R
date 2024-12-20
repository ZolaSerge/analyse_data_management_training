# Load the necessary packages
library(readr)
library(ggplot2)
library(tidyr)
library(plotly)


# Load data
data <- read_csv("data/total cote.csv")
data

data_sum <- data %>%
  filter(Niveau %in% c("Katuba", "Kenya", "Kisanga", "Lubumbashi", "Ruashi", "Tshamilemba")) %>%
  group_by(Niveau) %>%
  summarise(sum_pre_test = sum(pre_test),
            sum_post_test = sum(post_test))

print(data_sum)

# Transformer les données
data_long <- data_sum %>%
  pivot_longer(cols = c(sum_pre_test, sum_post_test), 
               names_to = "Test", 
               values_to = "Score")

# Ajuster l'ordre des niveaux
data_long$Test <- factor(data_long$Test, levels = c("sum_pre_test", "sum_post_test"))

data_long

# Créer le graphique
ggplot(data = data_long, aes(x = Niveau, y = Score, fill = Test)) +
  geom_col(position = "dodge") +
  labs(y = "Scores", fill = "Type de test") +
  theme_minimal()

ggplot(data = data_sum, aes(x = Niveau)) +
  #geom_bar(aes(y = sum_pre_test, fill = "Pré-test"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = sum_post_test, fill = "Post-test"), stat = "identity", position = "dodge") +
  labs(y = "Scores", fill = "Type de test") +
  theme_minimal()

  
Post-testggplot(data = eastern_cases, aes(x = reorder(district, count), y = count,
                                   fill = data_type)) +
  geom_col() +
  labs(x = "District", y = "Confirmed cases",
       title = "Confirmed cases in health facilities", subtitle = "Jan. 2020") +
  coord_flip() +
  scale_fill_manual(values = c("Confirmed" = "dodgerblue",
                               "Clinical" = "tomato",
                               "Confirmed_Passive_CHW" = "goldenrod2")) +
  theme_classic()

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
scatterplot_score

ggsave(filename = "plots/bcz/tshamilemba/scatterplot_score.pdf", plot = scatterplot_score)

# Create a whisker box diagram with ggplot2
whisker_box_score <- ggplot(data_long, aes(x = Test, y = Score, fill = Test)) +
  geom_boxplot() +
  labs(title = "Diagramme de Moustache des scores du pre-test & post-test",
       x = "Test",
       y = "Score") +
  theme_minimal()

ggsave(filename = "plots/bcz/tshamilemba/whisker_box_score.pdf", plot = whisker_box_score)

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
