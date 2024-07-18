library(ggplot2)
library(readxl)
library(magrittr)
library(dplyr)
library(tidyr)


df_raw <- read_excel('plot_temi.xlsx', skip = 1)
# col_names <- df_raw[1,]
# colnames(df_raw) <- df_raw
df_raw_name <- df_raw[-1,] 

df_raw_name <-  df_raw_name |>  rename(Variable =1,
         RRa_urban = 2,
         CCA_urban = 3,
         RRA_rural = 4,
         CCA_rural = 5) 

df_raw_name <- na.omit(df_raw_name)  

df_raw_clean <- df_raw_name %>% 
  separate(col = CCA_urban, into = c("CCA_ulower", "CCA_uupper"), sep = ",") %>% 
  separate(col = CCA_rural, into = c("CCA_rlower", "CCA_rupper"), sep = ",") %>% 
  mutate(across(-1, ~as.numeric(stringr::str_remove_all(., "\\(|\\)")))) 

df_raw_clean["Variable"][df_raw_clean["Variable"] == "45436"] <- "5-24"


# 
# df_raw_cleaned <- df_raw_clean %>% pivot_longer(cols = -c('RRA_rural', 'RRa_urban'),names_to = 'Variable', values_to = "Data") %>% na.omit()
# 
# df_raw_cleaned <- subset(df_raw_cleaned, Variable != 'Variable')

# plot <- ggplot(df_raw_cleaned, aes(Variable, Data)) + 
#   geom_boxplot() +
#   geom_point(aes(x = Variable, y = RRA_rural), color = 'blue')


# Create forest plot
ggplot(data = df_raw_clean, aes(y = Variable, x = RRa_urban, xmin = CCA_ulower, xmax = CCA_uupper)) +
  geom_point() +
  geom_errorbarh(height = 0.1) +
  #scale_y_continuous(breaks = 1:nrow(df_raw_clean), labels = df_raw_clean$Variable) +
  labs(title = 'Effect Size by Study', x = 'Effect Size', y = 'Study') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(0.9, max(df_raw_clean$CCA_uupper)))

ggplot(data = df_raw_clean, aes(y = Variable, x = RRA_rural, xmin = CCA_rlower, xmax = CCA_rupper)) +
  geom_point() +
  geom_errorbarh(height = 0.1) +
  #scale_y_continuous(breaks = 1:nrow(df_raw_clean), labels = df_raw_clean$Variable) +
  labs(title = 'Effect Size by Study', x = 'Effect Size', y = 'Study') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(0.9, max(df_raw_clean$CCA_rupper)+0.5))# Adjust the x-axis limits



#Efficient code

#### FInal----

library(patchwork)

df_raw_clean <- df_raw_clean %>% 
  mutate(Variable = factor(Variable, levels = c("Male", "Female", 
                                                "Asian/Pacific Islander", "Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other",
                                                "0-4", "5-24", "25-44", "45-64", "65-84", "85+")))

p1 <- ggplot(data = df_raw_clean, aes(y = Variable, x = RRa_urban, xmin = CCA_ulower, xmax = CCA_uupper)) +
  geom_point(color = "blue") +
  geom_errorbarh(height = 0.1, color = "blue") +
  labs(title = 'Urban', x = 'Relative Risk', y = ' ') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(0.0, max(c(df_raw_clean$CCA_uupper, df_raw_clean$CCA_rupper))))

p2 <- ggplot(data = df_raw_clean, aes(y = Variable, x = RRA_rural, xmin = CCA_rlower, xmax = CCA_rupper)) +
  geom_point(color = "red") +
  geom_errorbarh(height = 0.1, color = "red") +
  labs(title = 'Rural', x = 'Relative Risk', y = ' ') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(0.0, max(c(df_raw_clean$CCA_uupper, df_raw_clean$CCA_rupper)))) +
  theme(axis.title.y = element_blank())

p1/p2



#updated---
labels <- data.frame(
  Variable = c("Male", "0-4", "Asian/Pacific Islander"),
  label = c("{Sex}", "{Age}", "{Race/Ethnicity}")
)

p1 <- ggplot(data = df_raw_clean, aes(y = Variable, x = RRa_urban, xmin = CCA_ulower, xmax = CCA_uupper)) +
  geom_point(color = "blue") +
  geom_errorbarh(height = 0.1, color = "blue") +
  labs(title = 'Urban', x = 'Relative Risk', y = ' ') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(0.0, max(c(df_raw_clean$CCA_uupper, df_raw_clean$CCA_rupper)))) +
  geom_text(data = labels, aes(y = Variable, label = label), x = -0.2, size = 6, color = "black", hjust = 1)

p2 <- ggplot(data = df_raw_clean, aes(y = Variable, x = RRA_rural, xmin = CCA_rlower, xmax = CCA_rupper)) +
  geom_point(color = "red") +
  geom_errorbarh(height = 0.1, color = "red") +
  labs(title = 'Rural', x = 'Relative Risk', y = ' ') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(0.0, max(c(df_raw_clean$CCA_uupper, df_raw_clean$CCA_rupper)))) +
  theme(axis.title.y = element_blank()) +
  geom_text(data = labels, aes(y = Variable, label = label), x = -0.2, size = 6, color = "black", hjust = 1)

p1/p2

#drawing on them
p1 <- p1 + 
  pBrackets::grid.brackets(x = -0.2, y = c(13.5, 8.5, 1.5), 
                           labels = c("{Sex}", "{Age}", "{Race/Ethnicity}"), 
                           brackets = c("{", "}", "{"), 
                           col = "black", size = 6, hjust = 1)

p2 <- p2 + 
  pBrackets::grid.brackets(x = -0.2, y = c(13.5, 8.5, 1.5), 
                           labels = c("{Sex}", "{Age}", "{Race/Ethnicity}"), 
                           brackets = c("{", "}", "{"), 
                           col = "black", size = 6, hjust = 1)

p1/p2


#
# Your existing code for p1 and p2...
# library(patchwork)
# library(ggplot2)
# library(dplyr)
# 
# df_raw_clean <- df_raw_clean %>% 
#   mutate(Variable = factor(Variable, levels = c("Male", "Female", 
#                                                 "Asian/Pacific Islander", "Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other",
#                                                 "0-4", "5-24", "25-44", "45-64", "65-84", "85+")))
# 
# add_brackets <- function(p, y_start, y_end, label, x_pos = -0.2) {
#   p + 
#     annotate("segment", x = x_pos, xend = x_pos, y = y_start, yend = y_end, color = "black") +
#     annotate("segment", x = x_pos, xend = x_pos - 0.05, y = y_start, yend = y_start, color = "black") +
#     annotate("segment", x = x_pos, xend = x_pos - 0.05, y = y_end, yend = y_end, color = "black") +
#     annotate("text", x = x_pos - 0.1, y = (y_start + y_end) / 2, label = label, angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 4)
# }
# 
# p1 <- ggplot(data = df_raw_clean, aes(y = Variable, x = RRa_urban, xmin = CCA_ulower, xmax = CCA_uupper)) +
#   geom_point(color = "blue") +
#   geom_errorbarh(height = 0.1, color = "blue") +
#   labs(title = 'Urban', x = 'Relative Risk', y = ' ') +
#   geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(-0.3, max(c(df_raw_clean$CCA_uupper, df_raw_clean$CCA_rupper), na.rm = TRUE))) +
#   theme(axis.text.y = element_text(size = 8))
# 
# p1 <- add_brackets(p1, 7.5, 13.5, "Age")
# p1 <- add_brackets(p1, 0.5, 2.5, "Sex")
# 
# p2 <- ggplot(data = df_raw_clean, aes(y = Variable, x = RRA_rural, xmin = CCA_rlower, xmax = CCA_rupper)) +
#   geom_point(color = "red") +
#   geom_errorbarh(height = 0.1, color = "red") +
#   labs(title = 'Rural', x = 'Relative Risk', y = ' ') +
#   geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
#   theme_minimal() +
#   scale_x_continuous(limits = c(-0.3, max(c(df_raw_clean$CCA_uupper, df_raw_clean$CCA_rupper), na.rm = TRUE))) +
#   theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 8))
# 
# p2 <- add_brackets(p2, 7.5, 13.5, "Age")
# p2 <- add_brackets(p2, 0.5, 2.5, "Sex")
# 
# p1 / p2


#### adjust the x values to show labels correctly

library(patchwork)
library(ggplot2)
library(dplyr)

df_raw_clean <- df_raw_clean %>% 
  mutate(Variable = factor(Variable, levels = c("Male", "Female", 
                                                "Asian/Pacific Islander", "Hispanic", "Non-Hispanic White", "Non-Hispanic Black", "Other",
                                                "0-4", "5-24", "25-44", "45-64", "65-84", "85+")))

add_brackets <- function(p, y_start, y_end, label, x_pos = -0.2) {
  p + 
    annotate("segment", x = x_pos, xend = x_pos, y = y_start, yend = y_end, color = "black") +
    annotate("segment", x = x_pos, xend = x_pos - 0.05, y = y_start, yend = y_start, color = "black") +
    annotate("segment", x = x_pos, xend = x_pos - 0.05, y = y_end, yend = y_end, color = "black") +
    annotate("text", x = x_pos - 0.1, y = (y_start + y_end) / 2, label = label, angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 4)
}

p1 <- ggplot(data = df_raw_clean, aes(y = Variable, x = RRa_urban, xmin = CCA_ulower, xmax = CCA_uupper)) +
  geom_point(color = "blue") +
  geom_errorbarh(height = 0.1, color = "blue") +
  labs(title = 'Urban', x = 'Relative Risk', y = ' ') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(-0.3, max(c(df_raw_clean$CCA_uupper, df_raw_clean$CCA_rupper), na.rm = TRUE))) +
  theme(axis.text.y = element_text(size = 8))

p1 <- add_brackets(p1, 7.5, 13.5, "Age")
p1 <- add_brackets(p1, 3.0, 7.0, "R/E")
p1 <- add_brackets(p1, 0.5, 2.5, "Sex")

# Add text labels manually
p1 <- p1 + 
  annotate("text", x = -0.1, y = 10.5, label = "Age", angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 3) +
  annotate("text", x = -0.1, y = 5.0, label = "R/E", angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 3) +
  annotate("text", x = -0.1, y = 1.5, label = "Sex", angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 3)

p2 <- ggplot(data = df_raw_clean, aes(y = Variable, x = RRA_rural, xmin = CCA_rlower, xmax = CCA_rupper)) +
  geom_point(color = "red") +
  geom_errorbarh(height = 0.1, color = "red") +
  labs(title = 'Rural', x = 'Relative Risk', y = ' ') +
  geom_vline(xintercept = 1, color = 'black', linetype = 'dashed', alpha = 0.5) +
  theme_minimal() +
  scale_x_continuous(limits = c(-0.3, max(c(df_raw_clean$CCA_uupper, df_raw_clean$CCA_rupper), na.rm = TRUE))) +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size = 8))

p2 <- add_brackets(p2, 7.5, 13.5, "Age")
p2 <- add_brackets(p2, 3.0, 7.0, "R/E")
p2 <- add_brackets(p2, 0.5, 2.5, "Sex")

# Add text labels manually
p2 <- p2 + 
  annotate("text", x = -0.1, y = 10.5, label = "Age", angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 3) +
  annotate("text", x = -0.1, y = 5.0, label = "R/E", angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 3) +
  annotate("text", x = -0.1, y = 1.5, label = "Sex", angle = 90, hjust = 0.5, vjust = 0.5, color = "black", size = 3)

# Combine plots
combined_plot <- p1 / p2

# Print the combined plot
print(combined_plot)

#---
