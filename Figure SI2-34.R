# 021625

# DF <- read_dta('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Reg_HP_0421.dta') #replace data path
# # length(unique(DF$BILACCT_K))
# 
# load("EnergyCost.RData")
# 
# HPDF <- DF %>%  left_join(sorted_energy, by = c("BILACCT_K" = "BILACCT_K", "V8" = "YEAR"))
# 
# 
# 
# 
# 
# # using median values calaulate 
# 
# HPDF <- HPDF %>%
#   mutate(Income = case_when(
#     VINCOME == "Less than $15,000" ~ 7500,
#     VINCOME == "$15,000 to $24,999" ~ 20000,
#     VINCOME == "$25,000 to $34,999" ~ 30000,
#     VINCOME == "$35,000 to $49,999" ~ 42500,
#     VINCOME == "$50,000 to $74,999" ~ 62500,
#     VINCOME == "$75,000 to $99,999" ~ 87500,
#     VINCOME == "$100,000 to $149,999" ~ 125000,
#     VINCOME == "$150,000 or more" ~ 300000,
#     TRUE ~ NA_real_  # This handles any cases not specified
#   ))
# 
# HPDF$EB <- HPDF$Cost/ HPDF$Income
# 
# HPDF <- HPDF %>%
#   mutate(EB = round((Cost / Income) * 100, 2))
# 
# 
# 
# HPDF <- HPDF %>%
#   mutate(Income5k = case_when(
#     VINCOME == "Less than $15,000" ~ 5000,
#     VINCOME == "$15,000 to $24,999" ~ 20000,
#     VINCOME == "$25,000 to $34,999" ~ 30000,
#     VINCOME == "$35,000 to $49,999" ~ 42500,
#     VINCOME == "$50,000 to $74,999" ~ 62500,
#     VINCOME == "$75,000 to $99,999" ~ 87500,
#     VINCOME == "$100,000 to $149,999" ~ 125000,
#     VINCOME == "$150,000 or more" ~ 300000,
#     TRUE ~ NA_real_  # This handles any cases not specified
#   ))
# 
# HPDF <- HPDF %>%
#   mutate(EB5k = round((Cost / Income5k) * 100, 2))
# 
# HPDF <- HPDF %>%
#   mutate(Income10k = case_when(
#     VINCOME == "Less than $15,000" ~ 10000,
#     VINCOME == "$15,000 to $24,999" ~ 20000,
#     VINCOME == "$25,000 to $34,999" ~ 30000,
#     VINCOME == "$35,000 to $49,999" ~ 42500,
#     VINCOME == "$50,000 to $74,999" ~ 62500,
#     VINCOME == "$75,000 to $99,999" ~ 87500,
#     VINCOME == "$100,000 to $149,999" ~ 125000,
#     VINCOME == "$150,000 or more" ~ 300000,
#     TRUE ~ NA_real_  # This handles any cases not specified
#   ))
# 
# HPDF <- HPDF %>%
#   mutate(EB10k = round((Cost / Income10k) * 100, 2))

save(HPDF, file = "EnergyBurdenData.Rdata")
load("EnergyBurdenData.Rdata")

HPDF_2019 <- HPDF %>% filter(V8 == 2019)

count_less_15k <- HPDF_2019 %>%
  filter(VINCOME == "Less than $15,000")

df_long <- count_less_15k %>%
  pivot_longer(cols = c(EB, EB5k, EB10k), names_to = "Variable", values_to = "Value")

legend_labels <- c("EB" = "Median Values 7500", 
                   "EB5k" = "Median Values 5000", 
                   "EB10k" = "Median Values 10000")

ggplot(df_long, aes(x = Value, fill = Variable)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity", color = "black") +
  geom_vline(xintercept = c(6, 10), linetype = "dashed", color = "red", size = 0.5) +
  labs(title = "Comparison of Energy Burden (Three Setting of Low-income Group)",
       x = "Energy Burden", y = "Count") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  scale_fill_manual(values = c("EB" = "red", "EB5k" = "blue", "EB10k" = "green"),labels = legend_labels)  

ggsave("0215_EnergyBurden_1.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")




bins <- c(0, 6, 10, Inf)
bin_labels <- c("0-6", "6-10", ">10")

count_table <- df_long %>%
  mutate(Category = cut(Value, breaks = bins, labels = bin_labels, right = TRUE)) %>%
  group_by(Variable, Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = Count, values_fill = 0)  

print(count_table)


energyinsecure  <- subset(df_long, Value >= 6 & Value <= 10)

energypoor  <- subset(df_long, Value > 10 )



CBPEI <- ggplot(energyinsecure, aes(x = factor(Variable), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("EB" = "Median 7500", 
                              "EB5k" = "Median 5000", 
                              "EB10k" = "Median 10000")) +
  theme_minimal() +
  labs(x = "Energy Burden", y = "Cooling Balance Point", title = "(c) Energy Insecure - CBP") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.15),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
CSEI <- ggplot(energyinsecure, aes(x = factor(Variable), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("EB" = "Median 7500", 
                              "EB5k" = "Median 5000", 
                              "EB10k" = "Median 10000")) +
  theme_minimal() +
  labs(x = "Energy Burden", y = "Cooling Slope", title = "(d) Energy Insecure - CS") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))


CBPEP <- ggplot(energypoor, aes(x = factor(Variable), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("EB" = "Median 7500", 
                              "EB5k" = "Median 5000", 
                              "EB10k" = "Median 10000")) +
  theme_minimal() +
  labs(x = "Energy Burden", y = "Cooling Balance Point", title = "(a) Energy Poor - CBP") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.15),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))
CSEP <- ggplot(energypoor, aes(x = factor(Variable), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("EB" = "Median 7500", 
                              "EB5k" = "Median 5000", 
                              "EB10k" = "Median 10000")) +
  theme_minimal() +
  labs(x = "Energy Burden", y = "Cooling Slope", title = "(b) Energy Poor - CS") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(title = "Heat Pump"))

library(gridExtra)
final_plot <- grid.arrange(CBPEP, CSEP, CBPEI, CSEI, ncol = 2, nrow = 2)

ggsave(final_plot, file = "0215_EnergyBurden_2.png", width = 10, height = 10, dpi = 900, units = "in", type = "cairo-png")




