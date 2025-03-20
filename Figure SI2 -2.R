#### Income Distribution Figure SI2 -2 ####

# To assess the representativeness of our final sample, we then plot income distribution

HP <- read_dta("~/Desktop/2022-Technology reduce insecurity/Submit/data/Reg_HP_020225.dta") # Replace file path, use Reg_HP_020225.dta

# length(unique(HP$BILACCT_K)) #8199
HP_1 <- HP %>% drop_na()
HP_clean <- HP_1[!apply(HP_1 == "", 1, any), ]

Subset <- HP_clean %>% select(BILACCT_K,VINCOME) %>% distinct()

income_summary <- Subset %>%
  group_by(VINCOME) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
  arrange(desc(Count))


income_data <- data.frame(
  Income_Bucket = c(
    "Less than $15,000", "$15,000 to $24,999", "$25,000 to $34,999", "$35,000 to $49,999",
    "$50,000 to $74,999", "$75,000 to $99,999", "$100,000 to $149,999", "$150,000 or more"
  ),
  Full_Sample_Freq = c(326, 546, 523, 955, 1388, 1018, 1033, 690),
  Full_Sample_Percent = c(5.03, 8.43, 8.07, 14.74, 21.42, 15.71, 15.94, 10.65),
  Subset_Freq = c(161, 306, 333, 650, 1019, 789, 866, 566),
  Subset_Percent = c(3.43, 6.52, 7.10, 13.86, 21.73, 16.82, 18.64, 12.07),
  Arizona_Percent = c(12.97, 10.27, 10.29, 14.17, 18.64, 12.24, 12.90, 9.53)
)


income_long <- income_data %>%
  pivot_longer(cols = c("Full_Sample_Percent", "Subset_Percent", "Arizona_Percent"),
               names_to = "Group", values_to = "Percentage") %>%
  mutate(Group = recode(Group, 
                        "Full_Sample_Percent" = "Full Sample", 
                        "Subset_Percent" = "Subset", 
                        "Arizona_Percent" = "Arizona"))

income_long$Income_Bucket <- factor(income_long$Income_Bucket, levels = c(
  "Less than $15,000",
  "$15,000 to $24,999",
  "$25,000 to $34,999",
  "$35,000 to $49,999",
  "$50,000 to $74,999",
  "$75,000 to $99,999",
  "$100,000 to $149,999",
  "$150,000 or more"
))


library(viridis)

ggplot(income_long, aes(x = Income_Bucket, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Income Distribution Comparison",
       x = "Household Income Bucket", y = "Percentage (%)") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d(option = "viridis")  

ggsave("0215_Income Distribution_1.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")
