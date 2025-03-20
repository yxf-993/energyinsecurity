#### PreCovid Energy Consumption Distribution ####

# To assess the representativeness of our final sample, we first use pre-COVID energy consumption data for comparison.
# The original pre-COVID smart meter dataset contained 8,688 households, which we refer to as the Full Sample, while the final analysis dataset, consisting of 4,959 households, is referred to as the Subset.

library(haven)

#### 1 Load pre-COVID energy consumption data ####
PRE <- read_dta("/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Data/Pre_Data/Original_Pre.dta") # Import data (we can not share this dataset)

# filter APCODE = 0
PRE_sum_ele <- subset(PRE, APCODE == '0') 

# sum daily electricy
PRE_sum_ele <- PRE_sum_ele %>% mutate(across(8:31, as.numeric))

PRE_sum_ele <- PRE_sum_ele %>%
  mutate(across(8:31, ~replace_na(.x, 0)))

PRE_sum_ele$elesum = rowSums(PRE_sum_ele[, 8:31], na.rm = TRUE)

PRE_sum_ele <- PRE_sum_ele %>% select(LOCATN_K,BILACCT_K,MTR_NB,APCODE,CHNM,DATE,rate,elesum)

# For accounts with only APCODE “0”, there should be no “kWh-Received”
PRE_sum_ele <- subset(PRE_sum_ele, CHNM != "kWh - Received")

# length(unique(PRE_sum_ele$BILACCT_K)) 
# check ID number 8688

#### 2 Load Subset energy consumption data ####
HP <- read_dta("~/Desktop/2022-Technology reduce insecurity/Submit/data/Reg_HP_020225.dta") # Replace file path, use Reg_HP_020225.dta

# length(unique(HP$BILACCT_K)) #8199
HP_1 <- HP %>% drop_na()
HP_clean <- HP_1[!apply(HP_1 == "", 1, any), ]


#### 3 Combine ####
PRE_filtered <- PRE_sum_ele %>% filter(BILACCT_K %in% HP_clean$BILACCT_K)
PRE_filtered$Group <- "Subset"
PRE_sum_ele$Group <- "Full Sample"
combined_df <- bind_rows(PRE_sum_ele, PRE_filtered)

#### Distribution Fig. SI2-1(a) ####
ggplot(combined_df, aes(x = elesum, fill = Group)) +
  geom_histogram(alpha = 0.5, bins = 50, position = "identity") +  
  labs(title = "(a) Daily Energy Consumption Distribution",
       x = "Daily Energy Consumption", y = "Count") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  scale_fill_manual(values = c("blue", "red"))
ggsave("0215_Daily Energy Consumption_1.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")

#### density Fig. SI2-1(b) ####
ggplot(combined_df, aes(x = elesum, fill = Group, color = Group)) +
  geom_density(alpha = 0.2, size = 0.8) +
  labs(title = "(b) Density Comparison of Daily Energy Consumption",
       x = "Daily Energy Consumption", y = "Density") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red"))
ggsave("0215_Daily Energy Consumption_2.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")

#### Summary Statistics  Table SI2-1  ####
summary_table <- combined_df %>%
  group_by(Group) %>%
  summarise(
    count = n(),                        
    mean = mean(elesum, na.rm = TRUE),   
    sd = sd(elesum, na.rm = TRUE),       
    min = min(elesum, na.rm = TRUE),    
    Q1 = quantile(elesum, 0.25, na.rm = TRUE),  
    median = median(elesum, na.rm = TRUE),      
    Q3 = quantile(elesum, 0.75, na.rm = TRUE), 
    max = max(elesum, na.rm = TRUE)      
  )

print(summary_table)

#### daily average consumption overtime  Fig. SI2-1(c)  ####
daily_energy <- combined_df %>%
  group_by(DATE, Group) %>%
  summarise(Avg_Consumption = mean(elesum, na.rm = TRUE))

ggplot(daily_energy, aes(x = as.Date(DATE), y = Avg_Consumption, color = Group)) +
  geom_line(alpha = 0.8,size = 0.8) +  
  labs(title = "(c) Energy Consumption Over Time",
       x = "Date", y = "Daily Consumption (mean)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +  
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  scale_color_manual(values = c("blue", "red"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("0215_Daily Energy Consumption_3.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")
