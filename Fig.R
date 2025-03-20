library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)

# DF <- read_dta("~/Desktop/2022-Technology reduce insecurity/R&R/IV/IVData0219.dta")
# load("~/Desktop/2022-Technology reduce insecurity/Data/EnergyBurden/EnergyCost.RData")
# 
# EnergyCost <- DF %>% left_join(sorted_energy, by = c("BILACCT_K", "Year" = "YEAR"))
# EnergyCost <- EnergyCost %>% select(BILACCT_K,Cost,Heatpump,Year,VINCOME)
# EnergyCost <- EnergyCost %>% drop_na()

# EnergyCost$VINCOME[which(EnergyCost$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
# EnergyCost$VINCOME[which(EnergyCost$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
# EnergyCost$VINCOME[which(EnergyCost$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
# EnergyCost$VINCOME[which(EnergyCost$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
# EnergyCost$VINCOME[which(EnergyCost$VINCOME == "$150,000 or more")] <- '$150K or more'
# EnergyCost$VINCOME[which(EnergyCost$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
# EnergyCost$VINCOME[which(EnergyCost$VINCOME == "Less than $15,000")] <- 'Less than $15K'
# EnergyCost$VINCOME[which(EnergyCost$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'
# EnergyCost$VINCOME <- factor(EnergyCost$VINCOME,levels = c('Less than $15K',
#                                            '$15K to 25K',
#                                            '$25K to 35K',
#                                            '$35K to 50K',
#                                            '$50K to 75K',
#                                            '$75K to 100K',
#                                            '$100K to 150K',
#                                            '$150K or more'))




###################################################### FIGURE 2 ######################################################
######################################################################################################################
###Energy  spending comparison for households with and without heat pump across  eight income levels in three years###

load('0225EnergyCost.Rdata')


#### Year = 2019 
HP_2019 <- EnergyCost %>% filter(Year == 2019)

ggplot(HP_2019, aes(x = factor(VINCOME), y = Cost, fill = factor(Heatpump))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC"),  
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +  
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -2.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Energy Spending ($)", title = "(a) Year 2019") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_2a.png", width = 12, height = 5, dpi = 900, units = "in", type = "cairo-png")



#### Year = 2020 
HP_2020 <- EnergyCost %>% filter(Year == 2020)

ggplot(HP_2020, aes(x = factor(VINCOME), y = Cost, fill = factor(Heatpump))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC"),  
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +  
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -2.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Energy Spending ($)", title = "(b) Year 2020") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_2b.png", width = 12, height = 5, dpi = 900, units = "in", type = "cairo-png")



#### Year = 2021 
HP_2021 <- EnergyCost %>% filter(Year == 2021)


ggplot(HP_2021, aes(x = factor(VINCOME), y = Cost, fill = factor(Heatpump))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC"),  
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +  
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -2.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Energy Spending ($)", title = "(c) Year 2021") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_2c.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


############################################### FIGURE S3 S4 S5 ######################################################
######################################################################################################################
###Figure S3 Distribution of AC types by income group.
###Figure S4 Distribution of AC types by ethnic.
###Figure S5 Distribution of AC types by home size.


# load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Result0418.RData')
# 
# # filter 1 R2 > 0 and S3 >0
# DF <- subset(IT0418, R2 > 0 & S3 > 0)
# 
# library(readxl)
# Survey2017 <- read_excel('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Basic_Info/2017 RET survey/SRP RET 2017_Excel Data File_Final_8-16-17_Appended for Lucy.xlsx',
#                          sheet = 'Appended')
# HeatPump <- Survey2017 %>% select(BILACCT_K,VACTYPE,VINCOME,VETHNIC,VSQFEET,VRESAGE,VHH_AGECODE)
# 
# DF <- DF %>% left_join(HeatPump, by='BILACCT_K') %>% drop_na() %>% distinct()
# length(unique(DF$BILACCT_K)) # 4690
# 
# DF_0609 <- DF %>% 
#   select(BILACCT_K,VACTYPE,VINCOME,VETHNIC,VSQFEET,VHH_AGECODE) %>% distinct()
# 
# 
# DF_0609$VACTYPE[which(DF_0609$VACTYPE == "AC unit packaged with gas heating (sometimes called a gas pa")] <- 'AC unit packaged with gas heating'
# DF_0609$VACTYPE[which(DF_0609$VACTYPE == "Heat pump (same system heats and cools using electricity onl")] <- 'Heat pump'
# DF_0609$VACTYPE[which(DF_0609$VACTYPE == "Separate AC system that only cools")] <- 'Separate AC system'
# 
# DF_0609$VINCOME[which(DF_0609$VINCOME == "$100,000 to $149,999")] <- '$100K to 150K'
# DF_0609$VINCOME[which(DF_0609$VINCOME == "$35,000 to $49,999")] <- '$35K to 50K'
# DF_0609$VINCOME[which(DF_0609$VINCOME == "$50,000 to $74,999")] <- '$50K to 75K'
# DF_0609$VINCOME[which(DF_0609$VINCOME == "$15,000 to $24,999")] <- '$15K to 25K'
# DF_0609$VINCOME[which(DF_0609$VINCOME == "$150,000 or more")] <- '$150K or more'
# DF_0609$VINCOME[which(DF_0609$VINCOME == "$75,000 to $99,999")] <- '$75K to 100K'
# DF_0609$VINCOME[which(DF_0609$VINCOME == "Less than $15,000")] <- 'Less than $15K'
# DF_0609$VINCOME[which(DF_0609$VINCOME == "$25,000 to $34,999")] <- '$25K to 35K'
# 
# DF_0609$VACTYPE <- factor(DF_0609$VACTYPE,levels = c("Heat pump",
#                                                      "AC unit packaged with gas heating",
#                                                      "Separate AC system",
#                                                      "Don't know"))
# DF_0609$VINCOME <- factor(DF_0609$VINCOME,levels = c('Less than $15K',
#                                                      '$15K to 25K',
#                                                      '$25K to 35K',
#                                                      '$35K to 50K',
#                                                      '$50K to 75K',
#                                                      '$75K to 100K',
#                                                      '$100K to 150K',
#                                                      '$150K or more'))
# 
# save(DF_0609, file = "Distribution of AC types.Rdata")

load('Distribution of AC types.Rdata')

library(viridis)
library(scales)

#### 1. AC_Types_by_Income_Group (Figure S3)

DF_0609 %>% group_by(VACTYPE,VINCOME) %>% tally() %>% 
  ggplot(aes(x = VINCOME, y = n, fill = VACTYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Income Group", y = "Frequency", fill = "AC Type") +
  ggtitle("Distribution of AC Types by Income Group") +
  scale_fill_manual(breaks = DF_0609$VACTYPE,values = viridis_pal(option = "G", alpha = 0.8)(5))+
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Figure_S3.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


#### 2. AC_Types_by_Ethnic (Figure S4)

DF_0609 <- DF_0609 %>%  
  mutate(Ethnic = if_else(VETHNIC == "White/Caucasian", "White/Caucasian", "Others"))


DF_0609 %>% group_by(VACTYPE,Ethnic) %>% tally() %>% 
  ggplot(aes(x = Ethnic, y = n, fill = VACTYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Ethnic", y = "Frequency", fill = "AC Type") +
  ggtitle("Distribution of AC Types by Ethnic Group") +
  scale_fill_manual(breaks = DF_0609$VACTYPE,values = viridis_pal(option = "G", alpha = 0.8)(5))+
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Figure_S4.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


#### 3. AC_Types_by_Size (Figure S5)

DF_0609$VSQFEET <- factor(DF_0609$VSQFEET,levels = c('Under 1,000 sq. ft.',
                                                     '1,000 - 1,499 sq. ft.',
                                                     '1,500 - 1,999 sq. ft.',
                                                     '2,000 - 2,999 sq. ft.',
                                                     '3,000 - 3,999 sq. ft.',
                                                     '4,000 or more sq.'))

DF_0609 %>% group_by(VACTYPE,VSQFEET) %>% tally() %>% 
  ggplot(aes(x = VSQFEET, y = n, fill = VACTYPE)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Home Size", y = "Frequency", fill = "AC Type") +
  ggtitle("Distribution of AC Types by Home Size") +
  scale_fill_manual(breaks = DF_0609$VACTYPE,values = viridis_pal(option = "G", alpha = 0.8)(5))+
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.84, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Figure_S5.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")




#################################################### FIGURE 5 IV #################################################### 
# df <- data.frame(
#   Variable = rep(c("A", "B", "C", "D"), each = 2),
#   Outcome = rep(c("CBP", "CS"), times = 4),
#   Estimate = c(-0.996, -0.476, -7.922, -3.186, -6.126, -2.266, -9.087, -0.154),
#   t_value = c(-10.02, -8.68, -4.05, -3.93, -3.67, -3.22, -3.77, -0.18),
#   confDown = c(-1.190469, -0.5837152,-11.75405,-4.773064,-9.395891,-3.645485,-13.81194,-1.821055),
#   confup = c(-0.8008864,-0.36857,-4.089398, -1.598737,-2.856333,-0.8859848,-4.361735,1.51381)
# )

df1 <- data.frame(
  Variable = rep(c("A", "B", "C"), each = 2),
  Outcome = rep(c("CBP", "CS"), times = 3),
  Estimate = c(-7.922, -3.186, -6.126, -2.266, -9.087, -0.154),
  t_value = c(-4.05, -3.93, -3.67, -3.22, -3.77, -0.18),
  confDown = c(-11.75405,-4.773064,-9.395891,-3.645485,-13.81194,-1.821055),
  confup = c(-4.089398, -1.598737,-2.856333,-0.8859848,-4.361735,1.51381)
)

dodge_width <- 0.5  

ggplot(df1, aes(x = Estimate, y = Variable, color = Outcome)) +
  geom_point(aes(group = Outcome), size = 2.5, shape = 16, position = position_dodge(width = dodge_width)) +  
  geom_errorbarh(aes(xmin = confDown, xmax = confup, group = Outcome), 
                 height = 0.2, linewidth = 0.5, position = position_dodge(width = dodge_width)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Estimate)), 
            position = position_dodge(width = dodge_width), 
            vjust = -0.5, size = 4) +  
  scale_color_manual(values = c("CBP" = "#38AAACEE", "CS" = "#C4A080")) + 
  labs(title = "Heat Pump Effects (IV)",
       x = "Heat Pump Estimated Effect",
       y = "Models") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_blank(),  
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray92", size = 0.1),
    panel.grid.minor = element_line(color = "gray93", size = 0.1)) +
  guides(color = guide_legend(override.aes = list(label = "")))  

ggsave("Fig_5.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")




#################################################### FIGURE 6 & 7 ####################################################
# 加载 dplyr 进行数据整理
library(dplyr)
library(tidyr)


regression_results <- data.frame(
  Model = rep(1:8, 2),  
  Outcome = c(rep("CBP", 8), rep("CS", 8)),  
  Heatpump_Estimate = c(-0.764, -1.012, -1.270, -0.950, -0.782, -0.876, -1.317, -0.834,
                        -0.598, -0.605, -0.473, -0.240, -0.506, -0.331, -0.339, -0.475),
  Std_Error = c(0.536, 0.376, 0.365, 0.267, 0.220, 0.242, 0.221, 0.300,
                0.261, 0.180, 0.185, 0.140, 0.107, 0.117, 0.122, 0.173),
  CI_Lower = c(-1.814, -1.749, -1.986, -1.473, -1.214, -1.350, -1.751, -1.421,
               -1.110, -0.958, -0.836, -0.514, -0.715, -0.561, -0.578, -0.814),
  CI_Upper = c(0.286, -0.275, -0.554, -0.428, -0.350, -0.403, -0.884, -0.246,
               -0.086, -0.251, -0.109, 0.034, -0.296, -0.102, -0.099, -0.136)
)


df <- regression_results %>%
  mutate(Model = recode(Model,
                               "1" = "Less than $15K",
                               "2" = "$15K - $24K",
                               "3" = "$25K - $34K",
                               "4" = "$35K - $49K",
                               "5" = "$50K - $74K",
                               "6" = "$75K - $99K",
                               "7" = "$100K - $149K",
                               "8" = "$150K or more"
  ))

df$Model <- factor(df$Model,levels = c('Less than $15K',
                                                     '$15K - $24K',
                                                     '$25K - $34K',
                                                     '$35K - $49K',
                                                     '$50K - $74K',
                                       "$75K - $99K",
                                                     '$100K - $149K',
                                       "$150K or more"))

dodge_width <- 0.5  

ggplot(df, aes(x = Heatpump_Estimate, y = Model, color = Outcome)) +
  geom_point(aes(group = Outcome), size = 2.5, shape = 16, position = position_dodge(width = dodge_width)) +  
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, group = Outcome), 
                 height = 0.2, linewidth = 0.5, position = position_dodge(width = dodge_width)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Heatpump_Estimate)), 
            position = position_dodge(width = dodge_width), 
            vjust = -0.5, size = 4) +  
  scale_color_manual(values = c("CBP" = "#38AAACEE", "CS" = "#C4A080")) + 
  labs(title = "Heat Pump Effects (Different Income Levels)",
       x = "Heat Pump Estimated Effect",
       y = "Income Levels") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1)) +
  guides(color = guide_legend(override.aes = list(label = "")))  

ggsave("Fig_6.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")



regression_results <- data.frame(
  Model = c(1, 2, 3, 4),
  Outcome = c("CBP", "CBP", "CS", "CS"),
  Heatpump_Estimate = c(-1.047, -0.862, -0.477, -0.485),
  Std_Error = c(0.113, 0.205, 0.063, 0.112),
  CI_Lower = c(-1.269, -1.263, -0.600, -0.705),
  CI_Upper = c(-0.825, -0.461, -0.354, -0.265)
)

df <- regression_results %>%
  mutate(Model = recode(Model,
                        "1" = "White/Caucasian",
                        "2" = "Others",
                        "3" = "White/Caucasian",
                        "4" = "Others"))

ggplot(df, aes(x = Heatpump_Estimate, y = Model, color = Outcome)) +
  geom_point(aes(group = Outcome), size = 2.5, shape = 16, position = position_dodge(width = dodge_width)) +  
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, group = Outcome), 
                 height = 0.2, linewidth = 0.5, position = position_dodge(width = dodge_width)) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Heatpump_Estimate)), 
            position = position_dodge(width = dodge_width), 
            vjust = -0.5, size = 4) +  
  scale_color_manual(values = c("CBP" = "#38AAACEE", "CS" = "#C4A080")) + 
  labs(title = "Heat Pump Effects (Ethnic)",
       x = "Heat Pump Estimated Effect",
       y = "Ethnic") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1)) +
  guides(color = guide_legend(override.aes = list(label = "")))  

ggsave("Fig_7.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")


#################################################### FIGURE S6 ######################################################
#### Income Distribution Figure SI 6
# To assess the representatives of our final sample, we then plot income distribution

# HP <- read_dta("Reg_HP_020225.dta") # Replace file path, use Reg_HP_020225.dta
# 
# # length(unique(HP$BILACCT_K)) #8199
# HP_1 <- HP %>% drop_na()
# HP_clean <- HP_1[!apply(HP_1 == "", 1, any), ]
# 
# Subset <- HP_clean %>% select(BILACCT_K,VINCOME) %>% distinct()
# 
# income_summary <- Subset %>%
#   group_by(VINCOME) %>%
#   summarise(Count = n()) %>%
#   mutate(Percentage = round(Count / sum(Count) * 100, 2)) %>%
#   arrange(desc(Count))


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
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Income Distribution Comparison",
       x = "Household Income Bucket", y = "Percentage (%)") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1)) +
  scale_fill_viridis_d(option = "viridis")  

ggsave("FigureS6.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")


#################################################### FIGURE S19 ######################################################
###a statistical summary for thermal comfort indexes after modeling the relationship###

# load('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/040724/Result_Data/Result0418.RData')
# # # filter 1 R2 > 0 and S3 >0
# DF <- subset(IT0418, R2 > 0 & S3 > 0)
# save(DF, file = "Thermal comfort indexes.Rdata")

load("Thermal comfort indexes.Rdata")


ggplot(DF, aes(x = R2)) +
  geom_histogram(binwidth = 0.05, fill = "#38AAACCC", color = "white", alpha = 0.8) +
  labs(title = "Distribution of R²",
       x = "Adjusted R²",
       y = "Count") +
  theme_minimal()+
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.1, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_S19.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


#################################################### FIGURE 2 Summer & Winter ######################################################

setwd("~/Desktop/2022-Technology reduce insecurity/R&R/figure")
load('Summer_Cost.Rdata')

HP_2020 <- EnergyCost %>% filter(Year == 2020)

ggplot(HP_2020, aes(x = factor(VINCOME), y = Cost, fill = factor(Heatpump))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#5A5AC8", "0" = "#FCFFA4FF"),  
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +  
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.0f", ..y..)), 
               vjust = -2.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Energy Spending ($)", title = "(a) Year 2020 Summer ") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.15, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_2a_Summer.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HP_2021 <- EnergyCost %>% filter(Year == 2021)
ggplot(HP_2021, aes(x = factor(VINCOME), y = Cost, fill = factor(Heatpump))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#5A5AC8", "0" = "#FCFFA4FF"),  
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +  
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.0f", ..y..)), 
               vjust = -2.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Energy Spending ($)", title = "(b) Year 2021 Summer") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.15, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_2b_Summer.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


load('Winter_Cost.Rdata')
HP_2020 <- EnergyCost %>% filter(Year == 2020)
ggplot(HP_2020, aes(x = factor(VINCOME), y = Cost, fill = factor(Heatpump))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#A52C60FF", "0" = "#FCFFA4FF"),  
                    labels = c("0" = "No Heat Pump (Mainly Gas)", "1" = "Heat Pump")) +  
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.0f", ..y..)), 
               vjust = -2.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Electric Spending ($)", title = "(c) Year 2020 Winter ") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_2c_Winter.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")


HP_2021 <- EnergyCost %>% filter(Year == 2021)
ggplot(HP_2021, aes(x = factor(VINCOME), y = Cost, fill = factor(Heatpump))) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("1" = "#A52C60FF", "0" = "#FCFFA4FF"),  
                    labels = c("0" = "No Heat Pump (Mainly Gas)", "1" = "Heat Pump")) +  
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.0f", ..y..)), 
               vjust = -2.5, color = "black", position = position_dodge(width = 0.8)) +
  theme_minimal() +
  labs(x = "Income Group", y = "Electric Spending ($)", title = "(d) Year 2021 Winter") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_2d_Winter.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



#################################################### FIGURE S9 #####################################################
library(viridis)
load("EnergyBurdenData.Rdata")

HPDF_2019 <- HPDF %>% filter(V8 == 2019)


count_less_15k <- HPDF_2019 %>%
  filter(VINCOME == "Less than $15,000")

df_long <- count_less_15k %>%
  pivot_longer(cols = c(EB, EB5k, EB10k), names_to = "Variable", values_to = "Value")

legend_labels <- c("EB5k" = "Median Values 5000",
                   "EB" = "Median Values 7500",
                   "EB10k" = "Median Values 10000")

ggplot(df_long, aes(x = Value, fill = Variable)) +
  geom_histogram(binwidth = 1, alpha = 0.8, position = "identity", color = "black") +
  geom_vline(xintercept = c(6, 10), linetype = "dashed", color = "red", size = 0.5) +
  labs(title = "Comparison of Energy Burden (Three Setting of Low-income Group)",
       x = "Energy Burden", y = "Count") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  scale_fill_viridis_d(option = "C", labels = legend_labels, breaks = c("EB5k", "EB", "EB10k"))+
  theme(
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_S9.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")




bins <- c(0, 6, 10, Inf)
bin_labels <- c("0-6", "6-10", ">10")

count_table <- df_long %>%
  mutate(Category = cut(Value, breaks = bins, labels = bin_labels, right = TRUE)) %>%
  group_by(Variable, Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Variable, values_from = Count, values_fill = 0)  

energyinsecure  <- subset(df_long, Value >= 6 & Value <= 10)

energypoor  <- subset(df_long, Value > 10 )




CBPEI <- ggplot(energyinsecure, aes(x = factor(Variable, levels = c("EB5k", "EB", "EB10k")), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC"),
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("EB5k" = "5,000",
                              "EB" = "7,500", 
                              "EB10k" = "10,000")) +
  theme_minimal() +
  labs(x = "Energy Burden", y = "Cooling Balance Point", title = "(c) Energy Insecure - CBP") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.position = "top",  
        legend.direction = "horizontal",  
    legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))+
  guides(fill = guide_legend(title = NULL))

CSEI <- ggplot(energyinsecure, aes(x = factor(Variable, levels = c("EB5k", "EB", "EB10k")), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC"),
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("EB5k" = "5,000",
                              "EB" = "7,500", 
                              "EB10k" = "10,000")) +
  theme_minimal() +
  labs(x = "Energy Burden", y = "Cooling Slope", title = "(d) Energy Insecure - CS") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.position = "top",  
        legend.direction = "horizontal",  
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))+
  guides(fill = guide_legend(title = NULL))

CBPEP <- ggplot(energypoor, aes(x = factor(Variable, levels = c("EB5k", "EB", "EB10k")), y = CBP, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC"),
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("EB5k" = "5,000",
                              "EB" = "7,500", 
                              "EB10k" = "10,000")) +
  theme_minimal() +
  labs(x = "Energy Burden", y = "Cooling Balance Point", title = "(a) Energy Poor - CBP") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.position = "top",  
        legend.direction = "horizontal",  
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))+
  guides(fill = guide_legend(title = NULL))

CSEP <- ggplot(energypoor, aes(x = factor(Variable, levels = c("EB5k", "EB", "EB10k")), y = S3, fill = factor(HP))) +
  geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("1" = "#38AAACCC", "0" = "#F4ECD8CC"),
                    labels = c("0" = "No Heat Pump", "1" = "Heat Pump")) +
  stat_summary(fun = median, geom = "text", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, color = "black", position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels = c("EB5k" = "5,000",
                              "EB" = "7,500", 
                              "EB10k" = "10,000")) +
  theme_minimal() +
  labs(x = "Energy Burden", y = "Cooling Slope", title = "(b) Energy Poor - CS") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.position = "top",  
        legend.direction = "horizontal",  
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))+
  guides(fill = guide_legend(title = NULL))

library(gridExtra)
final_plot <- grid.arrange(CBPEP, CSEP, CBPEI, CSEI, ncol = 2, nrow = 2)

ggsave(final_plot, file = "Fig_13.png", width = 10, height = 10, dpi = 900, units = "in", type = "cairo-png")





###################################### FIGURE S7 Summer & Winter #####################################################
######################################################################################################################
########## PreCovid Energy Consumption Distribution ################

# To assess the representativeness of our final sample, we first use pre-COVID energy consumption data for comparison.
# The original pre-COVID smart meter dataset contained 8,688 households, which we refer to as the Full Sample, 
# while the final analysis dataset, consisting of 4,690 households, is referred to as the Subset.

# library(haven)
# 
# #### 1 Load pre-COVID energy consumption data ####
# PRE <- read_dta("/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/Data/Pre_Data/Original_Pre.dta") # Import data (we can not share this dataset)
# 
# # filter APCODE = 0
# PRE_sum_ele <- subset(PRE, APCODE == '0') 
# 
# # sum daily electricy
# PRE_sum_ele <- PRE_sum_ele %>% mutate(across(8:31, as.numeric))
# 
# PRE_sum_ele <- PRE_sum_ele %>%
#   mutate(across(8:31, ~replace_na(.x, 0)))
# 
# PRE_sum_ele$elesum = rowSums(PRE_sum_ele[, 8:31], na.rm = TRUE)
# 
# PRE_sum_ele <- PRE_sum_ele %>% select(LOCATN_K,BILACCT_K,MTR_NB,APCODE,CHNM,DATE,rate,elesum)
# 
# # For accounts with only APCODE “0”, there should be no “kWh-Received”
# PRE_sum_ele <- subset(PRE_sum_ele, CHNM != "kWh - Received")
# 
# # length(unique(PRE_sum_ele$BILACCT_K)) 
# # check ID number 8688
# 
# #### 2 Load Subset energy consumption data ####
# HP <- read_dta("~/Desktop/2022-Technology reduce insecurity/Submit/data/Reg_HP_020225.dta") 
# 
# # length(unique(HP$BILACCT_K)) #8199
# HP_1 <- HP %>% drop_na()
# HP_clean <- HP_1[!apply(HP_1 == "", 1, any), ]
# 
# 
# #### 3 Combine ####
# PRE_filtered <- PRE_sum_ele %>% filter(BILACCT_K %in% HP_clean$BILACCT_K)
# PRE_filtered$Group <- "Subset"
# PRE_sum_ele$Group <- "Full Sample"
# combined_df <- bind_rows(PRE_sum_ele, PRE_filtered)

#### Distribution Fig. SI2-1(a) ####
# ggplot(combined_df, aes(x = elesum, fill = Group)) +
#   geom_histogram(alpha = 0.5, bins = 50, position = "identity") +  
#   labs(title = "(a) Daily Energy Consumption Distribution",
#        x = "Daily Energy Consumption", y = "Count") +
#   theme_minimal() +
#   theme_bw(base_size = 14, base_family = "Arial") +
#   scale_fill_manual(values = c("blue", "red"))
# ggsave("0215_Daily Energy Consumption_1.png", width = 10, height = 6, dpi = 900, units = "in", type = "cairo-png")

#### density Fig. s7(a) 

ggplot(combined_df, aes(x = elesum, fill = Group, color = Group)) +
  geom_density(alpha = 0.2, size = 0.8) +
  labs(title = "(a) Density Comparison of Daily Energy Consumption",
       x = "Daily Energy Consumption (kWh)", y = "Density") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(limits = c(0, 400))  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))

ggsave("Fig_S7a.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

#### Summary Statistics  Table SI2-1  
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

t_test_result <- t.test(elesum ~ Group, data = combined_df, var.equal = TRUE)
print(t_test_result)


#### daily average consumption overtime  Fig. s7(b)
daily_energy <- combined_df %>%
  group_by(DATE, Group) %>%
  summarise(Avg_Consumption = mean(elesum, na.rm = TRUE))

ggplot(daily_energy, aes(x = as.Date(DATE), y = Avg_Consumption, color = Group)) +
  geom_line(alpha = 0.8,size = 0.8) +  
  labs(title = "(b) Energy Consumption Over Time",
       x = "Date", y = "Daily Consumption (mean)") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 month") +  
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  scale_color_manual(values = c("blue", "red"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.8),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray92", size = 0.1),
        panel.grid.minor = element_line(color = "gray93", size = 0.1))


ggsave("Fig_S7b.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



#################################################### FIGURE S1/S2/S16 Map ###############################################

### 1. IV Map

load("~/Desktop/2022-Technology reduce insecurity/R&R/IV/IV_Location.Rdata")
load("~/Desktop/2022-Technology reduce insecurity/R&R/IV/ResearchArea.Rdata")

library(sf)
# Convert geocoded data to sf object
HP_sf <- st_as_sf(HPaddress, coords = c("longitude", "latitude"), crs = 4326)

S_ALL_clean <- S_ALL %>%
  filter(!is.na(longitude) & !is.na(latitude))

AZind_sf <- st_as_sf(S_ALL_clean, coords = c("longitude", "latitude"), crs = 4326)


ggplot() +
  geom_sf(data = ResearchArea, fill = "#C0632D", color = "black", alpha = 0.8) +  
  geom_sf(data = HP_sf, color =  '#5A5AC8', size = 0.5) +  
  # geom_sf(data = stations, color = "red") +  
  theme_minimal() + 
  ggtitle ("Research Area and HVAC Contractors") +  
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.position = "none",
        # plot.title = element_text(hjust = 0.5, size = 16, family = "Times New Roman", face = "bold"),  # Center title and adjust font
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray90", size = 0.1),
        panel.grid.minor = element_line(color = "gray90", size = 0.1)) 


ggsave("Fig_S16.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")




### 2. Map AZ and Research Area

AZ <- st_read('/Users/yexiaofeng/Desktop/2022-Technology reduce insecurity/tl_2016_04_cousub/tl_2016_04_cousub.shp')

st_crs(AZ)    
# Transfer 
AZ <- st_transform(AZ, 4326)

ggplot() +
  # Plot AZ with blue fill and black borders
  geom_sf(data = AZ, fill = "#edeae8", color = "gray", alpha = 0.4, size = 0.7) +
  # Plot ResearchArea with the same fill as AZ but different border
  geom_sf(data = ResearchArea, fill = "#C0632D", color = "black",  size = 0.7) +
  theme_minimal() +
  ggtitle("Arizona and Research Area") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray90", size = 0.1),
        panel.grid.minor = element_line(color = "gray90", size = 0.1)) +
  coord_sf()  # Ensure the plot uses the spatial data's coordinate system

ggsave("Fig_S1.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")

### 3. Map Research Area and Wearther Stattion

library(tigris)
library(sf)
library(tidycensus)


# Ensure the GSODR package is loaded for historical weather data
library(GSODR)
# Load historical weather station data included with GSODR
load(system.file("extdata", "isd_history.rda", package = "GSODR"))
# Transforming the historical weather data into an 'sf' object with WGS84 coordinates
isd_history_az_sf <- isd_history %>%
  as_tibble() %>% 
  filter(CTRY == "US" & STATE == "AZ") %>%  # Filtering the spatial dataframe for records from Pennsylvania, US
  st_as_sf(coords = c("LON", "LAT"), crs = 4326, remove = FALSE)

# Filter stations available within a specific date range
stations <- filter(isd_history_az_sf, BEGIN <= 20190101 & END >= 20221231)


# Calculate centroids of polygons (counties)
az_co_centroids <- st_centroid(ResearchArea)

ggplot() +
  geom_sf(data = ResearchArea, fill = "#edeae8", color = "gray", alpha = 0.4, size = 0.7) +
  # Adding az_co_centroids with green color
  geom_sf(data = az_co_centroids, color = "#C0632D", color = "black",  size = 0.7) +
  # Adding stations with red color
  geom_sf(data = stations, shape = 2, color = "#5A5AC8", size = 1.2) +
  theme_minimal() +
  ggtitle("Research Area, Centroids, and Stations") +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(legend.position = "none",
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_text(size = 11),  
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = "gray90", size = 0.1),
        panel.grid.minor = element_line(color = "gray90", size = 0.1)) +
  coord_sf(xlim = c(-113, -110.5), ylim = c(32, 34.5), expand = FALSE)   # Ensuring that the plot uses the spatial data's coordinate system


ggsave("Fig_S2.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")



