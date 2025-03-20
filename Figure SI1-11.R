library(dplyr)
library(tidyr)
library(ggplot2)

# Rebust Size result from Stata
# CSP 
df <- data.frame(
  Category = c("Under 1,000", "1,000-1,499", "1,500-1,999", "2,000-2,999", "3,000-3,999", "4,000+"),
  Outcome = "CSP",
  Estimate = c(-1.292, -1.050, -0.738, -1.273, -0.625, -1.076),
  CI = c("[-2.159,-0.424]", "[-1.453,-0.647]", "[-1.084,-0.392]", "[-1.627,-0.919]", "[-1.451,0.202]", "[-2.219,0.066]"),
  N = c(638, 2962, 3902, 3290, 803, 316)
)

# CS
df_CS <- data.frame(
  Category = c("Under 1,000", "1,000-1,499", "1,500-1,999", "2,000-2,999", "3,000-3,999", "4,000+"),
  Outcome = "CS",
  Estimate = c(-0.323, -0.384, -0.105, -0.515, -0.236, -0.370),
  CI = c("[-0.659,0.013]", "[-0.543,-0.225]", "[-0.251,0.041]", "[-0.685,-0.344]", "[-0.669,0.197]", "[-1.370,0.631]"),
  N = c(638, 2962, 3902, 3290, 803, 316)
)

df <- bind_rows(df, df_CS)

df <- df %>%
  separate(CI, into = c("CI_Lower", "CI_Upper"), sep = ",", extra = "drop") %>%
  mutate(CI_Lower = as.numeric(gsub("\\[", "", CI_Lower)),  
         CI_Upper = as.numeric(gsub("]", "", CI_Upper)))  




df$Category <- factor(df$Category, levels = c(
  "Under 1,000", "1,000-1,499", "1,500-1,999", 
  "2,000-2,999", "3,000-3,999", "4,000+"
))

dodge_width <- 0.5  

plotSI <- ggplot(df, aes(x = Estimate, y = Category, color = Outcome)) +
  geom_point(aes(group = Outcome), size = 2.5, shape = 16, position = position_dodge(width = dodge_width)) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 0.5) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper, group = Outcome), 
                 height = 0.2, linewidth = 0.5, position = position_dodge(width = dodge_width)) +  
  geom_text(aes(label = sprintf("%.3f", Estimate)), 
            position = position_dodge(width = dodge_width), 
            vjust = -0.5, size = 4) +  
  scale_color_manual(values = c("CSP" = "#38AAACEE", "CS" = "#C4A080")) + 
  labs(title = "Heat Pump Effects among Different Size Group",
       x = "Estimated Effect",
       y = "Housing Category(sq. ft.)") +
  theme_minimal() +
  theme_bw(base_size = 14, base_family = "Arial") +
  theme(
    legend.position = c(0.1, 0.1),  
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_blank(),  
    legend.text = element_text(size = 9),  
    legend.key = element_blank(),  
    panel.grid.major = element_line(color = "gray95", size = 0.2), 
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(override.aes = list(label = "")))  

ggsave(plotSI, file = "0222_Sizeresult.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")
