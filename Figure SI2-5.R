load("~/Desktop/2022-Technology reduce insecurity/040724/Data/Pre_data_withtemp.RData")

library(dplyr)


Sample <- Pre1 %>% filter(BILACCT_K == "441005")


Sample <- na.omit(Sample) 
Sample$rate <- as.numeric(Sample$rate)

library(segmented)


lm <- lm(elesum ~ TEMP + holiday + Sunday + Monday + Tuesday + Wednesday + Thursday + Friday
                    , data = Sample)

seg <- segmented(lm,seg.Z = ~ TEMP, psi = c(10,25))  

summary(seg)



# Sample$Fitted <- fitted(seg)
# 
# breakpoints <- seg$psi[, "Est."]
# print(breakpoints)  
# ggplot(Sample, aes(x = TEMP, y = elesum)) +
#   geom_point(alpha = 0.5, color = "gray") +  
#   geom_line(aes(y = Fitted), color = "blue", size = 1.2) +  
#   geom_vline(xintercept = breakpoints, linetype = "dashed", color = "red", size = 1) + 
#   labs(title = "Segmented Regression Fit",
#        x = "Temperature (TEMP)",
#        y = "Electricity Consumption (elesum)") +
#   theme_minimal(base_size = 14)
# 
# 
# ggplot(Sample, aes(x = TEMP, y = elesum)) +
#   geom_point(alpha = 0.5, color = "gray") +  
#   geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue", size = 1.2) +  
#   geom_vline(xintercept = seg$psi[, "Est."], linetype = "dashed", color = "red", size = 1) +  
#   labs(title = "Segmented Regression Fit",
#        x = "Temperature (TEMP)",
#        y = "Electricity Consumption (elesum)") +
#   theme_minimal(base_size = 14)

# Create a Predict Data
TEMP_seq <- seq(min(Sample$TEMP), max(Sample$TEMP), length.out = 300)  # 生成300个等间距点
new_data <- data.frame(TEMP = TEMP_seq, holiday = 0, Sunday = 0, Monday = 0, 
                       Tuesday = 0, Wednesday = 0, Thursday = 0, Friday = 0)

new_data$Fitted <- predict(seg, newdata = new_data)

R2_value <- 0.8744  

final_plot <- ggplot(Sample, aes(x = TEMP, y = elesum)) +
    geom_point(alpha = 0.5, color = "gray") +  
    geom_line(data = new_data, aes(x = TEMP, y = Fitted), color = "#38AAAC", size = 1.2) +  
    geom_vline(xintercept = breakpoints, linetype = "dashed", color = "red", size = 0.8) +  
    geom_text(data = data.frame(x = breakpoints, y = max(Sample$elesum) * 0.9),  
              aes(x = x, y = y, label = sprintf("%.2f", x)), 
              color = "black", size = 5, vjust = -0.5, nudge_x = 2) +  
    annotate("text", x = min(Sample$TEMP) + 2, y = max(Sample$elesum), 
             label = sprintf("R² = %.4f", R2_value), 
             hjust = 0, size = 5) +  
    labs(title = "Piecewise Linear Regression Plot",
         subtitle = "ID = 441005, Year = 2019",  
         x = "Temperature (°C)",
         y = "Electricity Consumption (kWh)") +
    theme_minimal(base_size = 14) + 
    theme_bw(base_size = 14, base_family = "Arial") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 9))


ggsave(final_plot, file = "0222_Sample.png", width = 8, height = 6, dpi = 900, units = "in", type = "cairo-png")
