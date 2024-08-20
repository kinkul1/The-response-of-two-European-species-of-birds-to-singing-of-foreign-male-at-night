### Global ----------------------------------------------------------------
library(ggplot2)
library(ggpubr)
library(glmmTMB)
library(visreg)
library(readxl)
library(car)

t_col <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  invisible(t.col)
}
  
### Yellowhammer ----------------------------------------------------------
# data
df_T_f <- read_xlsx("flights.xlsx", sheet = 1)

df_T_f$Type <- as.factor(df_T_f$Type)
df_T_f$Time <- as.factor(df_T_f$Time)
df_T_f$Month <- as.factor(df_T_f$Month)

# raw data summary 
hist(df_T_f$Number)

tab_day <- table(df_T_f[which(df_T_f$Number > 1),]$Type)
sum(tab_day[2])/sum(tab_day)

tab_yh_day <- table(df_T_f[which(df_T_f$Number > 1 & df_T_f$Time == "Day"),]$Type)
sum(tab_yh_day[2])/sum(tab_day)

tab_yh_night <- table(df_T_f[which(df_T_f$Number > 1 & df_T_f$Time == "Night"),]$Type)
sum(tab_yh_night[2])/sum(tab_yh_night)

## flights - models
mx_t0 <- glmmTMB(Probability ~ 1 + (1|Month/ID), 
                 data = df_T_f, family = binomial)

mx_t_p <- glmmTMB(Probability ~ relevel(Type, ref = 2) * relevel(Time, ref = 2) + Month + 
                    (1|Month/ID), 
                  data = df_T_f, family = binomial); summary(mx_t_p)

anova(mx_t0, mx_t_p, test = "Chisq")
Anova(mx_t_p, type = 2)

color_CI <- c(t_col("orange", 80), t_col("darkblue", 70))
v_yh <- visreg(mx_t_p, "Type", legend = FALSE, by = "Time", overlay = TRUE, 
                     ylab = "Probability of flights", 
       xlab = "", gg = T, partial = FALSE, type = "conditional",
       scale = "response") + theme_classic() + ggtitle("Yellowhammer") + 
  annotate("text", x = 0, y = 0.9, label = "a)") + 
  scale_color_manual(values=c("orange", "darkblue")) + 
  scale_fill_manual(values = color_CI) + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 10)); v_yh

### Common chaffinch ------------------------------------------------------
# data 
df_Z_f <- read_xlsx("flights.xlsx", sheet = 2)

df_Z_f$Type <- as.factor(df_Z_f$Type)
df_Z_f$Time <- as.factor(df_Z_f$Time)
df_Z_f$Month <- as.factor(df_Z_f$Month)

# raw data summary
tab_cc <- table(df_Z_f[which(df_Z_f$Number > 1),]$Type)
sum(tab_cc[2])/sum(tab_cc)

tab_cc_day <- table(df_Z_f[which(df_Z_f$Number > 1 & df_Z_f$Time == "Day"),]$Type)
sum(tab_cc_day[2])/sum(tab_cc)

tab_cc_night <- table(df_Z_f[which(df_Z_f$Number > 1 & df_Z_f$Time == "Night"),]$Type)
sum(tab_cc_night[2])/sum(tab_cc_night)

mx_z0 <- glmmTMB(Probability ~ 1 + (1|Month/ID), 
                data = df_Z_f, family = nbinom2)

mx_z_p <- glmmTMB(Probability ~ relevel(Type, ref = 2) * relevel(Time, ref = 2) + 
                    Month + (1|Month/ID),  
                data = df_Z_f, family = binomial); summary(mx_z_p)

anova(mx_z0, mx_z_p, test = "Chisq")
Anova(mx_z_p, type = 3)

v_cc <- visreg(mx_z_p, "Type", by = "Time", 
                  overlay = T, ylab = "Probability of flights", 
                  xlab = "Experiment phase", gg = T, type = "conditional",
       scale = "response") + theme_classic() + ggtitle("Common chaffinch") +
  annotate("text", x = 0.0, y = 0.9, label = "b)") + 
  scale_color_manual(values = c("orange", "darkblue")) + 
  scale_fill_manual(values = color_CI) + 
  theme(plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 10)); v_cc

### Merge plots and save --------------------------------------------------
ar <- ggarrange(v_yh, v_cc, ncol = 1, common.legend = TRUE,
                legend = "right"); ar

tiff("flights_prob.tiff", units = "in", width = 8, height = 6.5, res = 300)
ar
dev.off()