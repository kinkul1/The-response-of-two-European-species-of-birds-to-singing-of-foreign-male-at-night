### Global ----------------------------------------------------------------
library(ggplot2)
library(glmmTMB)
library(visreg)
library(readxl)
library(ggpubr)
library(car)

SE <- function(x){
  y <- na.omit(x)
  sd(y)/sqrt(length(y))
}

### Yellowhammer ----------------------------------------------------------
# data
df_T <- read_xlsx("audio.xlsx", sheet = 1)

df_T$Time <- as.factor(df_T$Time)
df_T$Type <- as.factor(df_T$Type)
df_T$Month <- as.factor(df_T$Month)

# raw data summary 
aggregate(Song ~ Month + Type, FUN = mean, data = df_T)
aggregate(Call ~ Month + Type, FUN = mean, data = df_T)

aggregate(Song ~ Month + Type, FUN = SE, data = df_T)
aggregate(Call ~ Month + Type, FUN = SE, data = df_T)


## Songs - models
T_Song1.0 <- glmmTMB(Song ~ 1 + (1|Month/ID),
                     data = df_T, family = "nbinom2"); summary(T_Song1.0)

T_Song1.1 <- glmmTMB(Song ~ relevel(Type, ref = 2) + Month + (1|Month/ID), 
                     data = df_T, family = "nbinom2"); summary(T_Song1.1)

T_Song1.2 <- glmmTMB(Song ~ relevel(Type, ref = 2) * Month + (1|Month/ID), 
                      data = df_T, family = "nbinom2"); summary(T_Song1.2)

anova(T_Song1.0, T_Song1.1, T_Song1.2, test = "Chisq")
Anova(T_Song1.1, type = 2)

Ysongs <- visreg(T_Song1.1, "Type", by = "Month", type = "conditional", scale = "response", 
                 rug = 3, overlay = TRUE, main = "Yellowhammer", xlab = "Experiment phase", 
                 ylab = "Diurnal songs", gg = T) + theme_classic() +
  ggtitle("Yellowhammer") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12)); Ysongs

## Calls - models
T_Call1.0 <- glmmTMB(Call ~ 1 + (1|Month/ID),
                     data = df_T, family = "nbinom2"); summary(T_Call1.0)

T_Call1.1 <- glmmTMB(Call ~ relevel(Type, ref = 2) + Month + (1|Month/ID),
                     data = df_T, family = nbinom2); summary(T_Call1.1)

T_Call1.2 <- glmmTMB(Call ~ relevel(Type, ref = 2) * Month + (1|Month/ID), 
                     data = df_T, family = nbinom2); summary(T_Call1.2)

anova(T_Call1.0, T_Call1.1, T_Call1.2,  test = "Chisq")
Anova(T_Call1.2, type = 2)

Ycalls <- visreg(T_Call1.2, "Type" , by = "Month" ,type = "conditional", 
                 scale = "response", main = "Yellowhammer", overlay = TRUE, rug = FALSE, 
                xlab = "Experiment phase", ylab = "Diurnal calls", gg = TRUE) + 
  theme_classic() + ggtitle("Yellowhammer") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12)); Ycalls
Ycalls[["layers"]][[1]][["data"]][["visregGGY"]][c(7,8)] <- 0; Ycalls

### Common chaffinch ------------------------------------------------------
# data 
df_Z <- read_xlsx("audio.xlsx", sheet = 2)

df_Z$Type <- as.factor(df_Z$Type)

# raw data summary 
aggregate(Song ~ Month + Type, FUN = mean, data = df_Z)
aggregate(Call ~ Month + Type, FUN = mean, data = df_Z)

aggregate(Song ~ Month + Type, FUN = SE, data = df_Z)
aggregate(Call ~ Month + Type, FUN = SE, data = df_Z)

## Songs - models
Z_Song2.0 <- glmmTMB(Song ~ 1 + (1|Month/ID),
                     data = df_Z, family = "nbinom2"); summary(Z_Song2.0)

Z_Song2.1 <- glmmTMB(Song ~ relevel(Type, ref = 2) + Month + (1|Month/ID),
                     data = df_Z, family = "nbinom2"); summary(Z_Song2.1)

Z_Song2.2 <- glmmTMB(Song ~ relevel(Type, ref = 2) * Month + (1|Month/ID),
                    data = df_Z, family = "nbinom2"); summary(Z_Song2.2)

anova(Z_Song2.0, Z_Song2.1, Z_Song2.2, test = "Chisq")
Anova(Z_Song2.1, type = 2)

Csongs <- visreg(Z_Song2.1, "Type", by = "Month",
                overlay = TRUE, rug = FALSE, type = "conditional", scale = "response",
       main = "Common chaffinch", xlab = "Experiment phase", ylab = "Diurnal songs", gg = T) + theme_classic() +
  ggtitle("Common chaffinch") +
  theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12)); Csongs

## Calls - models
Z_Call2.0 <- glmmTMB(Call ~ 1 + (1|Month/ID),
                     data = df_Z, family = "nbinom2"); summary(Z_Call2.0)

Z_Call2.1 <- glmmTMB(Call ~ relevel(Type, ref = 2) + Month + (1|Month/ID),
                     data = df_Z, family = "nbinom2"); summary(Z_Call2.1)

Z_Call2.2 <- glmmTMB(Call ~ relevel(Type, ref = 2) * Month + (1|Month/ID),
                     data = df_Z, family = "nbinom2"); summary(Z_Call2.2)

anova(Z_Call2.0, Z_Call2.1, Z_Call2.2, test = "Chisq")
Anova(Z_Call2.1, type = 2)

Ccalls <- visreg(Z_Call2.1, "Type", by = "Month", type = "conditional", scale = "response", rug = FALSE,
       main = "Common chaffinch", overlay = TRUE,
       xlab = "Experiment phase", ylab = "Diurnal calls", gg = TRUE) + theme_classic() +
      ggtitle("Common chaffinch") +
    theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 12)); Ccalls 

### Merge plots and save --------------------------------------------------
ar_sc <- ggarrange(Ycalls, Ccalls, Ysongs, Csongs, ncol = 2, nrow = 2, common.legend = TRUE,
          legend = "right"); ar_sc

tiff("Vocal_response.tiff", units = "in", width = 8, height = 6.5, res = 300)
ar_sc
dev.off()