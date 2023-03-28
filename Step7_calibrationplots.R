###10/18/2021 Sun - updated codes for calibration plots from line64-line70

#install.packages("ellipsis", dependencies=TRUE)
#install.packages("vctrs", dependencies=TRUE)
#install.packages("haven", dependencies=TRUE)
#install.packages("tidyverse", dependencies=TRUE)
#install.packages("rms", dependencies=TRUE)
#install.packages("devtools", dependencies=TRUE)

#install.packages("prodlim")
#install.packages("crayon", dependencies=TRUE)
#install.packages("backports", ependencies=TRUE)
#install.packages("data.table", dependencies=TRUE)
#install.packages("plotrix", dependencies=TRUE)
#install.packages("riskRegression")
#install.packages("survival", dependencies=TRUE)
#install.packages("coxphw")
#install.packages("cmprsk", dependencies=TRUE)
#install.packages("timereg", dependencies=TRUE)

#v2 - sachin edits ? pred_glm 
#v3 - updated with final model

library(prodlim)
library(crayon)
library(backports)
library(data.table)
library(plotrix)
library(riskRegression)
library(survival)
library(coxphw)
library(cmprsk)
library(timereg)


library(ellipsis)
library(vctrs)
library(haven)
library(tidyverse)
library(rms)
library(devtools)


## Load data
data.roc <- read.csv("results/02222022 trimmed_model_results/02222022 test_results.csv")

# https://darrendahly.github.io/post/homr/
val_m1 <- val.prob(data.roc$test.prob, data.roc$death_flag, pl=F)
val_m1

ggplot(data.cal, aes(x=mean_p, y=mean_obs_death)) +
  geom_point() +
  scale_y_continuous(name = "Observed probability", 
                    limits = c(0, 1), 
                    breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(name = "Predicted probability",
                    limits = c(0, 1), 
                    breaks = seq(0, 1, by = 0.1)) +
  geom_abline(linetype="dotted") + # 45 degree line indicating perfect calibration
  geom_smooth(method = "lm", se = FALSE, color = "black") 
  

##death
  death.cal <- mutate(data.roc, bin = ntile(test.prob, 10)) %>%
    group_by(bin) %>%
    mutate(n=n(),
           bin_pred = mean(test.prob), 
           bin_prob = mean(death_flag),
           se = sqrt((bin_prob * (1-bin_prob))/n),
           ul = bin_prob + 1.96*se,
           ll = bin_prob - 1.96*se) %>%
    ungroup() %>%
    ggplot(aes(x=bin_pred, y=bin_prob, ymin=ll, ymax=ul)) + 
    scale_y_continuous(limits=c(0,0.8), breaks=seq(0,1,by=0.1)) + 
    scale_x_continuous(limits=c(0,0.8), breaks=seq(0,1,by=0.1)) + 
    geom_abline()+
    geom_smooth(method="loess", se=FALSE,  color="red", formula=y~-1+x)+
    geom_pointrange(size=0.8, color="black", shape=18) +
    xlab("Predicted Probability") + 
    ylab("Observed Probability") + 
    theme_minimal() 
  
  death.cal 
  
  death.cal.paper <- death.cal +  
    theme(plot.caption = element_text(hjust = 0)) + # set the left align here
    labs(title="Figure 2: Calibration of Social Frailty Index in validation cohort", 
                    caption = 
"Legend:
Plot of observed by predicted probability of 4-year mortality in the\n validation cohort, by decile of predicted risk. 
")

  ggsave(paste0("results/calibration plot ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".png"), death.cal, width = 5, height = 5)
  ggsave(paste0("results/calibration plot ", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".eps"), death.cal, width = 5, height = 5)
  
  data.roc %>% 
    mutate(bin = ntile(test.prob, 10)) %>% 
    group_by(bin) %>% 
    summarize(n=n(), bin_pred=mean(test.prob), bin_prob = mean(death_flag))
    
    
  mutate(data.roc, bin = ntile(test.prob, 10)) %>%
    group_by(bin) %>%
    summarise(n=n(), bin_pred = mean(test.prob), bin_obs = mean(death_flag), 
              se = sqrt((bin_obs * (1-bin_obs))/n), ul = bin_obs + 1.96*se, ll = bin_obs - 1.96*se) %>% 
    kable(digits = 3)
  
  
###################