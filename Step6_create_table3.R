# create figure 1 - forest plot of model coefficients. 

# v2 = updated to include new beta coefficients from Suns

library(tidyverse)

temp = c("Age < X", 
           "Age >= X", 
           "Female", 
           "Local area is full of rubbish and litter*",
           "Low self-reported control over financial situation** ",
           "Fired a weapon or been fired upon in combat",
           "Decreasing satisfaction with daily life and leisure activities^",
           "Not at all satisified with retirement",
           "Less active with children^^",
           "Does no volunteering or charity work",
           "Less active in clubs$",
           "Has and meets with children less than once a year",
           "Has friends and writes or emails them every few months or less frequently", 
           "Feel isolated from others (some of the time vs. hardly even or never)",
           "Feel isolated from others (often vs. hardly even or never)",
           "Do not often feel 'in tune' with the people around you",
           "Feel part of a group of friends (some of the time vs. often)",
           "Feel part of a group of friends (hardly even or never vs. often)",
           "Treated with less courtesy or respect than other people (a few times a year or more often)",
           "People act as if they think you are not smart (a few times a year or more often)", 
           "Do not have a current major activity (job, looking after home, voluntary work)", 
           "Model intercept")
         
indicator = factor(temp, levels=temp)

indicator = factor(c("Age < X", 
              "Age >= X", 
              "Female", 
              "Local area is full of rubbish and litter*",
              "Low self-reported control over financial situation** ",
              "Fired a weapon or been fired upon in combat",
              "Decreasing satisfaction with daily life and leisure activities^",
              "Not at all satisified with retirement",
              "Less active with children^^",
              "Does no volunteering or charity work",
              "Less active in clubs$",
              "Has and meets with children less than once a year",
              "Has friends and writes or emails them every few months or less frequently", 
              "Feel isolated from others (some of the time vs. hardly even or never)",
              "Feel isolated from others (often vs. hardly even or never)",
              "Do not often feel 'in tune' with the people around you",
              "Feel part of a group of friends (some of the time vs. often)",
              "Feel part of a group of friends (hardly even or never vs. often)",
              "Treated with less courtesy or respect than other people (a few times a year or more often)",
              "People act as if they think you are not smart (a few times a year or more often)", 
              "Do not have a current major activity (job, looking after home, voluntary work)", 
              "Model intercept"))

# from M:/SVI/060302021 LASSO_desc_outcome edited.xlsx 
beta.original = c(  0.06381, 0.05487, -0.51228, 0.25949, -0.36397, -0.30294, 0.06147, 0.40864, 0.09821, 0.35349, 0.05358, 0.42056, 0.24282, 
           -0.13719, -0.13719, 0.12049, 0.06503, 0.06503, -0.19358, -0.17584, 0.16835, -6.47167)

se = c(0.01666, 0.01886, 0.08755, 0.10563, 0.12605, 0.15711, 0.04917, 0.17301, 0.02526, 0.09105, 0.02828, 0.15648, 0.09604, 0.07698, 
       0.07698, 0.09114, 0.06662, 0.06662, 0.10807, 0.11256, 0.11845, 1.26274)

m = c(1, 1, 1, 1, -1, -1, 1, 1, 1, 1, 1, 1, 1, -1, -2, 1, 1, 2, -1, -1, 1, 1)
      
include = c(F, F, F, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, F)
footnote <- 
"*defined as (>=4 on 1 to 7 scale where 1=area is kept very clean, 7=area is always full of rubbish and litter)
**defined as (<=3 on 0 to 10 scale where 1=no control at all, 10=very much control)
^ on a 1 to 5 scale (1=completely satisfied,5= not at all)
^^grandchildren, neighborhood children, nieces/nephews, on a 1 to 7 scale (1=daily, 7=never/not relevant)
$ sport, social, or other clubs on a 1 to 7 scale (1=daily, 7=never/not relevant)"
          
data.t2 <- data.frame(indicator, beta.original, se, m, include)
data.t2 <- filter(data.t2, include==T) %>% 
            mutate(beta = (beta.original * m)) %>% 
            mutate(OR = round(exp(beta), 2)) %>% 
            mutate(LL = round(exp(beta - 1.96*se), 2)) %>% 
            mutate(UL = round(exp(beta + 1.96*se), 2)) 

print(data.t2)
print(footnote)

ggplot(data.t2, aes(y=indicator, x=OR)) +
  geom_point() +
  geom_errorbar(aes(xmin=LL, xmax=UL), width = 0.2) +
  scale_y_discrete(name="") +
  scale_x_continuous(name = "Odds ratio for predictor", 
                     limits = c(0.8, 2.2), 
                     breaks = c(seq(from= 0.8, to=2.2, by = 0.2))) + 
  theme(
    panel.background = element_blank(), 
    axis.line = element_line(colour = "grey75"),
    panel.border = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "grey", linetype = "dotted" ),
    #    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none", 
    legend.title = element_blank(),
    axis.title=element_text(size=10),
    plot.caption = element_text(hjust = +0, face = "italic"),
    plot.title = element_text(hjust = +1)
  ) +
  geom_vline(xintercept = 1, linetype ="dashed")
  

