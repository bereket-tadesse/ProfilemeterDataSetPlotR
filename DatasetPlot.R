library(tidyverse)
library(ineq)

data1 <- read.csv(file.choose(), header=T)
data1

#piping : All variables.
data1 %>%
  ggplot( aes(TimeMin, rmsMicrons))  + 
  geom_point(aes(size = Zaxis, color = Light), alpha = 0.5) +
  stat_smooth(method = loess, se = T, colour = "red") +    #loess = loess model with outliers smoothing, se = stadard error
  stat_smooth(method = lm, se = T , colour = "grey") +    #lom = linear model, se = stadard error
  facet_wrap(~Magnification , labeller = labeller(Magnification = 
                                                    c("2.5" = "Maginification: 2.5"))) +
  labs(title = "Time Vs Rouhgness of Kapton ", x = "Time (Min)", y= "Rouhgness (microns)")  +
 scale_y_continuous(expand = c(0,0),limits = c(0, 20))
  
 # rms_list <- data1[5] +
 # text(x = 300, y = 300,                # Text with different color & size
   #    (paste ("Gini Index: " , ineq(as.numeric(unlist( rms_list))),type="Gini") ),
    #   col = "#1b98e0",
     #  cex = 2)

#piping : Time Vs Light
data1 %>%
  ggplot(aes(TimeMin, Light))  + 
  geom_point(size = 3, color = 'red', alpha = 0.5) +
 geom_smooth(aes(colour = "lm - linear"), method = lm, se = T , fill = 'brown', alpha = 0.2) +    #lm = linear model, se = stadard error
  facet_wrap(~Magnification , labeller = labeller(Magnification = 
                                                    c("2.5" = "Maginification: 2.5"))) +
  
  labs(title = "Time Vs Light Intensity", x = "Time (min)", y= "Light Intensity (%)") +
  scale_y_continuous(expand = c(0,0),limits = c(0, 0.46))

#piping : Time Vs Z-axis
data1 %>%
  ggplot(aes(TimeMin ,Zaxis ))  + 
  geom_point(size = 3, color = 'red', alpha = 1) +
  geom_smooth(aes(colour = "loess - handles outlier"),method = loess, se = T , fill = 'black', alpha = 0.2 ) +    #loess = with outliers smoothing, se = stadard error
  geom_smooth(aes(colour = "lm - linear"), method = lm, se = T , fill = 'brown', alpha = 0.2) +    #lm = linear model, se = stadard error
  
  facet_wrap(~Magnification , labeller = labeller(Magnification = 
                                                    c("2.5" = "Maginification: 2.5"))) +
  
  labs(title = "Time Vs Z-axis (mm)", x = "Time (min)", y= "Z-axis (mm)")


#piping : Time Vs rmsMicrons
data1 %>%
  ggplot(aes(TimeMin ,rmsMicrons ))  + 
  geom_point(size = 3, color = 'red', alpha = 1) +
  geom_smooth(aes(colour = "loess - handles outlier"),method = loess, se = T , fill = 'black', alpha = 0.2 ) +    #loess = with outliers smoothing, se = stadard error
  geom_smooth(aes(colour = "lm - linear"), method = lm, se = T , fill = 'brown', alpha = 0.2) +    #lm = linear model, se = stadard error
  facet_wrap(~Magnification , labeller = labeller(Magnification = 
                                                    c("2.5" = "Maginification: 2.5"))) +
  
  labs(title = "Time Vs rms(microns)", x = "Time (min)", y= "rms(microns)") +
  scale_y_continuous(expand = c(0,0),limits = c(0, 20))
