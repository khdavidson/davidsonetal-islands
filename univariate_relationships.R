# Simple univariate relationships between mammal richness ~ area, dist mainland, dist NN, wrack
## section 1 of analysis in Davidson et al mammal islands paper
## re-analyzed Feb 2020

library(tidyverse)

# read
setwd("~/UVic/`Field Work 2016/`RESULTS/Data files ch 2")
data <- read.csv("islands_master_feb2020.csv")

# create new log-transformed variables to use 
data <- data %>% 
  mutate(log_area = log(area)) %>% 
  mutate(log_dist_ml = log(distw_ml)) %>% 
  mutate(log_dist_nn = log(dist_nn)) %>% 
  mutate(log_wrack1 = log(wrack_biom+1)) %>%
  mutate(log_slope = log(slope)) %>%
  mutate(log_s1 = log(n_spp+1)) %>%
  print()


# Assess collinearity of predictors together - for multivariate models only
z <- cbind(data$area, data$dist_nn, data$distw_ml, data$slope, data$wrack_gperm2)
colnames(z) <- c("area", "NN", "dist ml", "slope", "wrack")
cor(z)
pairs(z)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = cor(x, y, use="na.or.complete")
  txt = format(c(r, 0.123456789), digits=digits)[1]
  txt = paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor = 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)#cex.cor * r)
}

pairs(z,
      upper.panel = panel.cor,
      cex=1.75,
      pch=16)


##############################################################################################################################################################

                                                                ######################
                                                                # UNIVARIATE MODELS  #
                                                                ######################

# log(S+1) ~ log(area) 

summary(lm(data$log_s1 ~ data$log_area))
summary(aov(data$log_s1 ~ data$log_area))

ggplot(data, aes(x=log_area, y=log_s1)) + 
  labs(x="log Area", y="log(S+1)") +
  ylim(c(0,2))+
  geom_vline(xintercept = 6.77, lty=2, col="red",size=1) +
  geom_vline(xintercept = 6.00, lty =2, size=1)+
  geom_smooth(method = "lm", formula = y~x, colour="black") +
  geom_point(shape=21, size = 7, stroke=2.1) +
  theme_bw()
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))+
  theme(axis.ticks.length = unit(0.4,"cm"))+
  theme(axis.ticks = element_line(size=2))



## log(S+1) ~ log(dist mainland) 

summary(lm(data$log_s1 ~ data$log_dist_ml))
summary(aov(data$log_s1 ~ data$log_dist_ml))

ggplot(data, aes(x=log_dist_ml, y=log_s1)) + 
  labs(x="Distance from mainland (km)", y="log(S+1)") +
  ylim(c(0,2))+
  geom_smooth(method = "lm", formula = y~x, colour="black") +
  geom_point(shape=21, size = 7, stroke=2.1) +
  theme_bw()
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))+
  xlab(bquote('log'* ~D[ML]~ ''))+
  theme(axis.ticks.length = unit(0.4,"cm"))+
  theme(axis.ticks = element_line(size=2))



# log(S+1) ~ log(dist nn) 
  
summary(lm(data$log_s1 ~ data$log_dist_nn))
summary(aov(data$log_s1 ~ data$log_dist_nn))
  
ggplot(data, aes(x=log_dist_nn, y=log_s1)) + 
  labs(x="Distance from nearest-neighbour", y="log(S+1)") +
  ylim(c(0,2))+
  geom_smooth(method = "lm", formula = y~x, colour="black") +
  geom_point(shape=21, size = 7, stroke=2.1) +
  theme_bw()
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))+
  xlab(bquote('log'* ~D[NN]~ ''))+
  theme(axis.ticks.length = unit(0.4,"cm"))+
  theme(axis.ticks = element_line(size=2))



# log(S+1) ~ log(wrack+1) 

summary(lm(data$log_s1 ~ data$log_wrack1))
summary(aov(data$log_s1 ~ data$log_wrack1))
  
ggplot(data, aes(x=log_wrack1, y=log_s1)) + 
  labs(x="log(Wrack+1)", y="log(S+1)") +
  ylim(c(0,2))+
  geom_smooth(method = "lm", formula = y~x, colour="black") +
  geom_point(shape=21, size = 7, stroke=2.1) +
  theme_bw()
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))+
  theme(axis.ticks.length = unit(0.4,"cm"))+
  theme(axis.ticks = element_line(size=2))


# log(S+1) ~ log(wrack+1) 
  
summary(lm(data$log_s1 ~ data$log_slope))
summary(aov(data$log_s1 ~ data$log_slope))
  
ggplot(data, aes(x=log_slope, y=log_s1)) + 
  labs(x="log Slope", y="log(S+1)") +
  ylim(c(0,2))+
  geom_smooth(method = "lm", formula = y~x, colour="black") +
  geom_point(shape=21, size = 7, stroke=2.1) +
  theme_bw()
  theme(text = element_text(size=50)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=40,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))+
  theme(axis.ticks.length = unit(0.4,"cm"))+
  theme(axis.ticks = element_line(size=2))







