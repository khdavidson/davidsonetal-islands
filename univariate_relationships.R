# Simple univariate relationships between mammal richness ~ area, dist mainland, dist NN, wrack
## section 1 of analysis in Davidson et al mammal islands paper
## re-analyzed Feb 2020

library(tidyverse)
library(car)

# read island and wrack data files 
setwd("~/UVic/`Field Work 2016/`RESULTS/Data files ch 2")
isl_data <- read.csv("islands_master_feb2020.csv")
wrack_data <- read.csv("wrack_biomass_SEPT2017.csv")       # NEEDS NEW WRACK DATA NEXT DAY! 
wrack_data2 <- read.csv("dry_wrack_biomass2.csv")

###########################
# WRACK DATA ORGANIZATION #
###########################

# restructure wrack data
wrack_data <- wrack_data %>% 
  select(-c(47)) %>% 
  gather(species, biomass, c(3:46)) %>% 
  print()

# calculate average biomass (g/m2) at each site (six 1m2 quadrats per site)
wrack_site <- wrack_data %>% 
  group_by(island, NODE_ISLAND_SITE) %>% 
  summarize(SITE_sum = sum(biomass)) %>% 
  mutate(avg_biomass_site = SITE_sum/6) %>% 
  mutate(n_sites = n()) %>%
  print()

# standardize per island (i.e., number of sites per island, minimum 4 per island, some with more if more beaches)
wrack_isl <- wrack_site %>% 
  group_by(island) %>% 
  summarize(total_biomass = sum(avg_biomass_site), n_sites=unique(n_sites)) %>% 
  mutate(wrack_std_isl = total_biomass/n_sites) %>%
  print()

wrack_isl_forjoin <- wrack_isl %>%
  select(c(1,4)) %>% 
  print()

########
# JOIN #
########

isl_master <- left_join(isl_data, wrack_isl_forjoin, by="island")
write.csv(isl_master, "island_master_feb2020.csv")

#####################
# CLEAN MASTER DATA #
#####################
 
isl_master <- isl_master %>% 
  select(-c(16:18)) %>% 
  mutate(log_area = log(area)) %>% 
  mutate(log_dist_ml = log(distw_ml)) %>% 
  mutate(log_dist_nn = log(dist_nn)) %>% 
  mutate(log_wrack1 = log(wrack_std_isl+1)) %>%
  mutate(log_slope = log(slope)) %>%
  mutate(log_s1 = log(n_spp+1)) %>%
  print()


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


##############################################################################################################################################################
  
                                                                    ########################
                                                                    # MULTIVARIATE MODELS  #
                                                                    ########################
#################################
# ASSESS PREDICTOR COLLINEARITY #  
#################################  
  
# Asess collinearity of predictors together  
z <- cbind(isl_master$log_area, isl_master$log_dist_nn, isl_master$log_dist_ml, isl_master$log_slope, isl_master$log_wrack1)
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
pairs(z, upper.panel = panel.cor, cex=1.75, pch=16)
  

# global model VIF 
m_global <- glm(n_spp ~ log_area + log_dist_nn + log_dist_ml + log_slope + log_wrack1, family=poisson(link = "log"), data=isl_master)
summary(m_global)
hist(resid(m_global))
plot(resid(m_global))
qqnorm(resid(m_global))
qqline(resid(m_global))

vif(m_global)


##############
# FIT MODELS #
##############
























