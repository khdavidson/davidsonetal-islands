# rarefaction curve code revised Feb 2020

# read in
library(rareNMtests)
library(vegan)
library(tidyverse)
library(ggpubr)

setwd("~/Documents/`Field Work 2016/`RESULTS/Data files ch 2")

data <- read.csv("Mammal_Richness_2015-16-17_ToShare.csv")

data_r <- data %>% 
  mutate_at(vars(c(3:9)), funs(as.numeric)) %>% 
  select(-c(1:2)) %>%
  print()

# vegan sample (site)-based rarefaction using random method 
r <- specaccum(data_r, method = "random", permutations = 1000)
t <- as.data.frame(cbind(unlist(r[3]), unlist(r[4]), unlist(r[5])))

# plot results
all<-ggplot(data=t) +
  geom_ribbon(aes(x=V1, ymin=V2-V3, ymax=V2+V3), fill="gray80", alpha=0.6) +
  geom_line(aes(x=V1, y=V2), size=1.3) +
  annotate(geom="text", label="D", x=0.3, y=8.3, color="black", fontface="bold", size=6) +
  labs(x="Islands", y="Species Richness") +
  theme_bw() +
  theme(text = element_text(size=20))


#################
# SPLIT BY YEAR #
#################

# rarefaction curves by year to see if spp accum occurred differently year-to-year

##### 2015 #####

data2015 <- data %>% 
  filter(Year=="2015") %>%
  mutate_at(vars(c(3:9)), funs(as.numeric)) %>% 
  select(-c(1:2)) %>%
  print()

# vegan sample (site)-based rarefaction
r2015 <- specaccum(data2015, method = "random", permutations = 1000)
t2015 <- as.data.frame(cbind(unlist(r2015[3]), unlist(r2015[4]), unlist(r2015[5])))

# plot results
y15<-ggplot(data=t2015) +
  geom_ribbon(aes(x=V1, ymin=V2-V3, ymax=V2+V3), fill="gray80", alpha=0.6) +
  geom_line(aes(x=V1, y=V2), size=1.3) +
  annotate(geom="text", label="A", x=0.3, y=8.3, color="black", fontface="bold", size=6) +
  labs(x="Sites", y="Richness") +
  theme_bw() +
  theme(text = element_text(size=20))


##### 2016 #####

data2016 <- data %>% 
  filter(Year=="2016") %>%
  mutate_at(vars(c(3:9)), funs(as.numeric)) %>% 
  select(-c(1:2)) %>%
  print()

# vegan sample (site)-based rarefaction
r2016 <- specaccum(data2016, method = "random", permutations = 1000)
t2016 <- as.data.frame(cbind(unlist(r2016[3]), unlist(r2016[4]), unlist(r2016[5])))

# plot results
y16<-ggplot(data=t2016) +
  geom_ribbon(aes(x=V1, ymin=V2-V3, ymax=V2+V3), fill="gray80", alpha=0.6) +
  geom_line(aes(x=V1, y=V2), size=1.3) +
  annotate(geom="text", label="B", x=0.3, y=8.3, color="black", fontface="bold", size=6) +
  labs(x="Sites", y="Richness") +
  theme_bw() +
  theme(text = element_text(size=20))


##### 2017 #####

data2017 <- data %>% 
  filter(Year=="2017") %>%
  mutate_at(vars(c(3:9)), funs(as.numeric)) %>% 
  select(-c(1:2)) %>%
  print()

# vegan sample (site)-based rarefaction
r2017 <- specaccum(data2017, method = "random", permutations = 1000)
t2017 <- as.data.frame(cbind(unlist(r2017[3]), unlist(r2017[4]), unlist(r2017[5])))

# plot results
y17<-ggplot(data=t2017) +
  geom_ribbon(aes(x=V1, ymin=V2-V3, ymax=V2+V3), fill="gray80", alpha=0.6) +
  geom_line(aes(x=V1, y=V2), size=1.3) +
  annotate(geom="text", label="C", x=0.3, y=8.3, color="black", fontface="bold", size=6) +
  labs(x="Sites", y="Richness") +
  theme_bw() +
  theme(text = element_text(size=20))


ggarrange(y15, y16, y17, all, ncol=2, nrow=2)










########################################################################################################################################################

# OLD BELOW using rareNMtests package - similar results
sbr.q0 <- rarefaction.sample(data[,-1])
sbr.q1 <- rarefaction.sample(data[,-1], q=1)
sbr.q2 <- rarefaction.sample(data[,-1], q=2)
sbr.q0.cov <- rarefaction.sample(data[,-1], method="coverage")
sbr.q1.cov <- rarefaction.sample(data[,-1], q=1, method="coverage")
sbr.q2.cov <- rarefaction.sample(data[,-1], q=2, method="coverage")

# Plot the results
par(mfcol=c(1,2))
plot(sbr.q0[,1], sbr.q0[,2], lwd=2, xlab="Sampling units", ylab="Hill numbers",
     type="l", 
     cex.lab = 2, cex.axis = 2)
lines(sbr.q1[,1], sbr.q1[,2], lwd=2, lty=2)
lines(sbr.q2[,1], sbr.q2[,2], lwd=2, lty=3)
legend("bottomright", lty=c(1,2,3), lwd=2, legend=c("q = 0", "q = 1", "q = 2"), cex=1.2)


# using ggplot, first need to rename columns
names(sbr.q0)[names(sbr.q0) == "sample-size"] <- "samplesize"
names(sbr.q0)[names(sbr.q0) == "Hill (q=0)"] <- "hill"

ggplot(sbr.q0, aes(samplesize, hill)) + 
  labs(x="Sampling unit (islands)", y="Species richness")+
  geom_line(size=2)+
  theme(text = element_text(size=60)) + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(panel.grid.minor = element_line(colour = "transparent")) + 
  theme(panel.grid.major = element_line(colour = "transparent", linetype = "dotted", size = 0.2)) + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(axis.text = element_text(colour = "black")) + 
 # theme(axis.text.x = element_text(size=50)) +
  theme(axis.title = element_text(colour = "black")) + 
  theme(panel.background = element_rect(colour = "red")) + 
  theme(axis.ticks = element_line(size = 1, colour = "black")) +
  theme(legend.position="none")+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(axis.title.x = element_text(margin=margin(t=30,r=0,b=0,l=0))) +
  theme(axis.title.y = element_text(margin=margin(t=0,r=30,b=0,l=0),angle=90,vjust=0.5)) +
  theme(plot.margin=unit(c(3,2,2,2),"cm"))+
  theme(axis.ticks.length = unit(0.4,"cm"))+
  theme(axis.ticks = element_line(size=2))


plot(sbr.q0.cov[,1], sbr.q0.cov[,2], lwd=2, xlab="Coverage", ylab="Hill numbers",
     type="l", main="Coverage-based rarefaction")
lines(sbr.q1.cov[,1], sbr.q1.cov[,2], lwd=2, lty=2)
lines(sbr.q2.cov[,1], sbr.q2.cov[,2], lwd=2, lty=3)
legend("topleft", lty=c(1,2,3), lwd=2, legend=c("q = 0", "q = 1", "q = 2"), cex=1.2)







