library(ggplot2)
library(dplyr)

setwd("~/")
neb <- read.csv("setaria_psent98.csv")
labels=sort(unique(neb$line))
labels

#Plotting reflectance by treatment

pdf(file="ari_psent98_me034.pdf",width = 10,height = 6.5,pointsize = 5,useDingbats = FALSE)
ggplot(neb, aes(x=label, y=ari_freq, group=line, color=line)) + 
  geom_line(size=0.5) + geom_point(size=0.5) + #geom_errorbar(aes(ymin=mean_freq-std_dev, ymax=mean_freq+std_dev, alpha=0), width=0) + 
  theme_bw() +
  scale_x_continuous("ARI index value") + 
  scale_y_continuous("Pixel Frequency (%)") + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 13), legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +
  coord_cartesian(ylim=c(0,35))+
  scale_color_discrete(name = "Treatment",labels=c("me034","psent98"))+
  ggtitle("ARI Detection") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

#ks TEST

me034 <- filter(neb, line == "me034")
psent98 <- filter(neb, line == "psent98")

ks.test(x = me034$ari_freq, y = psent98$ari_freq)