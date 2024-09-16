library(ggplot2)
library(dplyr)

setwd("~/")

TA <- read.csv("anthocyanin_TA.csv")
labels=sort(unique(TA$line))

#ks TEST
unique(TA$line)

ta67 <- filter(TA, line == "#6-7_1uM_TA")
ta87 <- filter(TA, line == "#8-7_1uM_TA")
null810 <- filter(TA, line == "#8-10_1uM_TA")

ks.test(x = ta67$ari_freq, y = null810$ari_freq)
ks.test(x = ta87$ari_freq, y = null810$ari_freq)

pdf(file="anthocyanin-ari_TA.pdf",width = 10,height = 6.5,pointsize = 5,useDingbats = FALSE)
ggplot(TA, aes(x=label, y=ari_freq, group=line, color=line)) + 
  geom_line() + geom_point() + #geom_errorbar(aes(ymin=mean_freq-std_dev, ymax=mean_freq+std_dev, alpha=0), width=0) + 
  theme_bw() +
  scale_x_continuous("ARI index value") + 
  scale_y_continuous("Pixel Frequency (%)") + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 13), legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) + scale_color_manual(name= "Construct",values=c("deepskyblue2","cornsilk4", "coral1"))+
  ylim(0,15)+
  xlim(-15,15)+
  #scale_color_discrete(name = "Construct",labels=c("#6-7_1uM_TA", "null_1uM_TA","#8-7_1uM_TA"))+
  ggtitle("Anthocyanin Detection TA") + theme(plot.title = element_text(hjust = 0.5))#+ facet_wrap(~ construct)
dev.off()

DSP <- read.csv("anthocyanin_DSP.csv")
labels=sort(unique(DSP$line))

unique(DSP$line)

dsp67 <- filter(DSP, line == "#6-7_1uM_DSP")
dsp87 <- filter(DSP, line == "#8-7_1uM_DSP")
null810 <- filter(TA, line == "#8-10_1uM_TA")

ks.test(x = dsp67$ari_freq, y = null810$ari_freq)
ks.test(x = dsp87$ari_freq, y = null810$ari_freq)

pdf(file="anthocyanin-ari_DSP.pdf",width = 10,height = 6.5,pointsize = 5,useDingbats = FALSE)
ggplot(DSP, aes(x=label, y=ari_freq, group=line, color=line)) + 
  geom_line() + geom_point() + #geom_errorbar(aes(ymin=mean_freq-std_dev, ymax=mean_freq+std_dev, alpha=0), width=0) + 
  theme_bw() +
  scale_x_continuous("ARI index value") + 
  scale_y_continuous("Pixel Frequency (%)") + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 13), legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) + scale_color_manual(name= "Construct",values=c("deepskyblue2","coral1"))+
  #scale_color_discrete(name = "Construct",labels=c("#6-7_1uM_DSP", "#8-7_1uM_DSP"))+
  ylim(0,15)+
  xlim(-15,15)+
  ggtitle("Anthocyanin Detection DSP") + theme(plot.title = element_text(hjust = 0.5))#+ facet_wrap(~ construct)
dev.off()


DEX <- read.csv("anthocyanin_DEX.csv")
labels=sort(unique(DEX$line))

unique(DEX$line)

dex67 <- filter(DEX, line == "#6-7_1uM_DEX")
dex87 <- filter(DEX, line == "#8-7_1uM_DEX")
null810 <- filter(TA, line == "#8-10_1uM_TA")

ks.test(x = dex67$ari_freq, y = null810$ari_freq)
ks.test(x = dex87$ari_freq, y = null810$ari_freq)

pdf(file="anthocyanin-ari_DEX.pdf",width = 10,height = 6.5,pointsize = 5,useDingbats = FALSE)
ggplot(DEX, aes(x=label, y=ari_freq, group=line, color=line)) + 
  geom_line() + geom_point() + #geom_errorbar(aes(ymin=mean_freq-std_dev, ymax=mean_freq+std_dev, alpha=0), width=0) + 
  theme_bw() +
  scale_x_continuous("ARI index value") + 
  scale_y_continuous("Pixel Frequency (%)") + 
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 13), legend.text = element_text(size = 12),
        legend.title = element_text(size = 13)) +  scale_color_manual(name= "Construct",values=c("deepskyblue2","coral1"))+
  #scale_color_discrete(name = "Construct",labels=c("#6-7_1uM_DEX", "#8-7_1uM_DEX"))+
  ylim(0,15)+
  xlim(-15,15)+
  ggtitle("Anthocyanin Detection DEX") + theme(plot.title = element_text(hjust = 0.5))#+ facet_wrap(~ construct)
dev.off()
