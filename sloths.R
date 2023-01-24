install.packages("ggcorrplot")
install.packages("ggpubr")

library(datasets)
library(ggplot2)
library(plyr)
library(psych)
library(ggpubr)

sloth_data <- read.csv("sloth_data.csv", header = TRUE)

sloth_data <- sloth_data[,-1]

sloth_data[c("endangered", "specie", "sub_specie")] <- lapply(sloth_data[c("endangered", "specie", "sub_specie")], as.factor)

summary(sloth_data)

describeBy(sloth_data[c("size_cm", "claw_length_cm", "tail_length_cm", "weight_kg")])

corr <- round(cor(sloth_data[c("size_cm", "claw_length_cm", "tail_length_cm", "weight_kg")]), 2)

ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower")

mu1 <- ddply(sloth_data, .(specie), summarise, grp.mean=mean(size_cm))

p1 <- ggplot(sloth_data, aes(x=size_cm, color=specie, fill=specie)) + 
  geom_histogram(aes(y=after_stat(density)), binwidth = 0.35, alpha = 0.6, position="identity") + 
  geom_vline(data=mu1, aes(xintercept=grp.mean, color=specie), linetype="dashed") + 
  scale_color_brewer(palette="Accent") +
  scale_fill_brewer(palette="Accent") 

mu2 <- ddply(sloth_data, .(specie), summarise, grp.mean=mean(claw_length_cm))

p2 <- ggplot(sloth_data, aes(x=claw_length_cm, color=specie, fill=specie)) + 
  geom_histogram(aes(y=after_stat(density)), binwidth = 0.3, alpha = 0.6, position="identity") + 
  geom_vline(data=mu2, aes(xintercept=grp.mean, color=specie), linetype="dashed") + 
  scale_color_brewer(palette="Accent") +
  scale_fill_brewer(palette="Accent")

mu3 <- ddply(sloth_data, .(specie), summarise, grp.mean=mean(tail_length_cm))

p3 <- ggplot(sloth_data, aes(x=tail_length_cm,  color=specie, fill=specie)) + 
  geom_histogram(aes(y=after_stat(density)), binwidth = 0.3, alpha = 0.6, position="identity") + 
  geom_vline(data=mu3, aes(xintercept=grp.mean, color=specie), linetype="dashed") + 
  scale_color_brewer(palette="Accent") +
  scale_fill_brewer(palette="Accent")

mu4 <- ddply(sloth_data, .(specie), summarise, grp.mean=mean(weight_kg))

p4 <- ggplot(sloth_data, aes(x=weight_kg,  color=specie, fill=specie)) + 
  geom_histogram(aes(y=after_stat(density)), binwidth = 0.2, alpha = 0.6, position="identity") + 
  geom_vline(data=mu4, aes(xintercept=grp.mean, color=specie), linetype="dashed") + 
  scale_color_brewer(palette="Accent") +
  scale_fill_brewer(palette="Accent")

p1
p2
p3
p4

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

ggplot(sloth_data, aes(x=sub_specie, y=size_cm, fill=specie, color=specie)) + 
  geom_violin(alpha=0.5, trim = FALSE) + 
  geom_boxplot(width=0.1, alpha=0.5) + 
  scale_color_brewer(palette="Accent") +
  scale_fill_brewer(palette="Accent") +
  theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), plot.title = element_text(hjust = 0.5)) + 
  labs(title="Sizes by subspecies", x="Subspecies", y="Size [cm]", color="Species", fill="Species")



