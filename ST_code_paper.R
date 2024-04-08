#title: 'Carbon, nitrogen, and sulfur elemental and isotopic variations in mouse hair and bone collagen during short-term graded calorie restriction'
#author: Elea Gutierrez, Sharon Mitchell, Catherine Hambly, Kerry Sayle, Alex von Kriegsheim, John R. Speakman, and Kate Britton

{
library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggbreak)
library(ggsignif)
library(dunn.test)
library(RColorBrewer)
library(ggimage)
}

{
#Importing dataset
ST <- read_excel("ST.xlsx", col_types = c("text",  "text", "text", "numeric","numeric", "numeric","numeric",
                                          "numeric", "numeric","numeric", "numeric", "numeric","numeric",
                                          "numeric", "numeric","numeric","numeric"))
#Bone dataset
bone <- ST[c(1:17, 20:41),]

#Hair dataset
hair <- ST[c(43:59, 62:83),]

#Bone and hair dataset
bonehair <- ST[c(1:17, 20:41, 43:59, 62:83),]

#Adding column bone dataset
bone <- bone %>%
  add_column(D15Ncoll_diet = bone$d15N-5.0, 
             D34Scoll_diet = bone$d34S-8.6, 
             D15Ncoll_ker = bone$d15N - hair$d15N,
             D13Ccoll_ker = bone$d13C - hair$d13C,
             D34Scoll_ker = bone$d34S - hair$d34S)

#Adding column hair dataset
hair <- hair %>%
  add_column(D15Nker_diet = hair$d15N-5.0, 
             D34Sker_diet = hair$d34S-8.6, 
             D15Ncoll_ker = bone$d15N - hair$d15N,
             D13Ccoll_ker = bone$d13C - hair$d13C,
             D34Scoll_ker = bone$d34S - hair$d34S)
}

{
#Filtering by groups
zeroCR_b <-bone %>% 
  filter(CR_group == 0)
tenCR_b <- bone %>% 
  filter(CR_group == 1)
twentyCR_b <-bone %>% 
  filter(CR_group == 2)
thirtyCR_b <-bone %>% 
  filter(CR_group == 3)
fourtyCR_b <-bone %>% 
  filter(CR_group == 4)

zeroCR_h <-hair %>% 
  filter(CR_group == 0)
tenCR_h <- hair %>% 
  filter(CR_group == 1)
twentyCR_h <-hair %>% 
  filter(CR_group == 2)
thirtyCR_h <-hair %>% 
  filter(CR_group == 3)
fourtyCR_h <-hair %>% 
  filter(CR_group == 4)
zeroCR_bh <-bonehair %>% 
  filter(CR_group == 0)
}

#Normality of the bone and hair dataset for the isotopic values
shapiro.test(bone$d15N)

shapiro.test(bone$d13C)
bartlett.test(d13C ~ CR_group, data = bone) 
shapiro.test(zeroCR_b$d13C) 
shapiro.test(tenCR_b$d13C)
shapiro.test(twentyCR_b$d13C)
shapiro.test(thirtyCR_b$d13C)
shapiro.test(fourtyCR_b$d13C)

shapiro.test(bone$d34S)
bartlett.test(d34S ~ CR_group, data = bone) 
shapiro.test(zeroCR_b$d34S) 
shapiro.test(tenCR_b$d34S)
shapiro.test(twentyCR_b$d34S)
shapiro.test(thirtyCR_b$d34S)
shapiro.test(fourtyCR_b$d34S)

shapiro.test(hair$d15N)
shapiro.test(hair$d13C)
shapiro.test(hair$d34S)

#Statistics for Table 1
#Nitrogen, bone
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(d15N), 2),
    median = round(median(d15N, na.rm= TRUE), 2),
    min = round(min(d15N, na.rm = TRUE), 2),
    max = round(max(d15N, na.rm = TRUE), 2),
    IQR = round(IQR(d15N, na.rm = TRUE), 2))

#Nitrogen, hair
group_by(hair, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(d15N), 2),
    median = round(median(d15N, na.rm= TRUE), 2),
    min = round(min(d15N, na.rm = TRUE), 2),
    max = round(max(d15N, na.rm = TRUE), 2),
    IQR = round(IQR(d15N, na.rm = TRUE), 2))

#Nitrogen, bone-diet
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(D15Ncoll_diet), 2),
    median = round(median(D15Ncoll_diet, na.rm= TRUE), 2),
    min = round(min(D15Ncoll_diet, na.rm = TRUE), 2),
    max = round(max(D15Ncoll_diet, na.rm = TRUE), 2),
    IQR = round(IQR(D15Ncoll_diet, na.rm = TRUE), 2))

#Nitrogen, hair-diet
group_by(hair, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(D15Nker_diet), 2),
    median = round(median(D15Nker_diet, na.rm= TRUE), 2),
    min = round(min(D15Nker_diet, na.rm = TRUE), 2),
    max = round(max(D15Nker_diet, na.rm = TRUE), 2),
    IQR = round(IQR(D15Nker_diet, na.rm = TRUE), 2))

#Nitrogen, bone-hair
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(D15Ncoll_ker), 2),
    median = round(median(D15Ncoll_ker, na.rm= TRUE), 2),
    min = round(min(D15Ncoll_ker, na.rm = TRUE), 2),
    max = round(max(D15Ncoll_ker, na.rm = TRUE), 2),
    IQR = round(IQR(D15Ncoll_ker, na.rm = TRUE), 2))

#Carbon, bone
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(d13C), 2),
    median = round(median(d13C, na.rm= TRUE), 2),
    min = round(min(d13C, na.rm = TRUE), 2),
    max = round(max(d13C, na.rm = TRUE), 2),
    IQR = round(IQR(d13C, na.rm = TRUE), 2))

#Carbon, hair
group_by(hair, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(d13C), 2),
    median = round(median(d13C, na.rm= TRUE), 2),
    min = round(min(d13C, na.rm = TRUE), 2),
    max = round(max(d13C, na.rm = TRUE), 2),
    IQR = round(IQR(d13C, na.rm = TRUE), 2))

#Carbon, bone-hair
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(D13Ccoll_ker), 2),
    median = round(median(D13Ccoll_ker, na.rm= TRUE), 2),
    min = round(min(D13Ccoll_ker, na.rm = TRUE), 2),
    max = round(max(D13Ccoll_ker, na.rm = TRUE), 2),
    IQR = round(IQR(D13Ccoll_ker, na.rm = TRUE), 2))

#Sulphur, bone
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(d34S), 2),
    median = round(median(d34S, na.rm= TRUE), 2),
    min = round(min(d34S, na.rm = TRUE), 2),
    max = round(max(d34S, na.rm = TRUE), 2),
    IQR = round(IQR(d34S, na.rm = TRUE), 2))

#Sulphur, hair
group_by(hair, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(d34S), 2),
    median = round(median(d34S, na.rm= TRUE), 2),
    min = round(min(d34S, na.rm = TRUE), 2),
    max = round(max(d34S, na.rm = TRUE), 2),
    IQR = round(IQR(d34S, na.rm = TRUE), 2))

#Sulphur, bone-diet
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(D34Scoll_diet), 2),
    median = round(median(D34Scoll_diet, na.rm= TRUE), 2),
    min = round(min(D34Scoll_diet, na.rm = TRUE), 2),
    max = round(max(D34Scoll_diet, na.rm = TRUE), 2),
    IQR = round(IQR(D34Scoll_diet, na.rm = TRUE), 2))
    
#Sulphur, hair-diet
group_by(hair, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(D34Sker_diet), 2),
    median = round(median(D34Sker_diet, na.rm= TRUE), 2),
    min = round(min(D34Sker_diet, na.rm = TRUE), 2),
    max = round(max(D34Sker_diet, na.rm = TRUE), 2),
    IQR = round(IQR(D34Sker_diet, na.rm = TRUE), 2))

#Sulphur, bone-hair
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(D34Scoll_ker), 2),
    median = round(median(D34Scoll_ker, na.rm= TRUE), 2),
    min = round(min(D34Scoll_ker, na.rm = TRUE), 2),
    max = round(max(D34Scoll_ker, na.rm = TRUE), 2),
    IQR = round(IQR(D34Scoll_ker, na.rm = TRUE), 2))

#Kruskal-Wallis and Dunn tests for the bone and hair dataset
dunn.test (bone$d15N, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (bone$d13C, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (bone$d34S, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (hair$d15N, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (hair$d13C, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (hair$d34S, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)

#Figure 1A
boxplot_d15N_CR_group_hair_and_bone <- 
  ggplot(bonehair, aes(x=CR_group, y=d15N, fill=Tissue)) + 
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + 
  labs(x="Level of restriction", y = expression(paste(delta^{15}, "N AIR (\u2030)")), tag = "A") + 
  theme_classic() + 
  stat_summary(fun=mean, colour="gray28", geom="point", 
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + 
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0%\n(n=8)", 
                              "10%\n(n=8)", "20%\n(n=7)", "30%\n(n=7)", 
                              "40%\n(n=9)")) + 
  theme(axis.title = element_text(size=12), 
        axis.text = element_text(size=10), 
        legend.title=element_blank(), 
        legend.position=c(0.09,0.23), 
        legend.text = element_text(size = 12), 
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)
  ) +  
  scale_fill_brewer(palette = "Set3") + 
  scale_colour_brewer(palette = "Set3") + 
  geom_point(aes(shape = Tissue, 
                 colour = CN<=2.8, 
                 group= Tissue), 
             position = position_jitterdodge(0.25), 
             size=1.6, alpha=0.7) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") + 
  annotate("segment", x = 0.75, xend = 0.75, y = 4.5, yend = 4.7,
           colour = "black", linewidth = 0.5) + 
  annotate("segment", x = 0.72, xend = 0.78, y = 4.5, yend = 4.5,
           colour = "black", linewidth = 0.5) + 
  annotate("segment", x = 0.72, xend = 0.78, y = 4.7, yend = 4.7,
           colour = "black", linewidth = 0.5) + 
  annotate("text", x = 1.17, y = 4.6, label = "Analytical error") + 
  annotate("text", x = 5.3, y = 5.15, label = "Diet",size = 5.5, colour="dimgray") + 
  geom_hline(yintercept=5.0,linetype=5, size=1, colour="dimgray") + 
  geom_signif(y_position = c(10.5), xmin = c(1.2), 
              xmax = c(3.2), annotation = c("*"),
              tip_length = 0.01, vjust = 0.8,
              colour= 'goldenrod3')
boxplot_d15N_CR_group_hair_and_bone

#Figure 1B
{
  boxplot_d13C_CR_group_hair_and_bone <- 
    ggplot(bonehair, aes(x=CR_group, y=d13C, fill=Tissue)) + 
    theme_classic() +  
    theme(axis.text.y.right = element_blank(), axis.ticks.y.right = element_blank(), 
          axis.line.y.right = element_blank(),
          axis.title = element_text(size=12), 
          axis.text = element_text(size=10),
          legend.title=element_blank(),
          legend.text = element_text(size = 12), 
          plot.tag = element_text(size=18, face = "bold"),
          plot.tag.position =c(0.95, 0.95)
    ) +
    geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + 
    scale_y_break(c(-20.5, -16.5)) + 
    scale_y_continuous(limits = c(-24.1, -15), breaks = c(-24, -23, -22, -21, -16, -15)) + 
    labs(x="Level of restriction", y = expression(paste(delta^{13}, C, " VPDB (\u2030)")), tag = "B") + 
    stat_summary(fun=mean, colour="gray0", geom="point",
                 shape=4, size=2, show.legend  = FALSE, 
                 position = position_dodge(width = .75)) + 
    scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                     labels = c("0%\n(n=8)", 
                                "10%\n(n=8)", "20%\n(n=7)", "30%\n(n=7)", 
                                "40%\n(n=9)")) + 
    scale_fill_brewer(palette = "Set3") + 
    scale_colour_brewer(palette = "Set3") + 
    geom_point(aes(shape = Tissue, 
                   colour = CN<=2.8, 
                   group= Tissue), 
               position = position_jitterdodge(0.25), 
               size=1.6, alpha=0.7) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
    annotate("segment", x = 1.497, xend = 1.497, y = -24.0, yend = -23.9,
             colour = "black", linewidth = 0.5) +
    annotate("segment", x = 1.48, xend = 1.52, y = -24.0, yend = -24.0,
             colour = "black", linewidth = 0.5) +
    annotate("segment", x = 1.48, xend = 1.52, y = -23.9, yend = -23.9,
             colour = "black", linewidth = 0.5) + 
    annotate("text", x = 1.83, y = -23.93, label = "Analytical error") + 
    annotate("text", x = 5.3, y = -15.65, label = "Diet",size = 5.5, colour="dimgray") + 
    geom_hline(yintercept=-15.8,linetype=5, size=1, colour="dimgray") + 
    geom_signif(y_position = c(-16.4, -16.3,-16.2), xmin = c(0.8,0.8, 1.8), 
                xmax = c(3.8,4.8, 4.8), annotation = c("**","***","**"), colour = 'forestgreen',
                tip_length = 0.005, vjust = 0.5) 
  boxplot_d13C_CR_group_hair_and_bone
  boxplot_d13C_CR_group_hair_and_bone_2 <- boxplot_d13C_CR_group_hair_and_bone +
    theme(legend.position="none") 
  leg = cowplot::get_legend(boxplot_d13C_CR_group_hair_and_bone)
  boxplot_d13C_CR_group_hair_and_bone_3 <- ggplotify::as.ggplot(print(boxplot_d13C_CR_group_hair_and_bone_2))
  boxplot_d13C_CR_group_hair_and_bone_4 <- boxplot_d13C_CR_group_hair_and_bone_3 + ggimage::geom_subview(x=.18, y=.15, subview=leg)
  boxplot_d13C_CR_group_hair_and_bone_4
}

#Figure 1C
boxplot_d34S_CR_group_hair_and_bone <- 
  ggplot(bonehair, aes(x=CR_group, y=d34S, fill=Tissue)) + 
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + 
  labs(x="Level of restriction", y = expression(paste(delta^{34}, "S CDT (\u2030)")), tag = "C") + 
  theme_classic() + 
  stat_summary(fun=mean, colour="gray28", geom="point", 
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + 
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0%\n(n=8)", 
                              "10%\n(n=8)", "20%\n(n=7)", "30%\n(n=7)", 
                              "40%\n(n=9)")) + 
  theme(axis.title = element_text(size=12), 
        axis.text = element_text(size=10), 
        legend.title=element_blank(), 
        legend.position=c(0.09,0.215),  
        legend.text = element_text(size = 12), 
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)
  ) +  
  scale_fill_brewer(palette = "Set3") + 
  scale_colour_brewer(palette = "Set3") + 
  geom_point(aes(shape = Tissue, 
                 colour = CN<=2.8, 
                 group= Tissue), 
             position = position_jitterdodge(0.25), 
             size=1.6, alpha=0.7) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") + 
  annotate("segment", x = 0.75, xend = 0.75, y = 5.9, yend = 6.2,
           colour = "black", size = 0.5) +
  annotate("segment", x = 0.73, xend = 0.77, y = 5.9, yend = 5.9,
           colour = "black", size = 0.5) + 
  annotate("segment", x = 0.73, xend = 0.77, y = 6.2, yend = 6.2,
           colour = "black", size = 0.5) + 
  annotate("text", x = 1.15, y = 6.05, label = "Analytical error") + 
  annotate("text", x = 5.25, y = 8.72, label = "Diet", size = 5.5, colour="dimgray") + 
  geom_hline(yintercept=8.6,linetype=5, size=1, colour="dimgray")  
boxplot_d34S_CR_group_hair_and_bone 

#Normality of the hair dataset for the elemental content
shapiro.test(hair$percentN)
shapiro.test(hair$percentC)
shapiro.test(hair$percentS)

#Statistics for Table 2
#Nitrogen
group_by(hair, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(percentN, na.rm = TRUE), 2),
    median = round(median(percentN, na.rm= TRUE), 2),
    min = round(min(percentN, na.rm = TRUE), 2),
    max = round(max(percentN, na.rm = TRUE), 2),
    IQR = round(IQR(percentN, na.rm = TRUE), 2))
round(median(hair$percentN, na.rm=TRUE), 2)
round(min(hair$percentN, na.rm=TRUE), 2)
round(max(hair$percentN, na.rm=TRUE), 2)

#Carbon
group_by(hair, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(percentC, na.rm = TRUE), 2),
    median = round(median(percentC, na.rm= TRUE), 2),
    min = round(min(percentC, na.rm = TRUE), 2),
    max = round(max(percentC, na.rm = TRUE), 2),
    IQR = round(IQR(percentC, na.rm = TRUE), 2))
round(median(hair$percentC, na.rm=TRUE), 2)
round(min(hair$percentC, na.rm=TRUE), 2)
round(max(hair$percentC, na.rm=TRUE), 2)

#Sulphur
group_by(hair, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(percentS, na.rm = TRUE), 2),
    median = round(median(percentS, na.rm= TRUE), 2),
    min = round(min(percentS, na.rm = TRUE), 2),
    max = round(max(percentS, na.rm = TRUE), 2),
    IQR = round(IQR(percentS, na.rm = TRUE), 2))
round(median(hair$percentS, na.rm=TRUE), 2)
round(min(hair$percentS, na.rm=TRUE), 2)
round(max(hair$percentS, na.rm=TRUE), 2)

#Normality of the bone dataset for the elemental content
shapiro.test(bone$percentN)
bartlett.test(percentN ~ CR_group, data = bone) #test if same variance between groups
shapiro.test(zeroCR_b$percentN) #test if normally distributed inside group
shapiro.test(tenCR_b$percentN)
shapiro.test(twentyCR_b$percentN)
shapiro.test(thirtyCR_b$percentN)
shapiro.test(fourtyCR_b$percentN)
shapiro.test(bone$percentC)
shapiro.test(bone$percentS)

#Statistics for Table 2
#Nitrogen
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(percentN), 2),
    median = round(median(percentN, na.rm= TRUE), 2),
    min = round(min(percentN, na.rm = TRUE), 2),
    max = round(max(percentN, na.rm = TRUE), 2),
    IQR = round(IQR(percentN, na.rm = TRUE), 2))
round(min(bone$percentN, na.rm=TRUE), 2)
round(max(bone$percentN, na.rm=TRUE), 2)

#Carbon
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(percentC), 2),
    median = round(median(percentC, na.rm= TRUE), 2),
    min = round(min(percentC, na.rm = TRUE), 2),
    max = round(max(percentC, na.rm = TRUE), 2),
    IQR = round(IQR(percentC, na.rm = TRUE), 2))
round(min(bone$percentC, na.rm=TRUE), 2)
round(max(bone$percentC, na.rm=TRUE), 2)

#Sulphur
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(percentS), 2),
    median = round(median(percentS, na.rm= TRUE), 2),
    min = round(min(percentS, na.rm = TRUE), 2),
    max = round(max(percentS, na.rm = TRUE), 2),
    IQR = round(IQR(percentS, na.rm = TRUE), 2))
round(min(bone$percentS, na.rm=TRUE), 2)
round(max(bone$percentS, na.rm=TRUE), 2)

#C:N
group_by(bone, CR_group) %>%
  summarise(
    count = n(),
    mean = round(mean(CN), 2),
    median = round(median(CN, na.rm= TRUE), 2),
    min = round(min(CN, na.rm = TRUE), 2),
    max = round(max(CN, na.rm = TRUE), 2),
    IQR = round(IQR(CN, na.rm = TRUE), 2))

#Figure 2A
boxplot_percentN_CR_group_hair_and_bone <- 
  ggplot(bonehair, aes(x=CR_group, y=percentN, fill=Tissue)) + 
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + 
  labs(x="Level of restriction", y = "N content (%)", tag = "A") + 
  theme_classic() + 
  stat_summary(fun=mean, colour="gray28", geom="point", 
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + 
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0%\n(n=8)", 
                              "10%\n(n=8)", "20%\n(n=7)", "30%\n(n=7)", 
                              "40%\n(n=9)")) + 
  theme(axis.title = element_text(size=12), 
        axis.text = element_text(size=10), 
        legend.title=element_blank(), 
        legend.position=c(0.09,0.18),  
        legend.text = element_text(size = 12), 
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)
  ) +  
  scale_fill_brewer(palette = "Set3") + 
  scale_colour_brewer(palette = "Set3") + 
  geom_point(aes(shape = Tissue, 
                 colour = CN<=2.8, 
                 group= Tissue), 
             position = position_jitterdodge(0.25), 
             size=1.6, alpha=0.7) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
  geom_signif(y_position = c(15.5,15.7,15.9), xmin = c(0.8,0.8,1.8), 
              xmax = c(3.8,4.8,4.8), annotation = c("**","***","*"),
              tip_length = 0.01, vjust = 0.8,
              colour = 'forestgreen')
boxplot_percentN_CR_group_hair_and_bone 

#Figure 2B
boxplot_percentC_CR_group_hair_and_bone <- 
  ggplot(bonehair, aes(x=CR_group, y=percentC, fill=Tissue)) + 
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + 
  labs(x="Level of restriction", y = "C content (%)", tag = "B") + 
  theme_classic() + 
  stat_summary(fun=mean, colour="gray28", geom="point", 
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + 
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0%\n(n=8)", 
                              "10%\n(n=8)", "20%\n(n=7)", "30%\n(n=7)", 
                              "40%\n(n=9)")) + 
  theme(axis.title = element_text(size=12), 
        axis.text = element_text(size=10), 
        legend.title=element_blank(), 
        legend.position=c(0.09,0.12),  
        legend.text = element_text(size = 12), 
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)
  ) +  
  scale_fill_brewer(palette = "Set3") + 
  scale_colour_brewer(palette = "Set3") + 
  geom_point(aes(shape = Tissue, 
                 colour = CN<=2.8, 
                 group= Tissue), 
             position = position_jitterdodge(0.25), 
             size=1.6, alpha=0.7) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
  geom_signif(y_position = c(42.47,42.99,43.38), xmin = c(0.8,0.8,1.8), 
              xmax = c(3.8,4.8,4.8), annotation = c("**","***","**"),
              tip_length = 0.01, vjust = 0.8, 
              colour = 'forestgreen')
boxplot_percentC_CR_group_hair_and_bone

#Figure 2C
{
  boxplot_percentS_CR_group_hair_and_bone <- 
    ggplot(bonehair, aes(x=CR_group, y=percentS, fill=Tissue)) + 
    scale_y_break(c(0.45, 2.7)) + 
    theme_classic() +  
    theme(axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank(), 
          axis.line.x.top = element_blank(),
          axis.title = element_text(size=12), 
          axis.text = element_text(size=10),
          legend.title=element_blank(),
          legend.text = element_text(size = 12), 
          plot.tag = element_text(size=18, face = "bold"),
          plot.tag.position =c(0.95, 0.95)
    ) +
    geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + 
    #scale_y_continuous(limits = c(0, 4), breaks = c(1,2)) + 
    labs(x="Level of restriction", y = "S content (%)", tag = "C") + 
    stat_summary(fun=mean, colour="gray0", geom="point",
                 shape=4, size=2, show.legend  = FALSE, 
                 position = position_dodge(width = .75)) + 
    scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                     labels = c("0%\n(n=8)", 
                                "10%\n(n=8)", "20%\n(n=7)", "30%\n(n=7)", 
                                "40%\n(n=9)")) + 
    scale_fill_brewer(palette = "Set3") + 
    scale_colour_brewer(palette = "Set3") + 
    geom_point(aes(shape = Tissue, 
                   colour = CN<=2.8, 
                   group= Tissue), 
               position = position_jitterdodge(0.25), 
               size=1.6, alpha=0.7) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
    geom_signif(y_position = c(0.38,0.405,0.435), xmin = c(0.8,0.8,1.8), 
              xmax = c(3.8,4.8,4.8), annotation = c("**","***","**"),
              tip_length = 0.005, vjust = 0.5,
              colour = 'forestgreen') +
    geom_signif(y_position = c(3.9), xmin = c(4.2), 
                xmax = c(5.2), annotation = c("*"),
                tip_length = 0.005, vjust = 0.8,
                colour = 'goldenrod3')
  boxplot_percentS_CR_group_hair_and_bone
    boxplot_percentS_CR_group_hair_and_bone_2 <- boxplot_percentS_CR_group_hair_and_bone +
    theme(legend.position="none") 
  leg = cowplot::get_legend(boxplot_percentS_CR_group_hair_and_bone)
  boxplot_percentS_CR_group_hair_and_bone_3 <- ggplotify::as.ggplot(print(boxplot_percentS_CR_group_hair_and_bone_2))
  boxplot_percentS_CR_group_hair_and_bone_4 <- boxplot_percentS_CR_group_hair_and_bone_3 + ggimage::geom_subview(x=.18, y=.50, subview=leg)
  boxplot_percentS_CR_group_hair_and_bone_4
}

#Figure 3A
boxplot_CN_bone <- 
  ggplot(bone, aes(x=CR_group, y=CN, fill=CR_group)) + #create graph boxplots
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + #change the width of the lines
  labs(x="Level of restriction", y = expression(paste("C:N"[coll])), tag = "A") + #add title axis 
  theme_classic() + #change theme of the graph
  stat_summary(fun=mean, colour="gray28", geom="point", #add mean in boxplot
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + #align mean in boxplot
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0CR\n(n=8)", 
                              "10CR\n(n=8)", "20CR\n(n=7)", "30CR\n(n=7)", 
                              "40CR\n(n=9)")) + #add label for x axis
  theme(axis.title = element_text(size=12)) + #Change size axis titles
  theme(axis.text = element_text(size=10)) + #Change size axis text 
  theme(legend.position = "none",         
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)) + #remove legend boxplots
  scale_fill_grey() +
  geom_point(aes(shape = Tissue,
                 colour = CN<=2.8,
                 group= Tissue),
             position = position_jitterdodge(0.25),
             size=1.6, alpha=0.7) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
  geom_signif(y_position = c(3.41,3.43,3.45), xmin = c(1,2,2),
              xmax = c(5,4,5), annotation = c("*","**","**"),
              tip_length = 0.005, vjust = 0.75,
              colour = 'black') 
boxplot_CN_bone

#Figure 3B
boxplot_NS_bone <- 
  ggplot(bone, aes(x=CR_group, y=NS, fill=CR_group)) + #create graph boxplots
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + #change the width of the lines
  labs(x="Level of restriction", y = expression(paste("N:S"[coll])), tag = "B") + #add title axis 
  theme_classic() + #change theme of the graph
  stat_summary(fun=mean, colour="gray28", geom="point", #add mean in boxplot
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + #align mean in boxplot
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0CR\n(n=8)", 
                              "10CR\n(n=8)", "20CR\n(n=7)", "30CR\n(n=7)", 
                              "40CR\n(n=9)")) + #add label for x axis
  theme(axis.title = element_text(size=12)) + #Change size axis titles
  theme(axis.text = element_text(size=10)) + #Change size axis text 
  theme(legend.position = "none",
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)) + #remove legend boxplots
  scale_fill_grey() +
  geom_point(aes(shape = Tissue,
                 colour = CN<=2.8,
                 group= Tissue),
             position = position_jitterdodge(0.25),
             size=1.6, alpha=0.7) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
  geom_signif(y_position = c(190,192), xmin = c(1,2),
              xmax = c(5,5), annotation = c("***","***"),
              tip_length = 0.005, vjust = 0.75,
              colour = 'black') 
boxplot_NS_bone

#Figure 3C
boxplot_CS_bone <- 
  ggplot(bone, aes(x=CR_group, y=CS, fill=CR_group)) + #create graph boxplots
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + #change the width of the lines
  labs(x="Level of restriction", y = expression(paste("C:S"[coll])), tag = "C") + #add title axis 
  theme_classic() + #change theme of the graph
  stat_summary(fun=mean, colour="gray28", geom="point", #add mean in boxplot
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + #align mean in boxplot
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0CR\n(n=8)", 
                              "10CR\n(n=8)", "20CR\n(n=7)", "30CR\n(n=7)", 
                              "40CR\n(n=9)")) + #add label for x axis
  theme(axis.title = element_text(size=12)) + #Change size axis titles
  theme(axis.text = element_text(size=10)) + #Change size axis text 
  theme(legend.position = "none", 
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)) + #remove legend boxplots
  scale_fill_grey() +
  geom_point(aes(shape = Tissue,
                 colour = CN<=2.8,
                 group= Tissue),
             position = position_jitterdodge(0.25),
             size=1.6, alpha=0.7) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +
  geom_signif(y_position = c(550,557,566), xmin = c(1,1,2),
              xmax = c(4,5,5), annotation = c("*","***","***"),
              tip_length = 0.005, vjust = 0.7,
              colour = 'black') 
boxplot_CS_bone

#Kruskal-Wallis and Dunn tests for hair dataset
dunn.test (hair$percentN, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (hair$percentC, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (hair$percentS, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (hair$CN, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (hair$CS, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (hair$NS, g=hair$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)

#Kruskal-Wallis and Dunn tests for bone dataset
dunn.test (bone$percentN, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (bone$percentC, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (bone$percentS, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (bone$CN, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (bone$NS, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)
dunn.test (bone$CS, g=bone$CR_group, method='bonferroni', kw=TRUE, label=TRUE,
           wrap=FALSE, table=TRUE, list=FALSE, rmc=FALSE, alpha=0.05, altp=FALSE)

#Correlation between elemental content and stable isotopes
cor.test(bone$d15N, bone$percentN, method = "spearman")
cor.test(bone$d13C, bone$percentC, method = "spearman")
cor.test(bone$d34S, bone$percentS, method = "spearman")

#Figure S1
cor.test(zeroCR_b$d15N, zeroCR_h$d15N, method = "spearman")
ST_SI <- read_excel("ST_SI.xlsx")
scatterplot_d15Nh_d15Nb <- ggplot(ST_SI, aes(x=d15Nb, y=d15Nh)) + 
  geom_point (size=1.8, alpha = 0.6) +
  geom_point(shape = 1,size = 1.8,colour = "black") +
  geom_smooth(method="lm", fill = NA, alpha = 0.6, size = 1.25) + 
  theme_classic() + 
  labs(x = expression(paste(delta^{15}, "N" [coll], " AIR (\u2030)")), y = expression(paste(delta^{15}, "N" [ker], " AIR (\u2030)"))) +
  theme(legend.position = c(0.09,0.85), 
        legend.title = element_blank(), 
        legend.text = element_text (size = 12), 
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 1, linetype = "solid"),
        axis.title = element_text(size=12), 
        axis.text = element_text(size=10)  
  ) 
scatterplot_d15Nh_d15Nb

#Figure S2
cor.test(zeroCR_b$D15Ncoll_diet, zeroCR_b$Mass_diff, method = "spearman")
scatterplot_mass_diff_D15N_bone_AL <- ggplot(zeroCR_b, aes(x=D15Ncoll_diet, y=Mass_diff)) + 
  geom_point (size=1.8, alpha = 0.6) +
  geom_point(shape = 1,size = 1.8,colour = "black") +
  geom_smooth(method="lm", fill = NA, alpha = 0.6, size = 1.25) + 
  theme_classic() + 
  labs(x = expression(paste(Delta^{15}, "N" [coll-diet], " AIR (\u2030)")), y = "Mass difference") +
  theme(legend.position = c(0.09,0.85), 
        legend.title = element_blank(), 
        legend.text = element_text (size = 12), 
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 1, linetype = "solid"),
        axis.title = element_text(size=12), 
        axis.text = element_text(size=10)  
  ) 
scatterplot_mass_diff_D15N_bone_AL

#Figure S3A
boxplot_CN_hair <- 
  ggplot(hair, aes(x=CR_group, y=CN, fill=CR_group)) + #create graph boxplots
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + #change the width of the lines
  labs(x="Level of restriction", y = expression(paste("C:N"[ker])), tag = "A") + #add title axis 
  theme_classic() + #change theme of the graph
  stat_summary(fun=mean, colour="gray28", geom="point", #add mean in boxplot
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + #align mean in boxplot
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0CR\n(n=8)", 
                              "10CR\n(n=8)", "20CR\n(n=7)", "30CR\n(n=7)", 
                              "40CR\n(n=9)")) + #add label for x axis
  theme(axis.title = element_text(size=12)) + #Change size axis titles
  theme(axis.text = element_text(size=10)) + #Change size axis text 
  theme(legend.position = "none") + #remove legend boxplots
  theme(plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)) +
  scale_fill_grey() +
  geom_point(position = position_jitterdodge(0.25),
             size=1.6, alpha=0.7, colour = "black") +
  geom_signif(y_position = c(3.89), xmin = c(2),
              xmax = c(4), annotation = c("*"),
              tip_length = 0.005, vjust = 0.75,
              colour = 'black') 
boxplot_CN_hair

#Figure S3B
boxplot_CS_hair <- 
  ggplot(hair, aes(x=CR_group, y=CS, fill=CR_group)) + #create graph boxplots
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + #change the width of the lines
  labs(x="Level of restriction", y = expression(paste("C:S"[ker])), tag = "B") + #add title axis 
  theme_classic() + #change theme of the graph
  stat_summary(fun=mean, colour="gray28", geom="point", #add mean in boxplot
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + #align mean in boxplot
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0CR\n(n=8)", 
                              "10CR\n(n=8)", "20CR\n(n=7)", "30CR\n(n=7)", 
                              "40CR\n(n=9)")) + #add label for x axis
  theme(axis.title = element_text(size=12)) + #Change size axis titles
  theme(axis.text = element_text(size=10)) + #Change size axis text 
  theme(legend.position = "none") + #remove legend boxplots
  theme(plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)) +
  scale_fill_grey() +
  geom_point(position = position_jitterdodge(0.25),
             size=1.6, alpha=0.7, colour = "black") +
  geom_signif(y_position = c(41), xmin = c(4),
              xmax = c(5), annotation = c("***"),
              tip_length = 0.005, vjust = 0.75,
              colour = 'black') 
boxplot_CS_hair

#Figure S3C
boxplot_NS_hair <- 
  ggplot(hair, aes(x=CR_group, y=NS, fill=CR_group)) + #create graph boxplots
  geom_boxplot(lwd=0.8, fatten=0.6, outlier.shape = NA, alpha = 0.6) + #change the width of the lines
  labs(x="Level of restriction", y = expression(paste("N:S"[ker])), tag = "C") + #add title axis 
  theme_classic() + #change theme of the graph
  stat_summary(fun=mean, colour="gray28", geom="point", #add mean in boxplot
               shape=4, size=2, show.legend  = FALSE, 
               position = position_dodge(width = .75)) + #align mean in boxplot
  scale_x_discrete(limits = c("0", "1", "2", "3", "4"),
                   labels = c("0CR\n(n=8)", 
                              "10CR\n(n=8)", "20CR\n(n=7)", "30CR\n(n=7)", 
                              "40CR\n(n=9)")) + #add label for x axis
  theme(axis.title = element_text(size=12)) + #Change size axis titles
  theme(axis.text = element_text(size=10)) + #Change size axis text 
  theme(legend.position = "none") + #remove legend boxplots
  theme(plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95)) +
  scale_fill_grey() +
  geom_point(position = position_jitterdodge(0.25),
             size=1.6, alpha=0.7, colour = "black") +
  geom_signif(y_position = c(11, 11.05), xmin = c(2,4),
              xmax = c(5,5), annotation = c("**", "**"),
              tip_length = 0.005, vjust = 0.75,
              colour = 'black') 
boxplot_NS_hair

#Figure S4A
scatterplot_d15N_percentN_bone <- ggplot(bone, aes(x=d15N, y=percentN, colour=CR_group)) + 
  geom_smooth(method="lm", fill = NA, alpha = 0.6, size = 1.25) + 
  theme_classic() + 
  labs(x = expression(paste(delta^{15}, "N" [coll], " AIR (\u2030)")), y = "N content (%) in collagen", tag = "A") +
  scale_color_manual(labels=c("0%", "10%", "20%", "30%", "40%"), 
                     values=c("#9E0142", "#F46D43", "lemonchiffon3", "#ABDDA4", "#3288BD")) +
  theme(legend.position = c(0.09,0.15), 
        legend.title = element_blank(), 
        legend.text = element_text (size = 12), 
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 1, linetype = "solid"),
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95),
        axis.title = element_text(size=12), 
        axis.text = element_text(size=10)  
  ) +
  geom_point (aes(colour = CR_group), size=1.8, alpha = 0.6) +
  geom_point(shape = 1,size = 1.8,colour = "black") +
  geom_point(data = bone %>% filter(CN<=2.8), colour = "red", size = 2) 
scatterplot_d15N_percentN_bone

#Figure S4B
scatterplot_d13C_percentC_bone <- ggplot(bone, aes(x=d13C, y=percentC, colour=CR_group)) + 
  geom_point (aes(colour = CR_group), size=1.8, alpha = 0.6) +
  geom_point(shape = 1,size = 1.8,colour = "black") +
  geom_point(data = bone %>% filter(CN<=2.8), colour = "red", size = 2) +
  geom_smooth(method="lm", fill = NA, alpha = 0.6, size = 1.25) + 
  theme_classic() + 
  labs(x = expression(paste(delta^{13}, "C" [coll], " VPDB (\u2030)")), y = "C content (%) in collagen", tag = "B") +
  scale_color_manual(labels=c("0%", "10%", "20%", "30%", "40%"), 
                     values=c("#9E0142", "#F46D43", "lemonchiffon3", "#ABDDA4", "#3288BD")) +
  theme(legend.position = c(0.09,0.85), 
        legend.title = element_blank(), 
        legend.text = element_text (size = 12), 
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 1, linetype = "solid"),
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95),
        axis.title = element_text(size=12), 
        axis.text = element_text(size=10)  
  ) 
scatterplot_d13C_percentC_bone

#Figure S4C
scatterplot_d34S_percentS_bone <- ggplot(bone, aes(x=d34S, y=percentS, colour=CR_group)) + 
  geom_point (aes(colour = CR_group), size=1.8, alpha = 0.6) +
  geom_point(shape = 1,size = 1.8,colour = "black") +
  geom_point(data = bone %>% filter(CN<=2.8), colour = "red", size = 2) +
  geom_smooth(method="lm", fill = NA, alpha = 0.6, size = 1.25) + 
  theme_classic() + 
  labs(x = expression(paste(delta^{34}, "S" [coll], " CDT (\u2030)")), y = "S content (%) in collagen", tag = "C") +
  scale_color_manual(labels=c("0%", "10%", "20%", "30%", "40%"), 
                     values=c("#9E0142", "#F46D43", "lemonchiffon3", "#ABDDA4", "#3288BD")) +
  theme(legend.position = c(0.09,0.85), 
        legend.title = element_blank(), 
        legend.text = element_text (size = 12), 
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 1, linetype = "solid"),
        plot.tag = element_text(size=18, face = "bold"),
        plot.tag.position = c(0.95, 0.95),
        axis.title = element_text(size=12), 
        axis.text = element_text(size=10)  
  ) 
scatterplot_d34S_percentS_bone