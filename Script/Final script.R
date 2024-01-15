library(ggplot2)
library(ggpattern)
library(ggpubr)
library(ggtext)
library(tidyr)
library(dplyr)
library(plyr)
library(DataCombine)
library(stringr)
library(readxl)
library(FactoMineR)
library(data.table)
library(lme4)
library(car)
library(performance)
library(glmmTMB)
library(mvabund)
library(writexl)

##Fig 2
col2<-c("#279AB7","#034455","#B74427","#551403")

Tiles<-read_xlsx("Data/Tiles.xlsx")
Tilesum<-ddply(Tiles,c("Year","Orientation","Material","Texture","Abb","Age"),
                summarise,total=mean(Density),CI=qnorm(0.975)*sd(Density)/sqrt(length(Density)))
Tilesum$title<-paste(Tilesum$Age,"(",Tilesum$Year,")")
Tilesum$Orientation<-factor(Tilesum$Orientation,c("Upper side","Lower side"))
Tilesum$Abb<-factor(Tilesum$Abb,c("FPVC","TPVC","FPC","TPC"))

##Recruit density in 2020 and 2021
Tilesum1<-Tilesum %>% filter(Age=="9 months")

fig2a<-ggplot(Tilesum1, aes(x=Abb, y = total,fill= Abb)) +
  geom_bar(stat = "identity") +
  facet_grid(cols=vars(title),rows=vars(Orientation),scales="free_x")+
  xlab("")+
  ylim(0,30)+
  ylab(expression(paste("Live recruits per 100cm"^{2})))+
  theme(panel.background = element_rect(fill = "white"),legend.position="none",axis.text.x = element_text(angle = 90))+
  geom_errorbar(aes(ymin = total-CI, ymax = total + CI), width = 0.2)+
  scale_fill_manual(values=col2)+
  ggtitle("Recruit density")

##New recruit density in 2022 
Tilesum2<-Tilesum %>% filter(Age=="21 months")
fig2b<-ggplot(Tilesum2, aes(x=Abb, y = total,fill= Abb)) +
  geom_bar(stat = "identity") +
  facet_grid(rows=vars(Orientation),cols=vars(title))+
  xlab("")+
  ylim(0,30)+
  ylab(expression(paste("Live recruits per 100cm"^{2})))+
  theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(angle = 90),legend.position="none")+
  geom_errorbar(aes(ymin = total-CI, ymax = total + CI), width = 0.2)+
  scale_fill_manual(values=col2)+
  ggtitle("New recruitment")

##1 year survival of 2021 initial recruits
Survival<-Tiles %>% filter(Year=="2021" & Recruits !=0)
Survival$survival<-Survival$Survivors*100/Survival$Recruits
Survsum<-ddply(Survival,c("Orientation","Abb"),summarise,total=mean(survival),CI=qnorm(0.975)*sd(survival)/sqrt(length(survival)))
Survsum$title<-"21 months (2022)"
Survsum$Orientation<-factor(Survsum$Orientation,c("Upper side","Lower side"))
Survsum$Abb<-factor(Survsum$Abb,c("FPVC","TPVC","FPC","TPC"))

fig2c<-ggplot(Survsum, aes(x=Abb, y = total,fill= Abb)) +
  geom_bar(stat = "identity") +
  facet_grid(rows=vars(Orientation),cols=vars(title))+
  xlab("")+
  ylab("Recruit survival %")+
  theme(panel.background = element_rect(fill = "white"),axis.text.x = element_text(angle = 90),legend.position="none")+
  geom_errorbar(aes(ymin = total-CI, ymax = total + CI), width = 0.2)+
  scale_fill_manual(values=col2)+
  ggtitle("1 year survival")

##1 year growth of 2021 initial recruits
Growth<-read_xlsx("Data/Growth.xlsx")
Growth$factor<-Growth$Polyps2/Growth$Polyps1
Growsum<-ddply(Growth,c("Orientation","Abb"),summarise,total=mean(factor),CI=qnorm(0.975)*sd(factor)/sqrt(length(factor)))
Growsum$title<-"21 months (2022)"
Growsum$Orientation<-factor(Growsum$Orientation,c("Upper side","Lower side"))
Growsum$Abb<-factor(Growsum$Abb,c("FPVC","TPVC","FPC","TPC"))

fig2d<-ggplot(Growsum, aes(x=Abb, y = total,fill= Abb)) +
  geom_bar(stat = "identity") +
  facet_grid(rows=vars(Orientation),cols=vars(title))+
  xlab("")+
  ylab("Recruit growth factor")+
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90),legend.position="none")+
  geom_errorbar(aes(ymin = total-CI, ymax = total + CI), width = 0.2)+
  scale_fill_manual(values=col2)+
  ggtitle("1 year growth")

##Combine figures and save
Fig2AB<-ggarrange(fig2a,fig2b,ncol=2,widths=c(1,0.6))
Fig2CD<-ggarrange(fig2c,fig2d,ncol=2)

ggsave("Output/Fig2AB.svg",Fig2AB,width = 6, height = 4)
ggsave("Output/Fig2CD.svg",Fig2CD,width = 4.5, height = 4)

##Fig S1: Recruit size
Allrecruits<-read_xlsx("Data/Allrecruits.xlsx")
Recruits_9m<-Allrecruits %>% filter(Age=="9 months")
Sizesum<-ddply(Recruits_9m,c("Orientation","Abb"),summarise,total=mean(Polyps),CI=qnorm(0.975)*sd(Polyps)/sqrt(length(Polyps)))
Sizesum$PW<-c("b","ab","b","a","b","a","a")
Sizesum$title<-"9 months (2020 + 2021)"
Sizesum$Orientation<-factor(Sizesum$Orientation,c("Upper side","Lower side"))
Sizesum$Abb<-factor(Sizesum$Abb,c("FPVC","TPVC","FPC","TPC"))

FigS1<-ggplot(Sizesum, aes(x=Abb, y = total,fill= Abb)) +
  geom_bar(stat = "identity") +
  xlab("")+
  facet_grid(cols=vars(title),rows=vars(Orientation),scales="free_x")+
  ylab("Recruit size (polyps)")+
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90),legend.position="none")+
  geom_errorbar(aes(ymin = total-CI, ymax = total + CI), width = 0.2)+
  geom_text(aes(label = PW,y=9 ), colour="gray11", size=4)+
  scale_fill_manual(values=col2)+
  ggtitle("Recruit size")
ggsave("Output/FigS1.svg",FigS1,width = 2.5, height = 4)

##################################################

##Fig 3 (only Textured tiles)
col4<-c("#F09554","#78DBBA","#2D807F","#E8DA9D","#3B3838")
Tileareas<-read_xlsx("Data/Tileareas.xlsx")
Areasum<-ddply(Tileareas,c("Material","Orientation","Tile_area","Age"),
               summarise,total=mean(Density),CI=qnorm(0.975)*sd(Density)/sqrt(length(Density)))
Areasum$Material<-factor(Areasum$Material,c("PVC","Porous concrete"))
Areasum$Orientation<-factor(Areasum$Orientation,c("Upper side","Lower side"))

##recruit density after 9 months
Tileareas9<-Tileareas %>% filter(Age=="9 months")
pairwise.wilcox.test(Tileareas9$Density,Tileareas9$Tile_area)

Areasum9<-Areasum %>% filter(Age=="9 months")
Areasum9$KW<-c("a","a","a","a","b",
              "a","a","a","a","a",
              "a","a","a","a","b",
              "b","b","b","b","a")
              
fig3a<-ggplot(Areasum9, aes(x=Tile_area, y = total,fill= Tile_area)) +
  geom_bar(stat = "identity") +
  xlab("Tile area")+
  facet_grid(cols=vars(Material),rows=vars(Orientation),scales="free_x")+
  ylab(expression(paste("Recruit density per 100cm"^{2})))+
  geom_errorbar(aes(ymin = total-CI, ymax = total + CI), width = 0.2)+
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        plot.title = element_textbox(hjust = 0.5, margin = margin(t = 5, b = 5)))+
  scale_fill_manual(values=col4)+
  geom_text(aes(label = KW,y=14 ), colour="gray11", size=4)+
  ggtitle("Recruit density (9  mo)")

##recruit density after 21 months
Tileareas21<-Tileareas %>% filter(Age=="21 months")
pairwise.wilcox.test(Tileareas21$Density,Tileareas21$Tile_area)

Areasum21<-Areasum %>% filter(Age=="21 months")
Areasum21$KW<-c("ab","ab","b","a","c",
                "a","ab","b","b","b",
                "ab","ab","ab","a","b",
                "ab","b","b","b","a")
                
fig3b<-ggplot(Areasum21, aes(x=Tile_area, y = total,fill= Tile_area)) +
  geom_bar(stat = "identity") +
  xlab("Tile area")+
  facet_grid(cols=vars(Material),rows=vars(Orientation),scales="free_x")+
  ylab(expression(paste("Recruit density per 100cm"^{2})))+
  geom_errorbar(aes(ymin = total-CI, ymax = total+CI), width = 0.2)+
  theme(panel.background = element_rect(fill = "white"),legend.position = "none",
        plot.title = element_textbox(hjust = 0.5, margin = margin(t = 5, b = 5)))+
  scale_fill_manual(values=col4)+
  geom_text(aes(label = KW,y=35 ), colour="gray11", size=4)+
  ggtitle("New recruit density (21 mo)")

fig3<-ggarrange(fig3a,fig3b)
ggsave("Output/Fig3.svg",fig3,width = 8, height = 4)

##############################################""

#Fig 5
#Fig 5a: PCA
col1<-c("#279AB7","#B74427")
Tiles9<-Tiles %>% filter(Age=="9 months")
pca1<-PCA(Tiles9[,c(3:11)])
pca.vars <- pca1$var$coord %>% data.frame
pca.vars$vars <- rownames(pca.vars)
pca.vars.m <- melt(pca.vars, id.vars = "vars")
Tiles9$pc1 <- pca1$ind$coord[, 1]
Tiles9$pc2 <- pca1$ind$coord[, 2]

fig5a<-ggplot(data = Tiles9, aes(x = pc1, y = pc2,color=Material,shape=Orientation)) +
  geom_point()+
  scale_color_manual(values=col1)+
  geom_segment(data = pca.vars, inherit.aes =F, aes(x = 0, xend = Dim.1*4.5, y = 0, 
                                                    yend = Dim.2*4.5),
               arrow = arrow(length = unit(0.025, "npc"), type = "open"), 
               lwd = 0.5)+
  stat_ellipse( aes(fill = Material), 
                show.legend = FALSE, 
                level = 0.95)+
  geom_text(data = pca.vars, inherit.aes =F,
            aes(x = Dim.1*5, y =  Dim.2*5, 
                label = c("  Bryozoa", "Bare", "CCA", "Dead CCA", "ERA", 
                          "Lobophora", "Other algae","Other fauna", "Sponge")), 
            check_overlap = F, size = 3) +
  labs(x="PCA1 (25.17% variance)",y="PCA2 (18.54% variance)")+
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.9) +
  theme_minimal()+theme(legend.position="right")
fig5a
ggsave("Output/Fig5A.svg",fig5a,width = 6, height = 3)

#Fig 5b: benthos
bentcolors<-c("Bare substrate"="#3A9E97",
              "Bryozoa"="#6A74A3",
              "Dead CCA"="#E4E8F5",
              "ERA"="#BA343DCC",
              "Other fauna"="#EDC285",
              "Live CCA"="#F55839CC",
              "Other algae"="#8AB37C",
              "Lobophora"="#590F0DB3",
              "Sponge"="#E8DC36C3")
Tiles<-read_xlsx("Data/Tiles.xlsx")
benthos<-gather(Tiles,"species","percent",3:11)
bentsum<-ddply(benthos,c("Age","Abb","species","Orientation"),summarise,sum=mean(percent))
bentsum$Abb<-factor(bentsum$Abb,c("FPVC","TPVC","FPC","TPC"))
bentsum$Age<-factor(bentsum$Age,c("9 months","21 months"))
bentsum$Orientation<-factor(bentsum$Orientation,c("Upper side","Lower side"))

fig5b<-ggplot(bentsum, aes(x=Abb, y=sum, fill=species)) +
  geom_bar(stat="identity", width=0.9) +
  facet_grid(Orientation~Age)+
  scale_fill_manual(values=bentcolors,
                    labels = c("Bare substrate",
                               "Bryozoa",
                               "Dead CCA",
                               "ERA",
                               "Live CCA",
                               expression(italic("Lobophora")),
                               "Other algae",
                               "Other fauna",
                               "Sponge"))+
  labs(fill="Epibenthos classes", x=NULL,y="percent of surface")+
  theme(panel.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = 90),legend.text.align = 0)
fig5b
ggsave("Output/Fig5b.svg",fig5b,width = 5, height = 4.5)

######################

#Fig 6
Tileslong<-gather(Tiles,"species","percent",3:11)
Correlation<-ddply(Tileslong,c("Year","species","Orientation"),summarise,
                   cor=cor(percent,Density,method="spearman"),
                   pval=cor.test(percent,Density,method="spearman")$p.value)
Correlation$title<-paste(Correlation$Year," recruitment")

Tiles22<-Tiles %>% filter(Year==2022 & Photo %in% Survival$Photo)
Tiles22$survival<-Survival$survival
Survivallong<-gather(Tiles22,"species","percent",3:11)
Corrsurv<-ddply(Survivallong,c("Year","species","Orientation"),summarise,
                cor=cor(percent,survival,method="spearman"),
                pval=cor.test(percent,survival,method="spearman")$p.value)
Corrsurv$title<-"Recruit survival"

Totalcor<-rbind(Correlation,Corrsurv)
Totalcor$pval[Totalcor$pval<0.05]<-"*"
Totalcor$pval[Totalcor$pval>0.05]<-""
Totalcor$Orientation<-factor(Totalcor$Orientation,c("Upper side","Lower side"))

fig6<-ggplot(na.omit(Totalcor))+
  geom_point(aes(x=cor,y=reorder(species, desc(species)),color=species),size=5)+
  scale_color_manual(values=bentcolors)+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none",axis.title.y=element_blank(),
        axis.line.x = element_line(color="black"))+
  geom_vline(xintercept=0,linetype="dotted")+
  facet_grid(cols=vars(title), rows=vars(Orientation),scales = 'free')+
  geom_text(aes(label = pval,x=cor,y=species),size=8)+
  xlab("Spearman's correlation")+
  scale_x_continuous(breaks=c(-0.3,0,0.3,0.6),limits=c(-0.5,0.7))

ggsave("Output/fig6.svg",fig6,width = 8, height = 4)

##############

#Supplementary tables

#Table S1

mod1<-lmer(Density~Year+Orientation*Material*Texture
           +(1|Unit)
           ,data=Tiles9)
Anova(mod1)
check_model(mod1)

mod1<-lmer(Density~Orientation*Material*Texture
           +(1|Unit)
           ,data=Tiles22)
Anova(mod1)
check_model(mod1)

#############################"

#Table S2
#recruit size
model<-Recruits_9m %>% filter(Orientation=="Lower side")
mod1<-glmer(Polyps~Year+Material*Texture
            +(1|unit)
            ,data=model,family=nbinom2)
Anova(mod1)

model<-Recruits_9m %>% filter(Material=="Porous concrete")
mod1<-glmer(Polyps~Year+Orientation*Texture
            +(1|unit)
            ,data=model,family=nbinom2)
Anova(mod1)

model<-Recruits_9m %>% filter(Texture=="Textured")
mod1<-glmer(Polyps~Year+Orientation*Material
            +(1|unit)
            ,data=model,family=nbinom2)
Anova(mod1)

#survival
model<-Survival %>% filter(Orientation=="Lower side")
mod1 <- glmer(survival~Material*Texture+(1|Unit)
              ,data=model,family=nbinom2)
Anova(mod1)

model<-Survival %>% filter(Material=="Porous concrete")
mod1<-glmer(survival~Orientation*Texture
              +(1|Unit)
              ,data=model,family=nbinom2)
Anova(mod1)

model<-Survival %>% filter(Texture=="Textured")
mod1<-glmer(survival~Orientation*Material
            +(1|Unit)
            ,data=model,family=nbinom2)
Anova(mod1)

#Growth
model<-Growth %>% filter(Orientation=="Lower side")
mod1 <- lmer(factor~Texture*Material+ (1|unit)
              ,data=model)
Anova(mod1)

model<-Growth %>% filter(Material=="Porous concrete")
mod1 <- glmer(factor~Texture*Orientation+(1|unit)
             ,data=model,family=nbinom2)
Anova(mod1)

model<-Growth %>% filter(Texture=="Textured")
mod1 <- glmer(factor~Material*Orientation+(1|unit)
              ,data=model,family=nbinom2)
Anova(mod1)

#####################

#Table S3

mod1<-glmer(Density~Year+Orientation*Material*Tile_area+(1|Unit)
            ,data=Tileareas9,family=nbinom2)
Anova(mod1,type="III")

mod1<-glmer(Density~Orientation*Material*Tile_area
            +(1|Unit)
            ,data=Tileareas21,family=nbinom2)
Anova(mod1,type = "III")

#Table S4
bentsp<-mvabund(Tiles9[, 3:11]*0.49)
mod1 <- manyglm(bentsp ~ Tiles9$Orientation*Tiles9$Material*Tiles9$Texture, family = "negative_binomial")
anov<-anova(mod1)
aov<-as.data.frame(anov$table)
rownames(aov)->aov$variable
write_xlsx(aov,"Output/fullanova.xlsx")

#Table S5
Benthos<-Tiles %>% filter(Year!=2020)
bentev<-mvabund(Benthos[, 3:11]*0.49)
mod1 <- manyglm(bentev ~ Benthos$Age*Benthos$Orientation*Benthos$Material*Benthos$Texture, family = "negative_binomial")
anov<-anova(mod1)
aov<-as.data.frame(anov$table)
rownames(aov)->aov$variable
write_xlsx(aov,"Output/Benthicevolution.xlsx")

#Table S6
Totalcor<-rbind(Correlation,Corrsurv)