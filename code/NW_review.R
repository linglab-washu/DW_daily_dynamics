library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)
library(gghalves)
library(ape)
library(vegan)
library(caret)
library(otuSummary)
library(ggpubr)
library(jsonlite)
library(httr)
library(ggmap)
library(ecodist)
load("/home/lin/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/.RData")
home071221_meta<-read.csv(file = "~/LinsData/2021JunePilotHomeWater/metadata071221_home_merge.csv", header = T)
home071221_meta$site_type<-paste(home071221_meta$site, home071221_meta$type)
home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat<-merge(home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat, home071221_meta[,c(1,7,11)], by="SampleID")
colnames(home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat)[c(1,2,4,5)]<-c("s1", "SampleID", "volume1", "site_type1")
home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat<-merge(home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat, home071221_meta[,c(1,7,11)], by="SampleID")
colnames(home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat)[c(1,6,7)]<-c("s2", "volume2", "site_type2")
home071221_bc<-home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat[home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat$site_type1==home071221_merge_new_nomito_nochloro_noeuk_nounassign_rarefied_nomeansample.vegdist.bray_longformat$site_type2,]
home071221_bc <- home071221_bc %>%
  mutate(category = case_when(
    volume1 != "third" & volume2 != "third" & volume1 != volume2 ~ "First vs second liter",
    volume1 != "second" & volume2 != "second" & volume1 != volume2 ~ "First vs third liter",
    volume1 != "first" & volume2 != "first" & volume1 != volume2 ~ "Second vs third liter",
    volume1 == "first" & volume2 == "first" ~ "Within first liter variation",
    volume1 == "second" & volume2 == "second" ~ "Within second liter variation",
    volume1 == "third" & volume2 == "third" ~ "Within third liter variation"
  ))
home071221_bc$category<-factor(home071221_bc$category, levels = c("First vs second liter", "First vs third liter", "Second vs third liter", "Within first liter variation", "Within second liter variation", "Within third liter variation"))
home071221_bc$site_type1<-factor(home071221_bc$site_type1, levels = c("bath stagnant", "kitchen stagnant", "kitchen fresh"))
home071221_bc_plot<-ggplot(home071221_bc, aes(x=category, y=bc)) +
  geom_half_boxplot(nudge = 0.1) + geom_jitter(size=1, show.legend = FALSE, position = position_jitter(width=.1, height=0)) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'))+ labs(y="Bray-Curtis distance", x=NULL)+
  theme(legend.text = element_text(size=12), axis.text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) + facet_wrap(~site_type1) 
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/Task6_home071221_bc_plot.png",plot = home071221_bc_plot,device = "png", width = 12.5, height = 5)



#####mpa4
custom_labels_phylum <- function(label) {
  if (label == "Other phyla") {
    return("Other phyla")
  } else {
    return(as.expression(bquote(italic(.(label)))))
  }
}
shower_mpa_phylum0.1<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa4_phylum0.1.csv", header = T, sep = ",")
shower_mpa_phylum0.1_longformat <- shower_mpa_phylum0.1 %>% gather(key= "sampleinfo",value="relabun", Home.1.day.1:Home.5.day.7)
shower_mpa_phylum0.1_longformat$home<-substr(shower_mpa_phylum0.1_longformat$sampleinfo, 1, 6)
shower_mpa_phylum0.1_longformat$day<-substr(shower_mpa_phylum0.1_longformat$sampleinfo, 12, 12)
shower_mpa_phylum0.1_longformat <- shower_mpa_phylum0.1_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
shower_mpa_phylum0.1_longformat$clade_name<-factor(shower_mpa_phylum0.1_longformat$clade_name, levels = rev(shower_mpa_phylum0.1$clade_name))
custom_color7<-c("#808080", "#214d4e", "#87d6bc", "#4007d9", "#d5d2e7", "#941f73", "#7d8ece", "#01c472")
shower_mpa_phylum0.1_plot<-ggplot() +geom_area(aes(x=as.numeric(day), y=relabun,fill=clade_name), data = shower_mpa_phylum0.1_longformat, position = 'stack') + 
  facet_wrap(~home,nrow = 1)+theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  guides(fill=guide_legend(title="Phylum",title.theme = element_text(size = 15, colour = "black")))+
  scale_fill_manual(values = color_custom7, labels = sapply(levels(shower_mpa_phylum0.1_longformat$clade_name), custom_labels_phylum)) + 
  theme(legend.text = element_text(size=15, colour = "black"), axis.text = element_text(size = 15, colour = "black"),axis.title=element_text(size=15, colour = "black"),strip.text = element_text(size = 15, colour = "black"), strip.background = element_rect(fill = 'white'))+
  scale_x_continuous(n.breaks = 7)+ylab("Relative abundance (%)")+xlab("Day of the sampling")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_phylum0.1_plot.png", shower_mpa_phylum0.1_plot, device = "png", width = 10, height = 5)

custom_labels_family <- function(label) {
  if (label == "Other families") {
    return(label)
  } else if (label == "unclassified Alphaproteobacteria taxon") {
    return(expression("unclassified"~italic("Alphaproteobacteria")~"taxon"))
  } else if (label == "unclassified Pelagibacterales taxon") {
    return(expression("unclassified"~italic("Pelagibacterales")~"taxon"))
  } else if (label == "Actinobacteria FGB1083") {
    return(expression(italic("Actinobacteria")~"FGB1083"))
  } else if (label == "Acidobacteria FGB13989") {
    return(expression(italic("Acidobacteria")~"FGB13989"))
  }
  else {
    return(as.expression(bquote(italic(.(label)))))
  }
}
shower_mpa_family2<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa4_family2.csv", header = T, sep = ",")
shower_mpa_family2_longformat <- shower_mpa_family2 %>% gather(key= "sampleinfo",value="relabun", Home.1.day.1:Home.5.day.7)
shower_mpa_family2_longformat$home<-substr(shower_mpa_family2_longformat$sampleinfo, 1, 6)
shower_mpa_family2_longformat$day<-substr(shower_mpa_family2_longformat$sampleinfo, 12, 12)
shower_mpa_family2_longformat <- shower_mpa_family2_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
shower_mpa_family2_longformat$clade_name<-factor(shower_mpa_family2_longformat$clade_name, levels = rev(shower_mpa_family2$clade_name))
shower_mpa_family2_plot<-ggplot() + geom_area(aes(x=as.numeric(day), y=relabun,fill=clade_name), data = shower_mpa_family2_longformat, position = 'stack') + 
  facet_wrap(~home,nrow = 1)+theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  guides(fill=guide_legend(title="Family",title.theme = element_text(size = 15, colour = "black")))+
  scale_fill_manual(values = c("#808080","#214d4e", "#87d6bc", "#148fae", "#1e438d", "#ec9fe7", "#4007d9", "#d5d2e7", "#941f73", "#7d8ece", "#7771ff", "#01c472"), 
                    labels = sapply(levels(shower_mpa_family2_longformat$clade_name), custom_labels_family))+
  theme(legend.text = element_text(size=15, colour = "black"), axis.text = element_text(size = 15, colour = "black"),axis.title=element_text(size=15, colour = "black"),strip.text = element_text(size = 15, colour = "black"), strip.background = element_rect(fill = 'white'))+
  scale_x_continuous(n.breaks = 7)+ylab("Relative abundance (%)")+xlab("Day of the sampling")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_family2_plot.png", shower_mpa_family2_plot, device = "png", width = 10, height = 4)

custom_labels_genus <- function(label) {
  if (label == "Other genera") {
    return(label)
  } else if (label == "unclassified Alphaproteobacteria taxon") {
    return(expression("unclassified"~italic("Alphaproteobacteria")~"taxon"))
  } else if (label == "Actinobacteria GGB24856") {
    return(expression(italic("Actinobacteria")~"GGB24856"))
  } else if (label == "Burkholderiaceae GGB26028") {
    return(expression(italic("Burkholderiaceae")~"GGB26028"))
  } else if (label == "Ca. Methylopumilus") {
    return(expression(italic("Ca.")~"Methylopumilus"))
  } else if (label == "Ca. Fonsibacter") {
    return(expression(italic("Ca.")~"Fonsibacter"))
  } else if (label == "Actinobacteria GGB36422") {
    return(expression(italic("Actinobacteria")~"GGB36422"))
  }
  else {
    return(as.expression(bquote(italic(.(label)))))
  }
}
shower_mpa_genus2<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa4_genus2.csv", header = T, sep = ",")
shower_mpa_genus2_longformat <- shower_mpa_genus2 %>% gather(key= "sampleinfo",value="relabun", Home.1.day.1:Home.5.day.7)
shower_mpa_genus2_longformat$home<-substr(shower_mpa_genus2_longformat$sampleinfo, 1, 6)
shower_mpa_genus2_longformat$day<-substr(shower_mpa_genus2_longformat$sampleinfo, 12, 12)
shower_mpa_genus2_longformat <- shower_mpa_genus2_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
shower_mpa_genus2_longformat$clade_name<-factor(shower_mpa_genus2_longformat$clade_name, levels = rev(shower_mpa_genus2$clade_name))
shower_mpa_genus2_plot<-ggplot() +geom_area(aes(x=as.numeric(day), y=relabun,fill=clade_name), data = shower_mpa_genus2_longformat, position = 'stack') + 
  facet_wrap(~home,nrow = 1)+theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  guides(fill=guide_legend(title="Genus",title.theme = element_text(size = 15, colour = "black")))+
  scale_fill_manual(values = c("#808080","#214d4e", "#87d6bc", "#148fae", "#1e438d", "#ec9fe7", "#4007d9", "#d5d2e7", "#941f73", "#7d8ece", "#7771ff", "#01c472"),
                    labels = sapply(levels(shower_mpa_genus2_longformat$clade_name), custom_labels_genus))+ 
  theme(legend.text = element_text(size=15, colour = "black"), axis.text = element_text(size = 15, colour = "black"),axis.title=element_text(size=15, colour = "black"),strip.text = element_text(size = 15, colour = "black"), strip.background = element_rect(fill = 'white'))+
  scale_x_continuous(n.breaks = 7)+ylab("Relative abundance (%)")+xlab("Day of the sampling")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_genus2_plot.png", shower_mpa_genus2_plot, device = "png", width = 10, height = 5)

custom_labels_species <- function(label) {
  if (label == "Other species") {
    return(label)
  } else if (label == "Actinobacteria GGB24856 SGB81948") {
    return(expression(italic("Actinobacteria")~"GGB24856 SGB81948"))
  } else if (label == "Burkholderiaceae GGB26028 SGB38032") {
    return(expression(italic("Burkholderiaceae")~"GGB26028 SGB38032"))
  } else if (label == "Actinobacteria GGB36422 SGB61312") {
    return(expression(italic("Actinobacteria")~"GGB36422 SGB61312"))
  } else if (label == "Ca. Methylopumilus universalis") {
    return(expression(italic("Ca.")~"Methylopumilus universalis"))
  } else if (label == "Ca. Fonsibacter ubiquis") {
    return(expression(italic("Ca.")~"Fonsibacter ubiquis"))
  } 
  else {
    return(as.expression(bquote(italic(.(label)))))
  }
}
shower_mpa_species2<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa4_species2.csv", header = T, sep = ",")
shower_mpa_species2_longformat <- shower_mpa_species2 %>% gather(key= "sampleinfo",value="relabun", Home.1.day.1:Home.5.day.7)
shower_mpa_species2_longformat$home<-substr(shower_mpa_species2_longformat$sampleinfo, 1, 6)
shower_mpa_species2_longformat$day<-substr(shower_mpa_species2_longformat$sampleinfo, 12, 12)
shower_mpa_species2_longformat <- shower_mpa_species2_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
shower_mpa_species2_longformat$clade_name<-factor(shower_mpa_species2_longformat$clade_name, levels = rev(shower_mpa_species2$clade_name))
shower_mpa_species2_plot<-ggplot() +geom_area(aes(x=as.numeric(day), y=relabun,fill=clade_name), data = shower_mpa_species2_longformat, position = 'stack') + 
  facet_wrap(~home,nrow = 1)+theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  guides(fill=guide_legend(title="Species",title.theme = element_text(size = 15, colour = "black")))+
  scale_fill_manual(values = c("#808080","#214d4e", "#87d6bc", "#148fae", "#1e438d", "#ec9fe7", "#4007d9", "#d5d2e7", "#941f73", "#7d8ece", "#7771ff", "#01c472"),
                    labels = sapply(levels(shower_mpa_species2_longformat$clade_name), custom_labels_species))+ 
  theme(legend.text = element_text(size=15, colour = "black"), axis.text = element_text(size = 15, colour = "black"),axis.title=element_text(size=15, colour = "black"),strip.text = element_text(size = 15, colour = "black"), strip.background = element_rect(fill = 'white'))+
  scale_x_continuous(n.breaks = 7)+ylab("Relative abundance (%)")+xlab("Day of the sampling")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_species2_plot1.png", shower_mpa_species2_plot, device = "png", width = 13, height = 5)

mpa_arranged_areaplot<-ggarrange(shower_mpa_phylum0.1_plot+theme(axis.ticks.x = element_blank(),axis.title.x = element_blank(),axis.text.x = element_blank(),axis.title.y = element_blank(), legend.justification = "left", plot.margin = margin(0.1,8,0.1,8, "mm")),
                                 shower_mpa_family2_plot+theme(strip.text = element_blank(), axis.text.x = element_blank(),axis.title.x = element_blank(),axis.ticks.x = element_blank(), axis.title.y = element_blank(), legend.justification = "left", plot.margin = margin(0.1,8,0.1,8, "mm")),
                                 shower_mpa_genus2_plot+theme(strip.text = element_blank(), axis.title.x = element_blank(),axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(),legend.justification = "left", plot.margin = margin(0.1,8,0.1,8, "mm")), 
                                 shower_mpa_species2_plot+theme(strip.text = element_blank(), axis.title.y = element_blank(), legend.justification = "left", plot.margin = margin(0.1,8,0.1,8, "mm")), nrow = 4,align = "v")
mpa_arranged_areaplot_final <- annotate_figure(mpa_arranged_areaplot, left = text_grob("Relative abundance (%)", rot = 90, vjust = 1, size = 15))
ggsave("~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/mpa_area_v3.png", plot = mpa_arranged_areaplot_final, device = "png", width = 16.5, height = 14)
ggsave("~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/mpa_area_v3.pdf", plot = mpa_arranged_areaplot_final, device = "pdf", width = 16.5, height = 14)
postscript("~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/mpa_area_v3.eps", width = 16.5, height = 14, paper = "special", horizontal = FALSE)
annotate_figure(mpa_arranged_areaplot, left = text_grob("Relative abundance (%)", rot = 90, vjust = 1, size = 15))
dev.off()


######mpa pcoa
shower_mpa_species<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa4_species.txt", header = T, sep = "\t")
shower_mpa_species_forbc<-as.data.frame(shower_mpa_species[,2:57])
shower_mpa_species_forbc <- shower_mpa_species_forbc[, order(names(shower_mpa_species_forbc))]
shower_mpa_species_forbc_t$year<-c(rep(2019,7), rep(2019,7), rep(2018,7), rep(2018,7), rep(2019,7), rep(2019,7), rep(2018,7), rep(2019,7))
shower_mpa_species_forbc_t$year<-as.factor(shower_mpa_species_forbc_t$year)
adonis2(vegdist(shower_mpa_species_forbc_t[,1:564]) ~ home, data = shower_mpa_species_forbc_t)
adonis2(vegdist(shower_mpa_species_forbc_t[,1:564]) ~ year, data = shower_mpa_species_forbc_t)
permutest(betadisper(vegdist(shower_mpa_species_forbc_t[,1:564]),group = shower_mpa_species_forbc_t$home, type = c("centroid")))
permutest(betadisper(vegdist(shower_mpa_species_forbc_t[,1:564]),group = shower_mpa_species_forbc_t$year, type = c("centroid")))
source("~/R/pairwise.adonis.R")
write.csv(as.data.frame(pairwise.adonis(shower_mpa_species_forbc_t[,1:564], shower_mpa_species_forbc_t$home, sim.method = 'bray', p.adjust.m = "BH")),file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/mpa_species_pairwise_bray_permanova_BH.csv")
shower_mpa_species_forbc_bray<-vegdist(t(shower_mpa_species_forbc), method = "bray")
shower_mpa_species_forbc_bray_pcoa<-pcoa(shower_mpa_species_forbc_bray)
shower_mpa_species_forbc_bray_pcoa_data.scores = as.data.frame(shower_mpa_species_forbc_bray_pcoa$vectors[,1:2])
shower_mpa_species_forbc_bray_pcoa_data.scores$home<-substr(rownames(shower_mpa_species_forbc_bray_pcoa_data.scores),1,6)
shower_mpa_species_forbc_bray_pcoa_data.scores <- shower_mpa_species_forbc_bray_pcoa_data.scores %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
shower_mpa_species_forbc_bray_pcoa_data.scores$year<-c(rep(2019,7), rep(2019,7), rep(2018,7), rep(2018,7), rep(2019,7), rep(2019,7), rep(2018,7), rep(2019,7))
shower_mpa_species_forbc_bray_pcoa_data.scores$year<-as.factor(shower_mpa_species_forbc_bray_pcoa_data.scores$year)
shower_mpa_species_forbc_bray_pcoa_data.scores <- shower_mpa_species_forbc_bray_pcoa_data.scores %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
sort(shower_mpa_species_forbc_bray_pcoa$values$Relative_eig)
shower_mpa_species_bc_pcoa_plot<-ggplot(shower_mpa_species_forbc_bray_pcoa_data.scores, aes(x = Axis.1, y = Axis.2)) +
  geom_point(size=5, aes(color=home)) +
  theme(axis.text = element_text(colour = "black", size = 11, family = "Arial")) +
  theme(legend.text = element_text(colour = "black", size = 11, family = "Arial"), legend.position = "left")+
  theme(axis.title = element_text(colour = "black", size = 11, family = "Arial"))+
  theme(panel.background = element_rect(fill='white', colour = 'black'), panel.border = element_rect(fill = NA, colour = 'black'))+
  theme(legend.title = element_text(colour = "black", size = 11, family = "Arial"))+
  labs(x="Axis.1 [31.5%]", y="Axis.2 [19%]")+
  scale_color_manual(values = color_custom)+guides(color=guide_legend(title="Household\nidentity"))

shower_mpa_species_bc_pcoa_plot<-ggplot(shower_mpa_species_forbc_bray_pcoa_data.scores, aes(x = Axis.1, y = Axis.2)) +
  geom_point(size=5, aes(color=home)) +
  theme(axis.text = element_text(colour = "black", size = 11)) +
  theme(legend.text = element_text(colour = "black", size = 11), legend.position = "left")+
  theme(axis.title = element_text(colour = "black", size = 11))+
  theme(panel.background = element_rect(fill='white', colour = 'black'), panel.border = element_rect(fill = NA, colour = 'black'))+
  theme(legend.title = element_text(colour = "black", size = 11))+
  labs(x="Axis.1 [31.5%]", y="Axis.2 [19%]")+
  scale_color_manual(values = color_custom)+guides(color=guide_legend(title="Household\nidentity"))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_species_bc_pcoa3.pdf",plot =shower_mpa_species_bc_pcoa_plot,device = "pdf", width = 6, height = 4.5)

shower_mpa_species_bc_pcoa_plot_year<-ggplot(shower_mpa_species_forbc_bray_pcoa_data.scores, aes(x = Axis.1, y = Axis.2)) +
  geom_point(size=5, aes(color=year)) +
  theme(axis.text = element_text(colour = "black", size = 12, face = "bold")) +
  theme(legend.text = element_text(colour = "black", size = 15, face = "bold"), legend.position = "top")+
  theme(axis.title = element_text(colour = "black", size = 15, face = "bold"))+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(legend.title = element_text(colour = "black", size = 15, face = "bold"))+
  labs(x="Axis.1 [31.5%]", y="Axis.2 [19%]")+
  guides(color=guide_legend(title="Sampling year"))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_species_bc_pcoa_year.png",plot =shower_mpa_species_bc_pcoa_plot_year,device = "png", width = 6.5, height = 6)

shower_mpa_species_forbc_t<-as.data.frame(t(shower_mpa_species_forbc))
shower_mpa_species_forbc_t$home<-substr(rownames(shower_mpa_species_forbc_t),1,6)
shower_mpa_species_forbc_jaccard<-vegdist(shower_mpa_species_forbc_t[,1:564], method = "jaccard", binary = TRUE)
adonis2(shower_mpa_species_forbc_jaccard ~ home, data = shower_mpa_species_forbc_t)
shower_mpa_species_forbc_jaccard_pcoa<-pcoa(shower_mpa_species_forbc_jaccard)
shower_mpa_species_forbc_jaccard_pcoa_data.scores = as.data.frame(shower_mpa_species_forbc_jaccard_pcoa$vectors[,1:2])
shower_mpa_species_forbc_jaccard_pcoa_data.scores$home<-substr(rownames(shower_mpa_species_forbc_jaccard_pcoa_data.scores),1,6)
shower_mpa_species_forbc_jaccard_pcoa_data.scores <- shower_mpa_species_forbc_jaccard_pcoa_data.scores %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
sort(shower_mpa_species_forbc_jaccard_pcoa$values$Relative_eig)
shower_mpa_species_jaccard_pcoa_plot<-ggplot(shower_mpa_species_forbc_jaccard_pcoa_data.scores, aes(x = Axis.1, y = Axis.2)) +
  geom_point(size=5, aes(color=home)) +
  theme(axis.text = element_text(colour = "black", size = 12, face = "bold")) +
  theme(legend.text = element_text(colour = "black", size = 15, face = "bold"), legend.position = "top")+
  theme(axis.title = element_text(colour = "black", size = 15, face = "bold"))+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(legend.title = element_text(colour = "black", size = 15, face = "bold"))+
  labs(x="Axis.1 [31.2%]", y="Axis.2 [19.8%]")+
  scale_color_manual(values = color_custom)+guides(color=guide_legend(title="Household\nidentity"))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_species_jaccard_pcoa.png",plot =shower_mpa_species_jaccard_pcoa_plot,device = "png", width = 6.5, height = 6)


######mpa lasso
set.seed(1234)
shower_mpa_species_0.1to0 <- shower_mpa_species[,2:57] %>%
  mutate(across(everything(), ~ ifelse(. < 0.1, 0, .)))
rownames(shower_mpa_species_0.1to0)<-shower_mpa_species$clade_name
shower_mpa_species_0.1to0 <- shower_mpa_species_0.1to0[rowSums(shower_mpa_species_0.1to0) != 0, ]
shower_mpa_species_0.1to0<-shower_mpa_species_0.1to0[,order(names(shower_mpa_species_0.1to0))]
shower_mpa_species_0.1to0_t<-t(shower_mpa_species_0.1to0)
home1vsall_mpa<-as.data.frame(shower_mpa_species_0.1to0_t)
home2vsall_mpa<-as.data.frame(shower_mpa_species_0.1to0_t)
home3vsall_mpa<-as.data.frame(shower_mpa_species_0.1to0_t)
home4vsall_mpa<-as.data.frame(shower_mpa_species_0.1to0_t)
home5vsall_mpa<-as.data.frame(shower_mpa_species_0.1to0_t)
home6vsall_mpa<-as.data.frame(shower_mpa_species_0.1to0_t)
home7vsall_mpa<-as.data.frame(shower_mpa_species_0.1to0_t)
home8vsall_mpa<-as.data.frame(shower_mpa_species_0.1to0_t)
home1vsall_mpa$home<-c(rep(1,7),rep(0,49))
home2vsall_mpa$home<-c(rep(0,7),rep(1,7),rep(0,42))
home3vsall_mpa$home<-c(rep(0,14),rep(1,7),rep(0,35))
home4vsall_mpa$home<-c(rep(0,21),rep(1,7),rep(0,28))
home5vsall_mpa$home<-c(rep(0,28),rep(1,7),rep(0,21))
home6vsall_mpa$home<-c(rep(0,35),rep(1,7),rep(0,14))
home7vsall_mpa$home<-c(rep(0,42),rep(1,7),rep(0,7))
home8vsall_mpa$home<-c(rep(0,49),rep(1,7))
home1vsall_mpa$home<-as.factor(home1vsall_mpa$home)
home2vsall_mpa$home<-as.factor(home2vsall_mpa$home)
home3vsall_mpa$home<-as.factor(home3vsall_mpa$home)
home4vsall_mpa$home<-as.factor(home4vsall_mpa$home)
home5vsall_mpa$home<-as.factor(home5vsall_mpa$home)
home6vsall_mpa$home<-as.factor(home6vsall_mpa$home)
home7vsall_mpa$home<-as.factor(home7vsall_mpa$home)
home8vsall_mpa$home<-as.factor(home8vsall_mpa$home)

parameters <- 10^seq(-3, 3, length = 100)
train_control <- trainControl(method="LOOCV")

trainindex_home1 <- createDataPartition(home1vsall_mpa$home, p = .8, list = FALSE, times = 1)
train_home1 <- home1vsall_mpa[trainindex_home1,]
test_home1 <- home1vsall_mpa[-trainindex_home1,]
lasso_home1vsall<-train(x=train_home1[,1:176], y=train_home1$home, method = "glmnet", tuneGrid = expand.grid(alpha=1, lambda=parameters),trControl = train_control)
lassoAreaTypeTrain_home1vsall <- predict(lasso_home1vsall, newdata = train_home1)
lassoAreaTypeConfusionMatrixTrain_lasso_home1 <- confusionMatrix(data = lassoAreaTypeTrain_home1vsall, train_home1$home)
lassoAreaTypeTest_home1vsall <- predict(lasso_home1vsall, newdata = test_home1)
lassoAreaTypeConfusionMatrixTest_lasso_home1 <- confusionMatrix(data = lassoAreaTypeTest_home1vsall, test_home1$home)
coef_home1vsall<-coef(lasso_home1vsall$finalModel, lasso_home1vsall$bestTune$lambda)
lasso_home1 <- data.frame(species = coef_home1vsall@Dimnames[[1]][coef_home1vsall@i + 1])
lasso_home1 <- subset(lasso_home1, lasso_home1$species!="(Intercept)")
lasso_home1$home<-rep("STL-1", nrow(lasso_home1))

trainindex_home2 <- createDataPartition(home2vsall_mpa$home, p = .8, list = FALSE, times = 1)
train_home2 <- home2vsall_mpa[trainindex_home2,]
test_home2 <- home2vsall_mpa[-trainindex_home2,]
lasso_home2vsall<-train(x=train_home2[,1:176], y=train_home2$home, method = "glmnet", tuneGrid = expand.grid(alpha=1, lambda=parameters),trControl = train_control)
lassoAreaTypeTrain_home2vsall <- predict(lasso_home2vsall, newdata = train_home2)
lassoAreaTypeConfusionMatrixTrain_lasso_home2 <- confusionMatrix(data = lassoAreaTypeTrain_home2vsall, train_home2$home)
lassoAreaTypeTest_home2vsall <- predict(lasso_home2vsall, newdata = test_home2)
lassoAreaTypeConfusionMatrixTest_lasso_home2 <- confusionMatrix(data = lassoAreaTypeTest_home2vsall, test_home2$home)
coef_home2vsall<-coef(lasso_home2vsall$finalModel, lasso_home2vsall$bestTune$lambda)
lasso_home2 <- data.frame(species = coef_home2vsall@Dimnames[[1]][coef_home2vsall@i + 1])
lasso_home2 <- subset(lasso_home2, lasso_home2$species!="(Intercept)")
lasso_home2$home<-rep("STL-2", nrow(lasso_home2))

trainindex_home3 <- createDataPartition(home3vsall_mpa$home, p = .8, list = FALSE, times = 1)
train_home3 <- home3vsall_mpa[trainindex_home3,]
test_home3 <- home3vsall_mpa[-trainindex_home3,]
lasso_home3vsall<-train(x=train_home3[,1:176], y=train_home3$home, method = "glmnet", tuneGrid = expand.grid(alpha=1, lambda=parameters),trControl = train_control)
lassoAreaTypeTrain_home3vsall <- predict(lasso_home3vsall, newdata = train_home3)
lassoAreaTypeConfusionMatrixTrain_lasso_home3 <- confusionMatrix(data = lassoAreaTypeTrain_home3vsall, train_home3$home)
lassoAreaTypeTest_home3vsall <- predict(lasso_home3vsall, newdata = test_home3)
lassoAreaTypeConfusionMatrixTest_lasso_home3 <- confusionMatrix(data = lassoAreaTypeTest_home3vsall, test_home3$home)
coef_home3vsall<-coef(lasso_home3vsall$finalModel, lasso_home3vsall$bestTune$lambda)
lasso_home3 <- data.frame(species = coef_home3vsall@Dimnames[[1]][coef_home3vsall@i + 1])
lasso_home3 <- subset(lasso_home3, lasso_home3$species!="(Intercept)")
lasso_home3$home<-rep("STL-3", nrow(lasso_home3))

trainindex_home4 <- createDataPartition(home4vsall_mpa$home, p = .8, list = FALSE, times = 1)
train_home4 <- home4vsall_mpa[trainindex_home4,]
test_home4 <- home4vsall_mpa[-trainindex_home4,]
lasso_home4vsall<-train(x=train_home4[,1:176], y=train_home4$home, method = "glmnet", tuneGrid = expand.grid(alpha=1, lambda=parameters),trControl = train_control)
lassoAreaTypeTrain_home4vsall <- predict(lasso_home4vsall, newdata = train_home4)
lassoAreaTypeConfusionMatrixTrain_lasso_home4 <- confusionMatrix(data = lassoAreaTypeTrain_home4vsall, train_home4$home)
lassoAreaTypeTest_home4vsall <- predict(lasso_home4vsall, newdata = test_home4)
lassoAreaTypeConfusionMatrixTest_lasso_home4 <- confusionMatrix(data = lassoAreaTypeTest_home4vsall, test_home4$home)
coef_home4vsall<-coef(lasso_home4vsall$finalModel, lasso_home4vsall$bestTune$lambda)
lasso_home4 <- data.frame(species = coef_home4vsall@Dimnames[[1]][coef_home4vsall@i + 1])
lasso_home4 <- subset(lasso_home4, lasso_home4$species!="(Intercept)")
lasso_home4$home<-rep("STL-4", nrow(lasso_home4))

trainindex_home5 <- createDataPartition(home5vsall_mpa$home, p = .8, list = FALSE, times = 1)
train_home5 <- home5vsall_mpa[trainindex_home5,]
test_home5 <- home5vsall_mpa[-trainindex_home5,]
lasso_home5vsall<-train(x=train_home5[,1:176], y=train_home5$home, method = "glmnet", tuneGrid = expand.grid(alpha=1, lambda=parameters),trControl = train_control)
lassoAreaTypeTrain_home5vsall <- predict(lasso_home5vsall, newdata = train_home5)
lassoAreaTypeConfusionMatrixTrain_lasso_home5 <- confusionMatrix(data = lassoAreaTypeTrain_home5vsall, train_home5$home)
lassoAreaTypeTest_home5vsall <- predict(lasso_home5vsall, newdata = test_home5)
lassoAreaTypeConfusionMatrixTest_lasso_home5 <- confusionMatrix(data = lassoAreaTypeTest_home5vsall, test_home5$home)
coef_home5vsall<-coef(lasso_home5vsall$finalModel, lasso_home5vsall$bestTune$lambda)
lasso_home5 <- data.frame(species = coef_home5vsall@Dimnames[[1]][coef_home5vsall@i + 1])
lasso_home5 <- subset(lasso_home5, lasso_home5$species!="(Intercept)")
lasso_home5$home<-rep("STL-5", nrow(lasso_home5))

trainindex_home6 <- createDataPartition(home6vsall_mpa$home, p = .8, list = FALSE, times = 1)
train_home6 <- home6vsall_mpa[trainindex_home6,]
test_home6 <- home6vsall_mpa[-trainindex_home6,]
lasso_home6vsall<-train(x=train_home6[,1:176], y=train_home6$home, method = "glmnet", tuneGrid = expand.grid(alpha=1, lambda=parameters),trControl = train_control)
lassoAreaTypeTrain_home6vsall <- predict(lasso_home6vsall, newdata = train_home6)
lassoAreaTypeConfusionMatrixTrain_lasso_home6 <- confusionMatrix(data = lassoAreaTypeTrain_home6vsall, train_home6$home)
lassoAreaTypeTest_home6vsall <- predict(lasso_home6vsall, newdata = test_home6)
lassoAreaTypeConfusionMatrixTest_lasso_home6 <- confusionMatrix(data = lassoAreaTypeTest_home6vsall, test_home6$home)
coef_home6vsall<-coef(lasso_home6vsall$finalModel, lasso_home6vsall$bestTune$lambda)
lasso_home6 <- data.frame(species = coef_home6vsall@Dimnames[[1]][coef_home6vsall@i + 1])
lasso_home6 <- subset(lasso_home6, lasso_home6$species!="(Intercept)")
lasso_home6$home<-rep("STL-6", nrow(lasso_home6))

trainindex_home7 <- createDataPartition(home7vsall_mpa$home, p = .8, list = FALSE, times = 1)
train_home7 <- home7vsall_mpa[trainindex_home7,]
test_home7 <- home7vsall_mpa[-trainindex_home7,]
lasso_home7vsall<-train(x=train_home7[,1:176], y=train_home7$home, method = "glmnet", tuneGrid = expand.grid(alpha=1, lambda=parameters),trControl = train_control)
lassoAreaTypeTrain_home7vsall <- predict(lasso_home7vsall, newdata = train_home7)
lassoAreaTypeConfusionMatrixTrain_lasso_home7 <- confusionMatrix(data = lassoAreaTypeTrain_home7vsall, train_home7$home)
lassoAreaTypeTest_home7vsall <- predict(lasso_home7vsall, newdata = test_home7)
lassoAreaTypeConfusionMatrixTest_lasso_home7 <- confusionMatrix(data = lassoAreaTypeTest_home7vsall, test_home7$home)
coef_home7vsall<-coef(lasso_home7vsall$finalModel, lasso_home7vsall$bestTune$lambda)
lasso_home7 <- data.frame(species = coef_home7vsall@Dimnames[[1]][coef_home7vsall@i + 1])
lasso_home7 <- subset(lasso_home7, lasso_home7$species!="(Intercept)")
lasso_home7$home<-rep("STL-7", nrow(lasso_home7))

trainindex_home8 <- createDataPartition(home8vsall_mpa$home, p = .8, list = FALSE, times = 1)
train_home8 <- home8vsall_mpa[trainindex_home8,]
test_home8 <- home8vsall_mpa[-trainindex_home8,]
lasso_home8vsall<-train(x=train_home8[,1:176], y=train_home8$home, method = "glmnet", tuneGrid = expand.grid(alpha=1, lambda=parameters),trControl = train_control)
lassoAreaTypeTrain_home8vsall <- predict(lasso_home8vsall, newdata = train_home8)
lassoAreaTypeConfusionMatrixTrain_lasso_home8 <- confusionMatrix(data = lassoAreaTypeTrain_home8vsall, train_home8$home)
lassoAreaTypeTest_home8vsall <- predict(lasso_home8vsall, newdata = test_home8)
lassoAreaTypeConfusionMatrixTest_lasso_home8 <- confusionMatrix(data = lassoAreaTypeTest_home8vsall, test_home8$home)
coef_home8vsall<-coef(lasso_home8vsall$finalModel, lasso_home8vsall$bestTune$lambda)
lasso_home8 <- data.frame(species = coef_home8vsall@Dimnames[[1]][coef_home8vsall@i + 1])
lasso_home8 <- subset(lasso_home8, lasso_home8$species!="(Intercept)")
lasso_home8$home<-rep("STL-8", nrow(lasso_home8))

lasso_feature_caret040524<-rbind(lasso_home1, lasso_home2, lasso_home3, lasso_home4, lasso_home5, lasso_home6, lasso_home7, lasso_home8)
write.csv(as.data.frame(lasso_feature_caret040124), file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/compiled_no0.001_lasso040124.csv")

shower_mpa_species_0.1to0$species<-rownames(shower_mpa_species_0.1to0)
test_merge <- merge(lasso_feature_caret040524, shower_mpa_species_0.1to0, by = "species")
write.csv(test_merge, file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/mpa_0.1to0_lasso040524.csv")
shower_lasso1<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/mpa_0.1to0_lasso040524_processed.csv", header = T)
shower_lasso1 <- shower_lasso1 %>% mutate(across('species', str_replace, 'Candidatus', 'Ca.'))
shower_lasso2 <- shower_lasso1 %>% gather(key= "sampleinfo",value="relabun", Home.1.day.1:Home.8.day.7)
shower_lasso2$species<-factor(shower_lasso2$species, levels = shower_lasso1$species)
shower_lasso2$home<-substr(shower_lasso2$sampleinfo, 1, 6)
shower_lasso2$day<-substr(shower_lasso2$sampleinfo, 12, 12)
shower_lasso2 <- shower_lasso2 %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
shower_lasso2$relabun1<-shower_lasso2$relabun
shower_lasso2$relabun1 <- ifelse(shower_lasso2$relabun1 < 0.001, NA, shower_lasso2$relabun1)
shower_lasso2$log_relabun<-log10(shower_lasso2$relabun1)
custom_labels_lasso <- function(label) {
  if (label == "Sphingomonadaceae GGB16269 SGB62104") {
    return(expression(italic("Sphingomonadaceae")~"GGB16269 SGB62104"))
  } else if (label == "Bacteroidota GGB46492 SGB64353") {
    return(expression(italic("Bacteroidota")~"GGB46492 SGB64353"))
  } else if (label == "Bacteroidota GGB43022 SGB60257") {
    return(expression(italic("Bacteroidota")~"GGB43022 SGB60257"))
  } else if (label == "Mycobacteriaceae GGB38478 SGB55612") {
    return(expression(italic("Mycobacteriaceae")~"GGB38478 SGB55612"))
  } else if (label == "Planctomycetota GGB35689 SGB85076") {
    return(expression(italic("Planctomycetota")~"GGB35689 SGB85076"))
  } else if (label == "Verrucomicrobia GGB43612 SGB60949") {
    return(expression(italic("Verrucomicrobia")~"GGB43612 SGB60949"))
  } else if (label == "Actinobacteria GGB64139 SGB86549") {
    return(expression(italic("Actinobacteria")~"GGB64139 SGB86549"))
  } else if (label == "Ca. Methylopumilus planktonicus") {
    return(expression(italic("Ca.")~"Methylopumilus planktonicus"))
  } else if (label == "Ca. Methylopumilus SGB64798") {
    return(expression(italic("Ca.")~"Methylopumilus SGB64798"))
  } else if (label == "Limnohabitans SGB60293") {
    return(expression(italic("Limnohabitans")~"SGB60293"))
  } else if (label == "Tenericutes GGB26692 SGB38873") {
    return(expression(italic("Tenericutes")~"GGB26692 SGB38873"))
  }
  else {
    return(as.expression(bquote(italic(.(label)))))
  }
}
shower_lasso_heatmap<-ggplot(shower_lasso2,aes(x=day, y=species))+geom_tile(aes(fill=log_relabun))+ labs(y="Species", x=NULL) + 
  scale_fill_continuous(limits=c(-1, 2), breaks=seq(-1,2), low='#bbdffb', high='#0747a1', na.value="white") +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) + 
  theme(axis.text = element_text(size=15, family = "Arial", colour = "black"), axis.title = element_text(size=15, family = "Arial", colour = "black"))+ 
  facet_wrap(~home, nrow = 1)+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  scale_y_discrete(labels = function(y) sapply(y, custom_labels_lasso)) +
  theme(legend.text = element_text(size=15, family = "Arial", colour = "black"),legend.title = element_text(size = 15, family = "Arial", colour = "black"),
        strip.text = element_text(size = 15, family = "Arial", colour = "black"), legend.position = "bottom")+
  guides(fill=guide_colorbar(title="Log transformed relative abundance", reverse = F))

shower_lasso_heatmap<-ggplot(shower_lasso2,aes(x=day, y=species))+geom_tile(aes(fill=log_relabun))+ labs(y="Species", x=NULL) + 
  scale_fill_continuous(limits=c(-1, 2), breaks=seq(-1,2), low='#bbdffb', high='#0747a1', na.value="white") +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) + 
  theme(axis.text = element_text(size=15, colour = "black"), axis.title = element_text(size=15, colour = "black"))+ 
  facet_wrap(~home, nrow = 1)+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  scale_y_discrete(labels = function(y) sapply(y, custom_labels_lasso)) +
  theme(legend.text = element_text(size=15, colour = "black"),legend.title = element_text(size = 15, colour = "black"),
        strip.text = element_text(size = 15, colour = "black"), legend.position = "bottom")+
  guides(fill=guide_colorbar(title="Log transformed relative abundance", reverse = F))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/lasso_mpa3.pdf",plot =shower_lasso_heatmap,device = "pdf", width = 10, height = 9)

######mpa op
op_mpa4<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa4_op_0.1to0.csv", header = T)
op_mpa4_longformat <- op_mpa4 %>% gather(key= "sampleinfo",value="relabun", Home.1.day.1:Home.8.day.7)
op_mpa4_longformat$home<-substr(op_mpa4_longformat$sampleinfo, 1, 6)
op_mpa4_longformat$day<-substr(op_mpa4_longformat$sampleinfo, 12, 12)
op_mpa4_longformat <- op_mpa4_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
op_mpa4_longformat$relabun1 <- ifelse(op_mpa4_longformat$relabun < 0.1, NA, op_mpa4_longformat$relabun)
op_mpa4_longformat$log_relabun<-log10(op_mpa4_longformat$relabun1)
op_mpa4_heatmap<-ggplot(op_mpa4_longformat,aes(x=day, y=species))+geom_tile(aes(fill=log_relabun))+ labs(y="Species", x=NULL) + 
  scale_fill_continuous(limits=c(-1, 2), breaks=seq(-1,2,by=0.5), low='#bbdffb', high='#0747a1', na.value="white") +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) + 
  theme(axis.text = element_text(size=15, family = "Arial", colour = "black"), axis.title = element_text(size=15, family = "Arial", colour = "black"))+ theme(axis.text.y = element_text(face = "italic"))+
  facet_wrap(~home, nrow = 1)+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  theme(legend.text = element_text(size=15, family = "Arial", colour = "black"),legend.title = element_text(size = 15, family = "Arial", colour = "black"),strip.text = element_text(size = 15, family = "Arial", colour = "black"))+
  guides(fill=guide_legend(title="Log transformed\nrelative abundance", reverse = T))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/op_mpa4_1.png",plot =op_mpa4_heatmap,device = "png", height = 6, width = 13)

######prevalence plot for mpa op
op_prevalence_across_homes <- op_mpa4_longformat %>%
  group_by(species) %>%
  summarise(op_prevalence_across_homes = n_distinct(home[!is.na(relabun1)]))
write.csv(op_prevalence_across_homes, file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/op_prevalence_across_homes.csv")
op_order_df<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/op_prevalence_across_homes.csv", header = T)
op_prevalence_across_homes$species<-factor(op_prevalence_across_homes$species, levels = rev(op_order_df$species))
op_prevalence_across_homes$formatted_species <- ifelse(
  op_prevalence_across_homes$species == "Mycobacterium sp YC RL4",
  "<i>Mycobacterium</i> sp YC RL4",  # Italicize only "Mycobacterium"
  paste0("<i>", op_prevalence_across_homes$species, "</i>")  # Italicize the full species name for other species
)
op_prevalence_across_homes$formatted_species <- factor(
  op_prevalence_across_homes$formatted_species,
  levels = op_prevalence_across_homes$formatted_species[order(op_prevalence_across_homes$species)]
)
op_prevalence_across_homes$pervalence<-op_prevalence_across_homes$op_prevalence_across_homes/8
op_prevalence_across_homes_plot<-ggplot(op_prevalence_across_homes, aes(x = pervalence, y = formatted_species)) + 
  geom_bar(stat = "identity") + 
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  xlab("Across-household prevalence") + ylab(NULL) + 
  theme(axis.text = element_text(size=15, family = "Arial", colour = "black"), axis.title = element_text(size=15, family = "Arial", colour = "black"))+ 
  theme(axis.text.y = element_markdown()) 
#####pdf saving doesn't work if set arial 
op_prevalence_across_homes_plot<-ggplot(op_prevalence_across_homes, aes(x = pervalence, y = formatted_species)) + 
  geom_bar(stat = "identity") + 
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  xlab("Across-household prevalence") + ylab(NULL) + 
  theme(axis.text = element_text(size=15, colour = "black"), axis.title = element_text(size=15, colour = "black"))+ 
  theme(axis.text.y = element_markdown()) 

op_prevalence_per_home <- op_mpa4_longformat %>%
  group_by(species, home) %>%
  summarise(prevalence_in_home = sum(!is.na(relabun1))) %>%
  ungroup()
op_prevalence_per_home$species<-factor(op_prevalence_per_home$species, levels = rev(op_order_df$species))
op_prevalence_per_home$prevalence<-op_prevalence_per_home$prevalence_in_home/7
op_prevalence_per_home$prevalence<-round(op_prevalence_per_home$prevalence, 2)
op_prevalence_per_home_plot<-ggplot(op_prevalence_per_home, aes(x = prevalence_in_home, y = species)) + 
  geom_bar(stat = "identity") + facet_wrap(~home, nrow = 1) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  xlab("Within-household prevalence") + ylab(NULL) + scale_x_continuous(breaks = 1:7) +
  theme(axis.text = element_text(size=15, family = "Arial", colour = "black"), axis.title = element_text(size=15, family = "Arial", colour = "black"))+ 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(strip.text = element_text(size=15, family = "Arial", colour = "black")) 
op_prevalence_per_home_plot1<-ggplot(op_prevalence_per_home, aes(x = home, y = species)) + geom_tile(aes(fill=prevalence_in_home))+
  scale_fill_continuous(name="Number of\ndays detected", limits=c(0, 7), breaks=seq(0,7,by=2), low="white", high='#0747a1') +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  xlab("Within-household prevalence") + ylab(NULL) + 
  theme(axis.text = element_text(size=15, family = "Arial", colour = "black"), axis.title = element_text(size=15, family = "Arial", colour = "black"))+ 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.text = element_text(size=15, family = "Arial", colour = "black"), legend.title = element_text(size=15, family = "Arial", colour = "black")) 
op_prevalence_per_home <- op_prevalence_per_home %>%
  mutate(prevalence1 = case_when(
    prevalence == "0" ~ "(0 day)",
    prevalence == "0.14" ~ "(1 day)",
    prevalence == "0.29" ~ "(2 days)",
    prevalence == "0.43" ~ "(3 days)",
    prevalence == "0.86" ~ "(6 days)",
    prevalence == "1" ~ "(7 days)"
  ))
op_prevalence_per_home$prevalence2<-factor(paste(op_prevalence_per_home$prevalence, op_prevalence_per_home$prevalence1))
op_prevalence_per_home_plot2<-ggplot(op_prevalence_per_home, aes(x = home, y = species)) + geom_tile(aes(fill=prevalence2))+
  scale_fill_manual(name="Prevalence", values = c("white",'#FEFBE9', '#FCF7D5', '#F5F3C1','#B5DDD8', '#9BD2E1')) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  xlab("Within-household prevalence") + ylab(NULL) + 
  theme(axis.text = element_text(size=15, family = "Arial", colour = "black"), axis.title = element_text(size=15, family = "Arial", colour = "black"))+ 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.text = element_text(size=15, family = "Arial", colour = "black"), legend.title = element_text(size=15, family = "Arial", colour = "black")) 
op_prevalence_per_home_plot2<-ggplot(op_prevalence_per_home, aes(x = home, y = species)) + geom_tile(aes(fill=prevalence2))+
  scale_fill_manual(name="Prevalence", values = c("white",'#FEFBE9', '#FCF7D5', '#F5F3C1','#B5DDD8', '#9BD2E1')) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  xlab("Within-household prevalence") + ylab(NULL) + 
  theme(axis.text = element_text(size=15, colour = "black"), axis.title = element_text(size=15, colour = "black"))+ 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  theme(legend.text = element_text(size=15, colour = "black"), legend.title = element_text(size=15, colour = "black")) 

op_prevalence_plot<-op_prevalence_across_homes_plot + op_prevalence_per_home_plot + plot_layout(ncol = 2, widths = c(2, 9))
op_prevalence_plot1<-op_prevalence_across_homes_plot + op_prevalence_per_home_plot1 + plot_layout(ncol = 2, widths = c(2, 3))
op_prevalence_plot2<-op_prevalence_across_homes_plot + op_prevalence_per_home_plot2 + plot_layout(ncol = 2, widths = c(2, 3))
ggsave("~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/op_prevalence_plot2.pdf", plot = op_prevalence_plot2, device = "pdf", width=14, height = 6)

#########regression on op vs cl and temp
shower_meta$sample<-paste(shower_meta$home, ".day.", shower_meta$day, sep = "")
rownames(op_mpa4)<-op_mpa4$species
op_mpa4$species<-NULL
op_mpa4_t<-t(op_mpa4)
op_in_lasso<-c("Mycobacterium gordonae", "Acinetobacter junii", "Mycobacterium mucogenicum", "Mycobacterium arupense", "Mycobacterium sp YC RL4")
op_for_regression<-as.data.frame(op_mpa4_t[, colnames(op_mpa4_t) %in% op_in_lasso])
op_for_regression$sample<-rownames(op_for_regression)
op_for_regression<-merge(op_for_regression, shower_meta[,c(3,4,6)])
summary(lm(op_for_regression[,6] ~ op_for_regression[,7] + op_for_regression[,8]))
summary(lm(log10(op_for_regression[,6]+0.001) ~ op_for_regression[,7] + op_for_regression[,8]))

######ko vs taxonomy BC
shower_mpa_species_forbc_bray_long_format<-matrixConvert(shower_mpa_species_forbc_bray, colname = c("mpa_sp1", "mpa_sp2", "mpa_bc_dist"))
ko_mpa<-cbind(ko_rpkm_rarefied_bray_long_format, shower_mpa_species_forbc_bray_long_format)
ko_mpa$ko_bc_sim<-1-ko_mpa$ko_bc
ko_mpa$mpa_bc_sim<-1-ko_mpa$mpa_bc_dist
ko_mpa_plot<-ggplot(data = ko_mpa, aes(x=ko_bc_sim, y=mpa_bc_sim))+geom_point(size=0.5)+
  theme(panel.background = element_rect(fill='white', colour = 'black'), panel.border = element_rect(fill = NA, colour = 'black')) +  
  theme(axis.title = element_text(size = 12, family = "Arial", colour = "black"), axis.text = element_text(size = 12, family = "Arial", colour = "black"))+
  xlab("Similarity in KO profiles")+ylab("Similarity in species profiles")

ko_mpa_plot<-ggplot(data = ko_mpa, aes(x=ko_bc_sim, y=mpa_bc_sim))+geom_point(size=0.5)+
  theme(panel.background = element_rect(fill='white', colour = 'black'), panel.border = element_rect(fill = NA, colour = 'black')) +  
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 12, colour = "black"))+
  xlab("Similarity in KO profiles")+ylab("Similarity in species profiles")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_mpa1.pdf", ko_mpa_plot, device = "pdf", width = 3, height = 3)

#######ko vs amr BC
amr_gene_rpkm_sorted<-amr_gene_rpkm[,sort(colnames(amr_gene_rpkm))]
amr_gene_rpkm_sorted_bray<-vegdist(t(amr_gene_rpkm_sorted), method = "bray")
amr_gene_rpkm_sorted_bray_long_format<-matrixConvert(amr_gene_rpkm_sorted_bray, colname = c("amr_sp1", "amr_sp2", "amr_bc_dist"))
ko_amr<-cbind(ko_rpkm_rarefied_bray_long_format, amr_gene_rpkm_sorted_bray_long_format)
ko_amr$ko_bc_sim<-1-ko_amr$ko_bc
ko_amr$amr_bc_sim<-1-ko_amr$amr_bc_dist
ko_amr_plot<-ggplot(data = ko_amr, aes(x=ko_bc_sim, y=amr_bc_sim))+geom_point()+
  theme(panel.background = element_rect(fill='white', colour = 'black'), panel.border = element_rect(fill = NA, colour = 'black')) +  
  theme(axis.title = element_text(size = 12, family = "Arial", colour = "black"), axis.text = element_text(size = 12, family = "Arial", colour = "black"))+
  xlab("Similarity in KO profiles")+ylab("Similarity in AMR gene profiles")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_amr.png", ko_amr_plot, device = "png", width = 4, height = 4)

amr_gene_rpkm_matrix<-as.matrix(amr_gene_rpkm)
amr_gene_rpkm_filtered <- amr_gene_rpkm_matrix[, colSums(amr_gene_rpkm_matrix)>=302]  
amr_gene_rpkm_filtered_rarefied <- rbiom::rarefy(as.matrix(amr_gene_rpkm_filtered), seed=1, depth = 302)
amr_gene_rpkm_filtered_rarefied_sorted<-amr_gene_rpkm_filtered_rarefied[,sort(colnames(amr_gene_rpkm_filtered_rarefied))]
amr_gene_rpkm_filtered_rarefied_sorted_bray<-vegdist(t(amr_gene_rpkm_filtered_rarefied_sorted), method = "bray")
amr_gene_rpkm_filtered_rarefied_sorted_bray_long_format<-matrixConvert(amr_gene_rpkm_filtered_rarefied_sorted_bray, colname = c("amr_sp1", "amr_sp2", "amr_bc_dist"))
ko_rpkm_rarefied_filtered<-ko_rpkm_rarefied[,colnames(ko_rpkm_rarefied) %in% colnames(amr_gene_rpkm_filtered_rarefied_sorted)]
ko_rpkm_rarefied_filtered_bray<-vegdist(t(ko_rpkm_rarefied_filtered), method = "bray")
ko_rpkm_rarefied_filtered_bray_long_format<-matrixConvert(ko_rpkm_rarefied_filtered_bray, colname = c("ko_sp1", "ko_sp2", "ko_bc_dist"))
ko_amr1<-cbind(amr_gene_rpkm_filtered_rarefied_sorted_bray_long_format, ko_rpkm_rarefied_filtered_bray_long_format)
ko_amr1$ko_bc_sim<-1-ko_amr1$ko_bc_dist
ko_amr1$amr_bc_sim<-1-ko_amr1$amr_bc_dist
ko_amr_plot1<-ggplot(data = ko_amr1, aes(x=ko_bc_sim, y=amr_bc_sim))+geom_point(size=0.5)+
  theme(panel.background = element_rect(fill='white', colour = 'black'), panel.border = element_rect(fill = NA, colour = 'black')) +  
  theme(axis.title = element_text(size = 12, family = "Arial", colour = "black"), axis.text = element_text(size = 12, family = "Arial", colour = "black"))+
  xlab("Similarity in KO profiles")+ylab("Similarity in ARG profiles")

ko_amr_plot1<-ggplot(data = ko_amr1, aes(x=ko_bc_sim, y=amr_bc_sim))+geom_point(size=0.5)+
  theme(panel.background = element_rect(fill='white', colour = 'black'), panel.border = element_rect(fill = NA, colour = 'black')) +  
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 12, colour = "black"))+
  xlab("Similarity in KO profiles")+ylab("Similarity in ARG profiles")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_amr1.pdf", ko_amr_plot1, device = "pdf", width = 3, height = 3)
##########mpa vs amr bc
mpa_amr<-cbind(amr_gene_rpkm_sorted_bray_long_format, ko_mpa[,8])
colnames(mpa_amr)[4]<-"mpa_bc_sim"
mpa_amr$amr_bc_sim<-1-mpa_amr$amr_bc_dist
mpa_amr_plot<-ggplot(data = mpa_amr, aes(x=amr_bc_sim, y=mpa_bc_sim))+geom_point()+
  theme(panel.background = element_rect(fill='white', colour = 'black'), panel.border = element_rect(fill = NA, colour = 'black')) +  
  theme(axis.title = element_text(size = 12, family = "Arial", colour = "black"), axis.text = element_text(size = 12, family = "Arial", colour = "black"))+
  xlab("Similarity in ARG profiles")+ylab("Similarity in species profiles")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/mpa_amr.png", mpa_amr_plot, device = "png", width = 4, height = 4)
summary(lm(mpa_bc_sim~amr_bc_sim, data = mpa_amr))

summary(lm(mpa_bc_sim~ko_bc_sim,data = ko_mpa))
ko_nobac120_mpa<-cbind(ko_rpkm_nobac120_rarefied_bray_long_format, shower_mpa_species_forbc_bray_long_format)
ko_nobac120_mpa$ko_bc_sim<-1-ko_nobac120_mpa$ko_bc
ko_nobac120_mpa$mpa_bc_sim<-1-ko_nobac120_mpa$mpa_bc_dist
ko_nobac120_mpa_plot<-ggplot(data = ko_nobac120_mpa, aes(x=ko_bc_sim, y=mpa_bc_sim))+geom_point()+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12))+
  xlab("Bray-Curtis similarity in KO profiles\nafter removing Bac120")+ylab("Bray-Curtis similarity in species profiles")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_nobac120_mpa.png", ko_nobac120_mpa_plot, device = "png", width = 4, height = 4)
summary(lm(mpa_bc_sim~ko_bc_sim,data = ko_nobac120_mpa))
ko_nobac120_mpa$home1<-substr(ko_nobac120_mpa$ko_sp1,1,6)
ko_nobac120_mpa$home2<-substr(ko_nobac120_mpa$ko_sp2,1,6)
ko_nobac120_mpa$type <- ifelse(ko_nobac120_mpa$home1 == ko_nobac120_mpa$home2, "Within homes", "Between homes")
shower_mpa_species_forbc_bray_long_format1<-shower_mpa_species_forbc_bray_long_format
shower_mpa_species_forbc_bray_long_format1$sample <- ifelse(substr(shower_mpa_species_forbc_bray_long_format1$mpa_sp1,1,6) == substr(shower_mpa_species_forbc_bray_long_format1$mpa_sp2,1,6), "Within homes", "Between homes")
shower_mpa_species_forbc_bray_long_format1$type<-rep("Species profile", nrow(shower_mpa_species_forbc_bray_long_format1))
ko_rpkm_rarefied_bray_long_format1<-ko_rpkm_rarefied_bray_long_format
ko_rpkm_rarefied_bray_long_format1$sample <- ifelse(substr(ko_rpkm_rarefied_bray_long_format1$sp1,1,6) == substr(ko_rpkm_rarefied_bray_long_format1$sp2,1,6), "Within homes", "Between homes")
ko_rpkm_rarefied_bray_long_format1$type<-rep("KO profile", nrow(ko_rpkm_rarefied_bray_long_format1))
colnames(shower_mpa_species_forbc_bray_long_format1)<-colnames(ko_rpkm_rarefied_bray_long_format1)
ko_mpa_for_plot<-rbind(shower_mpa_species_forbc_bray_long_format1, ko_rpkm_rarefied_bray_long_format1)
ko_mpa_bc_boxplot <- ggplot(ko_nobac120_mpa_for_plot, aes(x=sample, y=ko_bc)) +
  geom_half_boxplot(nudge = 0.1,outlier.shape = NA) +
  geom_jitter(size=0.5, show.legend = FALSE, position = position_jitter(width=.1, height=0)) +
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  labs(y="Bray Curtis distance", x=NULL)+
  theme(strip.text = element_text(size = 12, family = "Arial", colour = "black"), strip.background = element_rect(fill = 'white'))+
  theme(axis.title = element_text(size = 12, family = "Arial", colour = "black"), axis.text = element_text(size = 12, family = "Arial", colour = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~type)

ko_mpa_bc_boxplot <- ggplot(ko_nobac120_mpa_for_plot, aes(x=sample, y=ko_bc)) +
  geom_half_boxplot(nudge = 0.1,outlier.shape = NA) +
  geom_jitter(size=0.5, show.legend = FALSE, position = position_jitter(width=.1, height=0)) +
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  labs(y="Bray Curtis distance", x=NULL)+
  theme(strip.text = element_text(size = 12, colour = "black"), strip.background = element_rect(fill = 'white'))+
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 12, colour = "black"), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~type)
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_mpa_bc_boxplot1.pdf", ko_mpa_bc_boxplot, device = "pdf", width = 3, height = 4)
ko_rpkm_nobac120_rarefied_bray_long_format1<-ko_rpkm_nobac120_rarefied_bray_long_format
ko_rpkm_nobac120_rarefied_bray_long_format1$sample <- ifelse(substr(ko_rpkm_nobac120_rarefied_bray_long_format1$ko_sp1,1,6) == substr(ko_rpkm_nobac120_rarefied_bray_long_format1$ko_sp2,1,6), "Within homes", "Between homes")
ko_rpkm_nobac120_rarefied_bray_long_format1$type<-rep("KO profile", nrow(ko_rpkm_nobac120_rarefied_bray_long_format1))  
colnames(ko_rpkm_nobac120_rarefied_bray_long_format1)<-colnames(shower_mpa_species_forbc_bray_long_format1)
ko_nobac120_mpa_for_plot<-rbind(shower_mpa_species_forbc_bray_long_format1, ko_rpkm_nobac120_rarefied_bray_long_format1)
ko_nobac120_mpa_bc_boxplot <- ggplot(ko_nobac120_mpa_for_plot, aes(x=sample, y=ko_bc)) +
  geom_half_boxplot(nudge = 0.1,outlier.shape = NA) +
  geom_jitter(size=1, show.legend = FALSE, position = position_jitter(width=.1, height=0)) +
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  labs(y="Bray-Curtis distance", x=NULL)+
  theme(strip.text = element_text(size = 15), strip.background = element_rect(fill = 'white'))+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 15, hjust = 1)) +
  facet_wrap(~type)
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_nobac120_mpa_bc_boxplot.png", ko_nobac120_mpa_bc_boxplot, device = "png", width = 5.5, height = 4)


ko_bc_compare<-as.data.frame(ko_mpa$ko_bc_sim)
ko_bc_compare$ko_nobac120_bc_sim<-ko_nobac120_mpa$ko_bc_sim
colnames(ko_bc_compare)[1]<-"ko_bc_sim"
t.test(ko_bc_compare$ko_bc_sim, ko_bc_compare$ko_nobac120_bc_sim, paired = T)
ko_bc_compare_longformat<-ko_bc_compare %>% gather(key = "bc_type", value = "bc", ko_bc_sim:ko_nobac120_bc_sim)
ko_bc_compare_boxplot <- ggplot(ko_bc_compare_longformat, aes(x=bc_type, y=bc)) +
  geom_half_boxplot(nudge = 0.1,outlier.shape = NA) +
  geom_jitter(size=1, show.legend = FALSE, position = position_jitter(width=.1, height=0)) +
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  labs(y="Bray-Curtis similarity", x=NULL)+
  theme(strip.text = element_text(size = 15), strip.background = element_rect(fill = 'white'))+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12), axis.text.x = element_text(angle = 15, hjust = 1)) 
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_bc_compare_boxplot.png", ko_bc_compare_boxplot, device = "png", width = 4, height = 4)

#####ko vs taxonomy richness
ko_non0_count <- apply(ko_rpkm, 2, function(x) sum(x != 0, na.rm = TRUE))
ko_non0_count<-as.data.frame(ko_non0_count)
ko_non0_count$home<-substr(rownames(ko_non0_count), 1, 6)
ko_non0_count <- ko_non0_count %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
ko_non0_count$day<-substr(rownames(ko_non0_count),8,12)
mpa_species_non0_count <- apply(shower_mpa_species[,2:57], 2, function(x) sum(x != 0, na.rm = TRUE))
mpa_species_non0_count<-as.data.frame(mpa_species_non0_count)
mpa_species_non0_count$home<-substr(rownames(mpa_species_non0_count), 1, 6)
mpa_species_non0_count <- mpa_species_non0_count %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
mpa_species_non0_count <- mpa_species_non0_count[order(rownames(mpa_species_non0_count)), ]
mpa_species_non0_count$day<-substr(rownames(mpa_species_non0_count),8,12)
species_ko_richness<-cbind(mpa_species_non0_count,ko_non0_count)
species_ko_richness[,2]<-NULL
species_ko_richness_plot<-ggplot(data = species_ko_richness, aes(x=mpa_species_non0_count, y=ko_non0_count))+geom_point(size=1)+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(axis.title = element_text(size = 12, family = "Arial", colour = "black"), axis.text = element_text(size = 12, family = "Arial", colour = "black"))+
  xlab("Number of species")+ylab("Number of KOs")

species_ko_richness_plot<-ggplot(data = species_ko_richness, aes(x=mpa_species_non0_count, y=ko_non0_count))+geom_point(size=1)+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(axis.title = element_text(size = 12, colour = "black"), axis.text = element_text(size = 12, colour = "black"))+
  xlab("Number of species")+ylab("Number of KOs")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/species_ko_richness1.pdf", species_ko_richness_plot, device = "pdf", width = 3, height = 3)
species_ko_richness_plot1<-ggplot(data = species_ko_richness, aes(x=mpa_species_non0_count, y=ko_non0_count, color=home))+geom_point(size=3)+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+scale_color_manual(values = color_custom)+
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 12), legend.position = "top")+
  xlab("Number of species")+ylab("Number of KOs")+guides(color=guide_legend(title="Household\nidentity"))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/Figure_S34.png", species_ko_richness_plot1, device = "png", width = 5, height = 5)
summary(lm(ko_non0_count~mpa_species_non0_count,data = species_ko_richness))

######number of kos in foam 
FOAM<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/functional_analysis/FOAM-onto_rel1.tsv", header = T, sep = "\t")
level1_ko_count<-FOAM %>%group_by(L1) %>% summarise(Unique_Count = n_distinct(KO))

######ko foam
ko_rarefied_foam<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/functional_analysis/ko_rpkm_keep_multiple_ko_assignment_rarefied_foamalllevel_appended.csv", header = T, sep = "\t")
ko_rarefied_foam_l1<-ko_rarefied_foam[,1:58]
ko_rarefied_foam_l1$ko_l1<-paste(ko_rarefied_foam_l1$query, ko_rarefied_foam_l1$L1, sep = "_")
ko_rarefied_foam_l1 <- ko_rarefied_foam_l1 %>% distinct(ko_l1, .keep_all = TRUE)
ko_rarefied_foam_l1_classified <- ko_rarefied_foam_l1 %>% filter(L1 != "")
ko_rarefied_foam_l1_classified_sum <- ko_rarefied_foam_l1_classified[,2:58] %>% group_by(L1) %>% summarise(across(starts_with("Home"), sum, na.rm = TRUE))
ko_rarefied_foam_l1_classified_sum_columnsums<-colSums(ko_rarefied_foam_l1_classified_sum[,2:57])
ko_rarefied_foam_l1_classified_sum_rel<-sweep(ko_rarefied_foam_l1_classified_sum[,2:57],2,ko_rarefied_foam_l1_classified_sum_columnsums,FUN = "/")
ko_rarefied_foam_l1_classified_sum_rel$L1<-ko_rarefied_foam_l1_classified_sum$L1
ko_rarefied_foam_l1_classified_sum_rel_longformat <- ko_rarefied_foam_l1_classified_sum_rel %>% gather(key= "sampleinfo",value="rpkm", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l1_classified_sum_rel_longformat$home<-substr(ko_rarefied_foam_l1_classified_sum_rel_longformat$sampleinfo, 1, 6)
ko_rarefied_foam_l1_classified_sum_rel_longformat$day<-substr(ko_rarefied_foam_l1_classified_sum_rel_longformat$sampleinfo,12,12)
ko_rarefied_foam_l1_classified_sum_rel_longformat <- ko_rarefied_foam_l1_classified_sum_rel_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
custom_color21<-c("#41bbc5", "#145a6a", "#60df82", "#2a6b2a", "#b5d08d", "#1f3ca6", "#dacff1", "#8886e9", "#fe74fe", "#a335c8", "#cf749b", "#722e57", "#c9197d", "#9f8b9a", "#3f16f9", "#9dea19", "#683d0d", "#fda547", "#dc3c07", "#e1d936", "#34f50e")
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("03_Superpathway of thiosulfate metabolism (Desulfovibrio sulfodismutans)",
                                                           "03_Superpathway of\nthiosulfate metabolism\n(Desulfovibrio sulfodismutans)",
                                                           ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("04_Utililization of sugar, conversion of pentose to EMP pathway intermediates",
                                                           "04_Utililization of sugar,\nconversion of pentose to\nEMP pathway intermediates",
                                                           ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("06_Amino acid utilization biosynthesis metabolism",
                                                           "06_Amino acid utilization\nbiosynthesis metabolism",
                                                           ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("09_Carbohydrate Active enzyme - CAZy",
                                                           "09_Carbohydrate Active\nenzyme - CAZy",
                                                           ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("13_Hydrogen metabolism", "13_Hydrogen\nmetabolism", ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("16_Embden Meyerhof-Parnos (EMP)",
                                                           "16_Embden Meyerhof\n-Parnos (EMP)",
                                                           ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("18_Sulfur compounds metabolism",
                                                           "18_Sulfur compounds\nmetabolism",
                                                           ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("19_Saccharide and derivated synthesis",
                                                           "19_Saccharide and\nderivated synthesis",
                                                           ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_rarefied_foam_l1_classified_sum_rel_longformat$L1<-gsub("21_Cellular response to stress",
                                                           "21_Cellular response\nto stress",
                                                           ko_rarefied_foam_l1_classified_sum_rel_longformat$L1, fixed = T)
ko_l1_arealot<-ggplot() +geom_area(aes(x=as.numeric(day), y=rpkm,fill=L1), data = ko_rarefied_foam_l1_classified_sum_rel_longformat, position = 'stack') + 
  facet_wrap(~home,nrow = 1)+theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  guides(fill=guide_legend(title="FOAM Level 1",title.theme = element_text(size = 12, family = "Arial", colour = 'black'), ncol = 5))+scale_fill_manual(values = custom_color21) + 
  theme(legend.position = "bottom",legend.text = element_text(size=12, family = "Arial", colour = 'black'), 
        axis.text = element_text(size = 12, family = "Arial", colour = 'black'),axis.title=element_text(size=12, family = "Arial", colour = 'black'),
        strip.text = element_text(size = 12, family = "Arial", colour = 'black'), strip.background = element_rect(fill = 'white'))+
  scale_x_continuous(n.breaks = 7)+ylab("Relative abundance")+xlab("Day of the sampling")

ko_l1_arealot<-ggplot() +geom_area(aes(x=as.numeric(day), y=rpkm,fill=L1), data = ko_rarefied_foam_l1_classified_sum_rel_longformat, position = 'stack') + 
  facet_wrap(~home,nrow = 1)+theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
  guides(fill=guide_legend(title="FOAM Level 1",title.theme = element_text(size = 12, colour = 'black'), ncol = 5))+scale_fill_manual(values = custom_color21) + 
  theme(legend.position = "bottom",legend.text = element_text(size=12, colour = 'black'), 
        axis.text = element_text(size = 12, colour = 'black'),axis.title=element_text(size=12, colour = 'black'),
        strip.text = element_text(size = 12, colour = 'black'), strip.background = element_rect(fill = 'white'))+
  scale_x_continuous(n.breaks = 7)+ylab("Relative abundance")+xlab("Day of the sampling")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_l1_areaplot2.pdf", ko_l1_arealot, device = "pdf", height=5)

ko_rarefied_foam_l1_classified_sum_longformat <- ko_rarefied_foam_l1_classified_sum %>% gather(key= "sampleinfo",value="rpkm", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l1_classified_sum_longformat$home<-substr(ko_rarefied_foam_l1_classified_sum_longformat$sampleinfo, 1, 6)
ko_rarefied_foam_l1_classified_sum_longformat$day<-substr(ko_rarefied_foam_l1_classified_sum_longformat$sampleinfo,12,12)
ko_rarefied_foam_l1_classified_sum_longformat_z <- ko_rarefied_foam_l1_classified_sum_longformat %>%
  group_by(L1) %>%
  mutate(z_score = (rpkm - mean(rpkm, na.rm = TRUE)) / sd(rpkm, na.rm = TRUE)) %>%
  ungroup() 
ko_rpkm_rarefied_foam_l1_heatmap <- ggplot(ko_rarefied_foam_l1_classified_sum_longformat,aes(x=day, y=L1))+geom_tile(aes(fill=rpkm))+ labs(y="FOAM Level 1", x=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  theme(axis.text.x = element_text(size=9), axis.title = element_text(size=9))+ 
  facet_wrap(~home, nrow = 1)+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  guides(fill=guide_legend(title="RPKM"))
ko_rpkm_rarefied_foam_l1_heatmap_z <- ggplot(ko_rarefied_foam_l1_classified_sum_longformat_z,aes(x=day, y=L1))+geom_tile(aes(fill=z_score))+ labs(y="FOAM Level 1", x=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  theme(axis.text.x = element_text(size=9), axis.title = element_text(size=9))+ 
  facet_wrap(~home, nrow = 1)+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  guides(fill=guide_legend(title="Row Z-Score"))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_rpkm_rarefied_foam_l1_heatmap_z.png", plot=ko_rpkm_rarefied_foam_l1_heatmap_z, device = "png")

ko_rarefied_foam_l2<-ko_rarefied_foam[,1:59]
ko_rarefied_foam_l2$ko_l1_l2<-paste(ko_rarefied_foam_l2$query, ko_rarefied_foam_l2$L1, ko_rarefied_foam_l2$L2, sep = "_")
ko_rarefied_foam_l2 <- ko_rarefied_foam_l2 %>% distinct(ko_l1_l2, .keep_all = TRUE)
ko_rarefied_foam_l2_classified <- ko_rarefied_foam_l2 %>% filter(L1 != "")
ko_rarefied_foam_l2_classified <- ko_rarefied_foam_l2 %>% filter(L2 != "")
ko_rarefied_foam_l2_classified$l1_l2<-paste(ko_rarefied_foam_l2_classified$L1, ko_rarefied_foam_l2_classified$L2, sep = "_")
ko_rarefied_foam_l2_classified_sum <- ko_rarefied_foam_l2_classified[,c(2:57,61)] %>% group_by(l1_l2) %>% summarise(across(starts_with("Home"), sum, na.rm = TRUE))
ko_rarefied_foam_l2_classified_sum_longformat <- ko_rarefied_foam_l2_classified_sum %>% gather(key= "sampleinfo",value="rpkm", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l2_classified_sum_longformat$home<-substr(ko_rarefied_foam_l2_classified_sum_longformat$sampleinfo, 1, 6)
ko_rarefied_foam_l2_classified_sum_longformat$day<-substr(ko_rarefied_foam_l2_classified_sum_longformat$sampleinfo,12,12)
ko_rarefied_foam_l2_classified_sum_longformat <- ko_rarefied_foam_l2_classified_sum_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
ko_rarefied_foam_l2_classified_sum_longformat <- separate(ko_rarefied_foam_l2_classified_sum_longformat, l1_l2, into = c("Col1", "Col2", "l2"), sep = "_")
ko_rarefied_foam_l2_classified_sum_longformat$l1<-paste(ko_rarefied_foam_l2_classified_sum_longformat$Col1, ko_rarefied_foam_l2_classified_sum_longformat$Col2, sep = "_")
l2_areaplot <- function(l1_level) {
  l2_df<-subset(ko_rarefied_foam_l2_classified_sum_longformat, ko_rarefied_foam_l2_classified_sum_longformat$Col1==as.character(l1_level))
  l2_df_rel<-l2_df %>% group_by(sampleinfo) %>% mutate(rpkm_rel = rpkm / sum(rpkm, na.rm = TRUE)) %>% ungroup()
  ggplot() +geom_area(aes(x=as.numeric(day), y=rpkm_rel,fill=l2), data = l2_df_rel, position = 'stack') + 
    facet_wrap(~home,nrow = 1)+theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
    guides(fill=guide_legend(title="FOAM Level 2",title.theme = element_text(size = 12), ncol = 2))+
    theme(plot.title = element_text(size = 12, face = "bold"),legend.position = "bottom",legend.text = element_text(size=12), axis.text = element_text(size = 12),axis.title=element_text(size=12),strip.text = element_text(size = 12), strip.background = element_rect(fill = 'white'))+
    scale_x_continuous(n.breaks = 7)+ylab("Relative abundance")+xlab("Day of the sampling")+ggtitle(l2_df_rel$l1)
}
numbers_list <- sprintf("%02d", 1:21)
numbers_list <- setdiff(numbers_list, "03")
for (i in numbers_list) {
  plot<-l2_areaplot(as.character(i))
  file_name <- paste0("l1_", i, "_l2_areaplot.png")
  ggsave(file_name, plot = plot, device = "png", height = 6)
}
ko_rarefied_foam_l2_classified_sum_longformat_z <- ko_rarefied_foam_l2_classified_sum_longformat %>%
  group_by(l1_l2) %>%
  mutate(z_score = (rpkm - mean(rpkm, na.rm = TRUE)) / sd(rpkm, na.rm = TRUE)) %>%
  ungroup()
ko_rpkm_rarefied_foam_l2_heatmap_z <- ggplot(ko_rarefied_foam_l2_classified_sum_longformat_z,aes(x=day, y=l1_l2))+geom_tile(aes(fill=z_score))+ labs(y="FOAM Level 2", x=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  theme(axis.text.x = element_text(size=9), axis.title = element_text(size=9))+ 
  facet_wrap(~home, nrow = 1)+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  guides(fill=guide_legend(title="Row Z-Score"))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/ko_rpkm_rarefied_foam_l2_heatmap_z.png", plot=ko_rpkm_rarefied_foam_l2_heatmap_z, device = "png", height = 17)


ko_rarefied_foam_l1_classified_11<-subset(ko_rarefied_foam_l1_classified, ko_rarefied_foam_l1_classified$L1=="11_Nitrogen cycle")
ko_rarefied_foam_l1_classified_11_longformat <- ko_rarefied_foam_l1_classified_11 %>% gather(key= "sampleinfo",value="rpkm", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l1_classified_11_longformat_z <- ko_rarefied_foam_l1_classified_11_longformat %>%
  group_by(query) %>%
  mutate(z_score = (rpkm - mean(rpkm, na.rm = TRUE)) / sd(rpkm, na.rm = TRUE)) %>%
  ungroup()
ko_rarefied_foam_l1_classified_11_longformat_z$home<-substr(ko_rarefied_foam_l1_classified_11_longformat_z$sampleinfo, 1, 6)
ko_rarefied_foam_l1_classified_11_longformat_z <- ko_rarefied_foam_l1_classified_11_longformat_z %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
ko_rarefied_foam_l1_classified_11_longformat_z$day<-substr(ko_rarefied_foam_l1_classified_11_longformat_z$sampleinfo, 12, 12)
ko_rarefied_foam_l1_classified_11_longformat_z<-ko_rarefied_foam_l1_classified_11_longformat_z[ko_rarefied_foam_l1_classified_11_longformat_z$query!="K10534",]
ko_rpkm_rarefied_foam_l1_11_heatmap_z <- ggplot(ko_rarefied_foam_l1_classified_11_longformat_z,aes(x=day, y=query))+geom_tile(aes(fill=z_score))+ labs(y="KO", x=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  theme(axis.text.x = element_text(size=9), axis.title = element_text(size=9))+ 
  facet_wrap(~home, nrow = 1)+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  guides(fill=guide_legend(title="Row Z-Score"))
ko_rarefied_foam_l1_classified_11_alllevel<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/nitrogen_all_level_rpkm.csv", header = T)
ko_rarefied_foam_l1_classified_11_alllevel_sorted <- ko_rarefied_foam_l1_classified_11_alllevel[order(ko_rarefied_foam_l1_classified_11_alllevel$L2, ko_rarefied_foam_l1_classified_11_alllevel$L3), ]
ko_rarefied_foam_l1_classified_11_alllevel_sorted$ko_order<-c(1:nrow(ko_rarefied_foam_l1_classified_11_alllevel_sorted))
ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat<-ko_rarefied_foam_l1_classified_11_alllevel_sorted %>% gather(key = "sampleinfo", value = "rpkm", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$home<-substr(ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$sampleinfo,1,6)
ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat<-ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$day<-substr(ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$sampleinfo,12,12)
ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$rpkm1<-ifelse(ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$rpkm==0, NA, ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$rpkm)
ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$ko_order<-factor(ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat$ko_order, levels = ko_rarefied_foam_l1_classified_11_alllevel_sorted$ko_order)
ko_l1_11_heatmap <- ggplot(ko_rarefied_foam_l1_classified_11_alllevel_sorted_longformat,aes(x=day, y=query))+
  geom_tile(aes(fill=rpkm))+ labs(y="KO", x=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  theme(axis.text = element_text(size=12), axis.title = element_text(size=12), legend.text = element_text(size = 12), strip.text = element_text(size = 12))+ 
  facet_grid(L3~home, scales = "free_y", space = "free")+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 1000), breaks=seq(0,1000,by=200), low='#bbdffb', high='#0747a1', na.value="white") +
  guides(fill=guide_legend(title="RPKM"))+theme(strip.text.y.right = element_text(angle = 0), legend.position = "bottom")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/nitrogen_ko_rpkm1.png", plot=ko_l1_11_heatmap, device = "png")

ko_rarefied_foam_l1_classified_11_alllevel_longformat <- ko_rarefied_foam_l1_classified_11_alllevel_longformat %>%
  mutate(L2 = recode(L2, "Nitrification (aerobic ammonia oxydation, bacterial and archea)" = "Nitrification (aerobic ammonia\noxydation, bacterial and archea)"))
nitrogen_l2l3_rpkm_plot<-ggplot(ko_rarefied_foam_l1_classified_11_alllevel_longformat,aes(x=day, y=query))+
  geom_tile(aes(fill=rpkm))+ labs(y="KO", x=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  theme(axis.text = element_text(size=12), axis.title = element_text(size=12), legend.text = element_text(size = 12), strip.text = element_text(size = 12))+
  facet_nested(L2+L3~home, scales = "free_y", space = "free", nest_line = element_line(linetype = 1))+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 1000), breaks=seq(0,1000,by=200), low='#bbdffb', high='#0747a1', na.value="white") +
  guides(fill=guide_legend(title="RPKM"))+theme(strip.text.y.right = element_text(angle = 0), legend.position = "bottom") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/nitrogen_l2l3_rpkm.png", plot=nitrogen_l2l3_rpkm_plot, device = "png", width = 13, height = 7.5)

ko_rarefied_foam_l1_classified_03<-subset(ko_rarefied_foam_l1_classified, ko_rarefied_foam_l1_classified$L1=="03_Superpathway of thiosulfate metabolism (Desulfovibrio sulfodismutans)")
ko_rarefied_foam_l1_classified_14<-subset(ko_rarefied_foam_l1_classified, ko_rarefied_foam_l1_classified$L1=="14_Methanogenesis")

ko_rarefied_foam_l1_classified_15<-subset(ko_rarefied_foam_l1_classified, ko_rarefied_foam_l1_classified$L1=="15_Methylotrophy")
ko_rarefied_foam_l1_classified_15_l3_longformat<-ko_rarefied_foam_l1_classified_15_l3 %>% gather(key = "sampleinfo", value = "rpkm", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l1_classified_15_l3_longformat$home<-substr(ko_rarefied_foam_l1_classified_15_l3_longformat$sampleinfo,1,6)
ko_rarefied_foam_l1_classified_15_l3_longformat<-ko_rarefied_foam_l1_classified_15_l3_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
ko_rarefied_foam_l1_classified_15_l3_longformat$day<-substr(ko_rarefied_foam_l1_classified_15_l3_longformat$sampleinfo,12,12)
ko_rarefied_foam_l1_classified_15_l3_longformat$rpkm1<-ifelse(ko_rarefied_foam_l1_classified_15_l3_longformat$rpkm==0, NA, ko_rarefied_foam_l1_classified_15_l3_longformat$rpkm)
ko_rarefied_foam_l1_classified_15_l3_longformat <- ko_rarefied_foam_l1_classified_15_l3_longformat %>%
  mutate(L2 = recode(L2, "RuMP pathway for formaldehyde fixation (first two steps are common to both RuMP pathway variants)" = "RuMP pathway for formaldehyde fixation\n(first two steps are common to both RuMP pathway variants)"))
ko_rarefied_foam_l1_classified_15_l3_longformat <- ko_rarefied_foam_l1_classified_15_l3_longformat %>%
  mutate(L2 = recode(L2, "Methane oxidation to cO2" = "Methane oxidation to CO2"))
ko_rarefied_foam_l1_classified_15_l3_longformat<-subset(ko_rarefied_foam_l1_classified_15_l3_longformat, !ko_rarefied_foam_l1_classified_15_l3_longformat$query %in% c("K13402", "K16162", "K16160"))
ko_l1_15_plot <- ggplot(ko_rarefied_foam_l1_classified_15_l3_longformat,aes(x=day, y=query))+
  geom_tile(aes(fill=rpkm1))+ labs(y="KO", x=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white')) +
  theme(axis.text = element_text(size=12), axis.title = element_text(size=12), legend.text = element_text(size = 12), strip.text = element_text(size = 12))+ 
  facet_grid(L2~home, scales = "free_y", space = "free")+xlab("Day of the sampling")+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 1800), breaks=seq(0,1800,by=300), low='#bbdffb', high='#0747a1', na.value="white") +
  guides(fill=guide_legend(title="RPKM", nrow = 1))+theme(strip.text.y.right = element_text(angle = 0), legend.position = "bottom") +
  theme(strip.text.y = element_text(angle = 0, hjust = 0))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/methylotrophy_ko_rpkm.png", plot=ko_l1_15_plot, device = "png",width = 12, height = 10)


###presence/absence plot
ko_rarefied_foam_l2_classified_sum_presence_absence <- as.data.frame(lapply(ko_rarefied_foam_l2_classified_sum[,2:57], function(x) as.integer(!is.na(x) & x != 0)))
ko_rarefied_foam_l2_classified_sum_presence_absence$l1_l2<-ko_rarefied_foam_l2_classified_sum$l1_l2
df_separated <- ko_rarefied_foam_l2_classified_sum_presence_absence %>% separate(l1_l2, into = c("L1", "L2"), sep = "_(?=[^_]+$)")
df_separated_longformat <- df_separated %>% gather(key= "sampleinfo",value="presence_absence", Home.1.day.1:Home.8.day.7)
df_separated_longformat$home<-substr(df_separated_longformat$sampleinfo, 1, 6)
df_separated_longformat$day<-substr(df_separated_longformat$sampleinfo,12,12)
df_separated_longformat <- df_separated_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
df_separated_longformat <- df_separated_longformat %>% mutate(across('presence_absence', str_replace, '1', 'Present'))
df_separated_longformat <- df_separated_longformat %>% mutate(across('presence_absence', str_replace, '0', 'Absent'))
df_separated_longformat1 <- df_separated_longformat %>% separate(l1_l2, into = c("test1","test2"), sep = "_")
df_separated_longformat1$test1<-factor(df_separated_longformat1$test1,levels = unique(df_separated_longformat1$test1))
l2_presence_absence<-ggplot(df_separated_longformat1,aes(x=test1, y=day))+geom_tile(aes(fill=presence_absence))+ labs(y="Day of the sampling", x=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size=12), legend.text = element_text(size = 12), legend.position = "bottom")+
  facet_wrap(~home, nrow = 8, strip.position = "right")+xlab("FOAM Level2 function")+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_manual(values = c("grey", '#bbdffb')) +
  guides(fill=guide_legend(title=NULL))+guides(x = ggh4x::guide_axis_nested(delim = "&&", position = "top", angle = 45))+theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/l2_presence_absence.png", plot=l2_presence_absence, device = "png", height = 17)

ko_rarefied_foam_l1_classified_11 <- ko_rarefied_foam_l1_classified_11[ko_rarefied_foam_l1_classified_11$query!="K10534",]
ko_rarefied_foam_l1_classified_11_presence_absence<-as.data.frame(lapply(ko_rarefied_foam_l1_classified_11[,2:57], function(x) as.integer(!is.na(x) & x != 0)))
ko_rarefied_foam_l1_classified_11_presence_absence$query<-ko_rarefied_foam_l1_classified_11$query
ko_rarefied_foam_l1_classified_11_presence_absence_longformat<-ko_rarefied_foam_l1_classified_11_presence_absence %>% gather(key= "sampleinfo",value="presence_absence", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l1_classified_11_presence_absence_longformat$home<-substr(ko_rarefied_foam_l1_classified_11_presence_absence_longformat$sampleinfo, 1, 6)
ko_rarefied_foam_l1_classified_11_presence_absence_longformat$day<-substr(ko_rarefied_foam_l1_classified_11_presence_absence_longformat$sampleinfo,12,12)
ko_rarefied_foam_l1_classified_11_presence_absence_longformat <- ko_rarefied_foam_l1_classified_11_presence_absence_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
ko_rarefied_foam_l1_classified_11_presence_absence_longformat <- ko_rarefied_foam_l1_classified_11_presence_absence_longformat %>% mutate(across('presence_absence', str_replace, '1', 'Present'))
ko_rarefied_foam_l1_classified_11_presence_absence_longformat <- ko_rarefied_foam_l1_classified_11_presence_absence_longformat %>% mutate(across('presence_absence', str_replace, '0', 'Absent'))
nitrogen_ko<-ggplot(ko_rarefied_foam_l1_classified_11_presence_absence_longformat,aes(x=day, y=query))+geom_tile(aes(fill=presence_absence))+ labs(x="Day of the sampling", y=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size=12), legend.text = element_text(size = 12), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_manual(values = c("grey", '#bbdffb')) +
  guides(fill=guide_legend(title=NULL))+ggtitle("11_Nitrogen cycle")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/nitrogen_ko.png", plot=nitrogen_ko, device = "png",height = 8)

ko_rarefied_foam_l1_classified_11_l2<-subset(ko_rarefied_foam_l2_classified_sum_longformat, ko_rarefied_foam_l2_classified_sum_longformat$Col1=="11")
ko_rarefied_foam_l1_classified_11_l2$l2<-gsub("Nitrification (aerobic ammonia oxydation, bacterial and archea)", 
                                              "Nitrification (aerobic ammonia\noxydation, bacterial and archea)", 
                                              ko_rarefied_foam_l1_classified_11_l2$l2, fixed = T)
nitrogen_l2_rpkm<-ggplot(ko_rarefied_foam_l1_classified_11_l2,aes(x=day, y=l2))+geom_tile(aes(fill=rpkm))+ labs(x="Day of the sampling", y="FOAM Level 2") +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12, family = "Arial", colour = "black")) +
  theme(axis.text = element_text(size = 12, family = "Arial", colour = "black"), axis.title = element_text(size=12, family = "Arial", colour = "black"), 
        legend.text = element_text(size = 12, family = "Arial", colour = "black"), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 3200), breaks=seq(0,3200,by=400), low='#bbdffb', high='#0747a1') +
  theme(plot.title = element_text(size = 12, family = "Arial", colour = "black", hjust = 0.5)) +
  guides(fill=guide_legend(title="RPKM", nrow = 1))+ggtitle("Nitrogen cycle")

nitrogen_l2_rpkm<-ggplot(ko_rarefied_foam_l1_classified_11_l2,aes(x=day, y=l2))+geom_tile(aes(fill=rpkm))+ labs(x="Day of the sampling", y="FOAM Level 2") +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12, colour = "black")) +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size=12, colour = "black"), 
        legend.text = element_text(size = 12, colour = "black"), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 3200), breaks=seq(0,3200,by=400), low='#bbdffb', high='#0747a1') +
  theme(plot.title = element_text(size = 12, colour = "black", hjust = 0.5)) +
  guides(fill=guide_legend(title="RPKM", nrow = 1))+ggtitle("Nitrogen cycle")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/nitrogen_l2_rpkm1.pdf", plot=nitrogen_l2_rpkm, device = "pdf", height = 2.5)

ko_rarefied_foam_l1_classified_15_l2<-subset(ko_rarefied_foam_l2_classified_sum_longformat, ko_rarefied_foam_l2_classified_sum_longformat$Col1=="15")
ko_rarefied_foam_l1_classified_15_l2 <- ko_rarefied_foam_l1_classified_15_l2 %>%
  mutate(l2 = recode(l2, "RuMP pathway for formaldehyde fixation (first two steps are common to both RuMP pathway variants)" = "RuMP pathway for formaldehyde fixation\n(first two steps are common to both RuMP pathway variants)"))
ko_rarefied_foam_l1_classified_15_l2 <- ko_rarefied_foam_l1_classified_15_l2 %>%
  mutate(l2 = recode(l2, "Methane oxidation to cO2" = "Methane oxidation to CO2"))
methylotrophy_l2_rpkm<-ggplot(ko_rarefied_foam_l1_classified_15_l2,aes(x=day, y=l2))+geom_tile(aes(fill=rpkm))+ labs(x="Day of the sampling", y="FOAM Level 2") +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), 
        strip.text = element_text(size = 12, family = "Arial", colour = "black")) +
  theme(axis.text = element_text(size = 12, family = "Arial", colour = "black"), axis.title = element_text(size=12, family = "Arial", colour = "black"), 
        legend.text = element_text(size = 12, family = "Arial", colour = "black"), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 2500), breaks=seq(0,2500,by=500), low='#bbdffb', high='#0747a1') +
  theme(plot.title = element_text(size = 12, family = "Arial", colour = "black", hjust = 0.5)) +
  guides(fill=guide_legend(title="RPKM", nrow = 1))+ggtitle("Methylotrophy")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/methylotrophy_l2_rpkm1.png", plot=methylotrophy_l2_rpkm, device = "png", height = 4, width = 13.5)

ko_rarefied_foam_l1_classified_11_l3<-subset(ko_rarefied_foam,ko_rarefied_foam$query %in% ko_rarefied_foam_l1_classified_11$query)
ko_rarefied_foam_l1_classified_11_l3<-subset(ko_rarefied_foam_l1_classified_11_l3,ko_rarefied_foam_l1_classified_11_l3$L1 == "11_Nitrogen cycle")
ko_rarefied_foam_l1_classified_11_l3<-subset(ko_rarefied_foam_l1_classified_11_l3, ko_rarefied_foam_l1_classified_11_l3$L3!="Unclassified")
ko_rarefied_foam_l1_classified_11_l3_sum<-ko_rarefied_foam_l1_classified_11_l3[,c(2:57,60)] %>% group_by(L3) %>% summarise(across(starts_with("Home"), sum))
ko_rarefied_foam_l1_classified_11_l3_sum_longformat<-ko_rarefied_foam_l1_classified_11_l3_sum %>% gather(key = "sampleinfo",value = "rpkm",Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l1_classified_11_l3_sum_longformat$home<-substr(ko_rarefied_foam_l1_classified_11_l3_sum_longformat$sampleinfo,1,6)
ko_rarefied_foam_l1_classified_11_l3_sum_longformat$day<-substr(ko_rarefied_foam_l1_classified_11_l3_sum_longformat$sampleinfo,12,12)
ko_rarefied_foam_l1_classified_11_l3_sum_longformat<-ko_rarefied_foam_l1_classified_11_l3_sum_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
ko_rarefied_foam_l1_classified_11_l3_sum_longformat$presence_absence <- ifelse(ko_rarefied_foam_l1_classified_11_l3_sum_longformat$rpkm == 0, "Absent", "Present")
nitrogen_l3<-ggplot(ko_rarefied_foam_l1_classified_11_l3_sum_longformat,aes(x=day, y=L3))+geom_tile(aes(fill=presence_absence))+ labs(x="Day of the sampling", y="FOAM Level 3") +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size=12), legend.text = element_text(size = 12), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_manual(values = c("grey", '#bbdffb')) +
  guides(fill=guide_legend(title=NULL))+ggtitle("11_Nitrogen cycle")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/nitrogen_l3.png", plot=nitrogen_l3, device = "png", height = 5)
ko_rarefied_foam_l1_classified_11_l3_sum_longformat$rpkm1<-ifelse(ko_rarefied_foam_l1_classified_11_l3_sum_longformat$rpkm==0, NA, ko_rarefied_foam_l1_classified_11_l3_sum_longformat$rpkm)
nitrogen_l3_rpkm<-ggplot(ko_rarefied_foam_l1_classified_11_l3_sum_longformat,aes(x=day, y=L3))+geom_tile(aes(fill=rpkm1))+ labs(x="Day of the sampling", y="FOAM Level 3") +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size=12), legend.text = element_text(size = 12), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 1210), breaks=seq(0,1210,by=200), low='#bbdffb', high='#0747a1', na.value="white") +
  guides(fill=guide_legend(title="RPKM", nrow = 1))+ggtitle("11_Nitrogen cycle")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/nitrogen_l3_rpkm.png", plot=nitrogen_l3_rpkm, device = "png", height = 3)


ko_rarefied_foam_l1_classified_15_l3<-subset(ko_rarefied_foam,ko_rarefied_foam$query %in% ko_rarefied_foam_l1_classified_15$query)
ko_rarefied_foam_l1_classified_15_l3<-subset(ko_rarefied_foam_l1_classified_15_l3,ko_rarefied_foam_l1_classified_15_l3$L1 == "15_Methylotrophy")
ko_rarefied_foam_l1_classified_15_l3<-subset(ko_rarefied_foam_l1_classified_15_l3, ko_rarefied_foam_l1_classified_15_l3$L3!="Unclassified")

ko_rarefied_foam_l1_classified_14_presence_absence<-as.data.frame(lapply(ko_rarefied_foam_l1_classified_14[,2:57], function(x) as.integer(!is.na(x) & x != 0)))
ko_rarefied_foam_l1_classified_14_presence_absence$query<-ko_rarefied_foam_l1_classified_14$query
ko_rarefied_foam_l1_classified_14_presence_absence<-ko_rarefied_foam_l1_classified_14_presence_absence[!ko_rarefied_foam_l1_classified_14_presence_absence$query%in%c("K14138","K04480","K00193"),]
ko_rarefied_foam_l1_classified_14_presence_absence_longformat<-ko_rarefied_foam_l1_classified_14_presence_absence %>% gather(key= "sampleinfo",value="presence_absence", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l1_classified_14_presence_absence_longformat$home<-substr(ko_rarefied_foam_l1_classified_14_presence_absence_longformat$sampleinfo, 1, 6)
ko_rarefied_foam_l1_classified_14_presence_absence_longformat$day<-substr(ko_rarefied_foam_l1_classified_14_presence_absence_longformat$sampleinfo,12,12)
ko_rarefied_foam_l1_classified_14_presence_absence_longformat <- ko_rarefied_foam_l1_classified_14_presence_absence_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
ko_rarefied_foam_l1_classified_14_presence_absence_longformat <- ko_rarefied_foam_l1_classified_14_presence_absence_longformat %>% mutate(across('presence_absence', str_replace, '1', 'Present'))
ko_rarefied_foam_l1_classified_14_presence_absence_longformat <- ko_rarefied_foam_l1_classified_14_presence_absence_longformat %>% mutate(across('presence_absence', str_replace, '0', 'Absent'))
methanogenesis_ko<-ggplot(ko_rarefied_foam_l1_classified_14_presence_absence_longformat,aes(x=day, y=query))+geom_tile(aes(fill=presence_absence))+ labs(x="Day of the sampling", y=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12)) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size=12), legend.text = element_text(size = 12), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_manual(values = c("grey", '#bbdffb')) +
  guides(fill=guide_legend(title=NULL))+ggtitle("14_Methanogenesis")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/foam/methanogenesis_ko.png", plot=methanogenesis_ko, device = "png", height = 6)


ko_rarefied_foam_l2_classified_presence_absence <- as.data.frame(lapply(ko_rarefied_foam_l2_classified[,2:57], function(x) as.integer(!is.na(x) & x != 0)))
ko_rarefied_foam_l2_classified_presence_absence$L1<-ko_rarefied_foam_l2_classified$L1
ko_rarefied_foam_l2_classified_presence_absence$L2<-ko_rarefied_foam_l2_classified$L2
ko_rarefied_foam_l2_classified_presence_absence$l1_l2<-ko_rarefied_foam_l2_classified$l1_l2
ko_rarefied_foam_l2_classified_presence_absence$query<-ko_rarefied_foam_l2_classified$query
ko_rarefied_foam_l2_classified_presence_absence_longformat <- ko_rarefied_foam_l2_classified_presence_absence %>% gather(key= "sampleinfo",value="presence_absence", Home.1.day.1:Home.8.day.7)
ko_rarefied_foam_l2_classified_presence_absence_longformat$home<-substr(ko_rarefied_foam_l2_classified_presence_absence_longformat$sampleinfo, 1, 6)
ko_rarefied_foam_l2_classified_presence_absence_longformat$day<-substr(ko_rarefied_foam_l2_classified_presence_absence_longformat$sampleinfo,12,12)
ko_rarefied_foam_l2_classified_presence_absence_longformat <- ko_rarefied_foam_l2_classified_presence_absence_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))


#####community assembly
rela_importance<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/community_assembly/Exp1.ASV.MetaCrct.bin24.ProcessImportance_EachGroup_processed.csv", header = T, sep = "\t")
process_order_list<-c("Dispersal limitation", "Drift", "Homogenizing dispersal", "Heterogeneous selection", "Homogeneous selection")
rela_importance$process<-factor(rela_importance$process, levels = process_order_list)
rela_importance$importance_perc<-rela_importance$importance*100
rela_importance <- rela_importance %>% mutate(importance_perc = scales::percent(importance))
rela_importance_piechart<-ggplot(rela_importance,aes(x="",y=importance,fill=process))+geom_bar(stat="identity", width=1, color="white")+coord_polar("y", start=0)+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),panel.grid  = element_blank(),
        legend.title = element_text(size = 12, family = "Arial", colour = "black"),legend.text = element_text(size = 12, family = "Arial", colour = "black"), legend.position = "bottom")+ 
  theme(panel.background = element_rect(fill='white', colour = 'black'))+theme(panel.background = element_blank())+
  geom_text(aes(label = importance_perc),position = position_stack(vjust = 0.5), family = "Arial", colour = "black")

rela_importance_piechart<-ggplot(rela_importance,aes(x="",y=importance,fill=process))+geom_bar(stat="identity", width=1, color="white")+coord_polar("y", start=0)+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank(),panel.grid  = element_blank(),
        legend.title = element_text(size = 12, colour = "black"),legend.text = element_text(size = 12, colour = "black"), legend.position = "bottom")+ 
  theme(panel.background = element_rect(fill='white', colour = 'black'))+theme(panel.background = element_blank())+
  geom_text(aes(label = importance_perc),position = position_stack(vjust = 0.5), colour = "black")
ggsave("~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/community_assembly/Exp1.ASV.MetaCrct.bin24.ProcessImportance_EachGroup_piechart1.pdf", plot = rela_importance_piechart, device = "pdf", width = 10, height = 5)

turnover<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/community_assembly/Exp1.ASV.MetaCrct.bin24.ProcessImportance_EachTurnover_processed.csv")
turnover_reformat <- turnover %>% gather(key= "eco_process",value="importance", HeS:DR)
turnover_reformat <- turnover_reformat %>% mutate(across('home', str_replace, 'Home ', 'STL-'))
turnover_plot<-ggplot(turnover_reformat, aes(x = xlabel, y=importance,fill = eco_process)) +geom_bar(stat = "identity") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), strip.background = element_rect(fill = 'white')) + 
  theme(legend.position = "bottom", legend.text = element_text(size=12, family = "Arial", colour = "black"), 
        axis.text = element_text(size = 12, family = "Arial", colour = "black"),axis.title=element_text(size=12, family = "Arial", colour = "black"),
        strip.text = element_text(size = 12, family = "Arial", colour = "black"))+
  labs(y="Relative importance")+labs(x="Turnover between two consecutive days")+guides(fill=guide_legend(title="Ecologicol process",title.theme = element_text(size = 12, family = "Arial", colour = "black")))+
  scale_x_continuous(n.breaks = 6)+facet_wrap(~home, nrow = 2)

turnover_plot<-ggplot(turnover_reformat, aes(x = xlabel, y=importance,fill = eco_process)) +geom_bar(stat = "identity") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), strip.background = element_rect(fill = 'white')) + 
  theme(legend.position = "bottom", legend.text = element_text(size=12, colour = "black"), 
        axis.text = element_text(size = 12, colour = "black"),axis.title=element_text(size=12, colour = "black"),
        strip.text = element_text(size = 12, colour = "black"))+
  labs(y="Relative importance")+labs(x="Turnover between two consecutive days")+guides(fill=guide_legend(title="Ecologicol process",title.theme = element_text(size = 12, colour = "black")))+
  scale_x_continuous(n.breaks = 6)+facet_wrap(~home, nrow = 2)
ggsave("~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/community_assembly/turnover_barplot1.pdf",plot=turnover_plot,device = "pdf", width = 6, height = 5)

exp2_rela_importance<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/community_assembly/Exp2.ASV.240416.MetaCrct.bin24.ProcessImportance_EachGroup_processed.csv")
exp2_rela_importance$process<-factor(exp2_rela_importance$process, levels = process_order_list)
exp2_rela_importance$type<-factor(exp2_rela_importance$type,levels = c("Bathtub faucet stagnant", "Kitchen faucet stagnant", "Kitchen faucet fresh"))
exp2_plot<-ggplot(exp2_rela_importance, aes(x = type, y=importance, fill = process)) +geom_bar(stat = "identity") +
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme(legend.position = "right", legend.text = element_text(size=12), axis.text = element_text(size = 12),axis.title=element_text(size=12),strip.text = element_text(size = 12))+
  labs(y="Relative importance")+labs(x="Sample type")+guides(fill=guide_legend(title="Ecologicol process",title.theme = element_text(size = 12)))
ggsave("~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/community_assembly/exp2_barplot_2source.png",plot=exp2_plot,device = "png",width = 8, height = 6)


######amr rpkm pcoa################################
amr_gene_rpkm_rarefied<-
shower_mpa_species_forbc<-as.data.frame(shower_mpa_species[,2:57])
shower_mpa_species_forbc <- shower_mpa_species_forbc[, order(names(shower_mpa_species_forbc))]
shower_mpa_species_forbc_bray<-vegdist(t(shower_mpa_species_forbc), method = "bray")
shower_mpa_species_forbc_bray_pcoa<-pcoa(shower_mpa_species_forbc_bray)
shower_mpa_species_forbc_bray_pcoa_data.scores = as.data.frame(shower_mpa_species_forbc_bray_pcoa$vectors[,1:2])
shower_mpa_species_forbc_bray_pcoa_data.scores$home<-substr(rownames(shower_mpa_species_forbc_bray_pcoa_data.scores),1,6)
shower_mpa_species_forbc_bray_pcoa_data.scores <- shower_mpa_species_forbc_bray_pcoa_data.scores %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
sort(shower_mpa_species_forbc_bray_pcoa$values$Relative_eig)
shower_mpa_species_bc_pcoa_plot<-ggplot(shower_mpa_species_forbc_bray_pcoa_data.scores, aes(x = Axis.1, y = Axis.2)) +
  geom_point(size=5, aes(color=home)) +
  theme(axis.text = element_text(colour = "black", size = 12, face = "bold")) +
  theme(legend.text = element_text(colour = "black", size = 15, face = "bold"), legend.position = "top")+
  theme(axis.title = element_text(colour = "black", size = 15, face = "bold"))+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(legend.title = element_text(colour = "black", size = 15, face = "bold"))+
  labs(x="Axis.1 [31.5%]", y="Axis.2 [19%]")+
  scale_color_manual(values = color_custom)+guides(color=guide_legend(title="Household\nidentity"))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_species_bc_pcoa.png",plot =shower_mpa_species_bc_pcoa_plot,device = "png", width = 6.5, height = 6)
###############################################


important_amr<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/amr/last_resort_resistant_genes.csv", header = T)
important_amr_longformat<-important_amr %>% gather(key = "sampleinfo", value = "rpkm", Home.5.day.4:Home.5.day.7)
important_amr_longformat$home<-substr(important_amr_longformat$sampleinfo,1,6)
important_amr_longformat<-important_amr_longformat %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
important_amr_longformat$day<-substr(important_amr_longformat$sampleinfo,12,12)
important_amr_longformat$rpkm1<-ifelse(important_amr_longformat$rpkm==0, NA, important_amr_longformat$rpkm)
important_amr_rpkm_plot<-ggplot(important_amr_longformat,aes(x=day, y=gene))+geom_tile(aes(fill=rpkm1))+ labs(x="Day of the sampling", y=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12, family = "Arial", colour = "black")) +
  theme(axis.text = element_text(size = 12, family = "Arial", colour = "black"), axis.title = element_text(size=12, family = "Arial", colour = "black"), legend.text = element_text(size = 12, family = "Arial", colour = "black"), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 50), breaks=seq(0,50,by=10), low='#bbdffb', high='#0747a1', na.value="white") +
  guides(fill=guide_legend(title="RPKM", nrow = 1))

important_amr_rpkm_plot<-ggplot(important_amr_longformat,aes(x=day, y=gene))+geom_tile(aes(fill=rpkm1))+ labs(x="Day of the sampling", y=NULL) +
  theme(panel.background = element_rect(fill='white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12, colour = "black")) +
  theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size=12, colour = "black"), legend.text = element_text(size = 12, colour = "black"), legend.position = "bottom")+
  facet_wrap(~home,nrow = 1)+theme(panel.spacing = unit(0.1, "lines"))+
  scale_fill_continuous(limits=c(1, 50), breaks=seq(0,50,by=10), low='#bbdffb', high='#0747a1', na.value="white") +
  guides(fill=guide_legend(title="RPKM", nrow = 1))
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/amr/important_amr2.pdf", plot=important_amr_rpkm_plot, device = "pdf", height = 2.5, width = 10)

amr_class_rpkm_relabun_1<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/amr/amr_class_rpkm_relabun_1.csv",header = T)
rownames(amr_class_rpkm_relabun_1)<-amr_class_rpkm_relabun_1$X
amr_class_rpkm_relabun_1$X<-NULL
amr_class_rpkm_relabun_1_forplot <- amr_class_rpkm_relabun_1 %>% gather(key= "sampleinfo",value="relabun", Home.5.day.4:Home.5.day.7)
amr_class_rpkm_relabun_1_forplot$home = substr(amr_class_rpkm_relabun_1_forplot$sampleinfo,1,6)
amr_class_rpkm_relabun_1_forplot<-amr_class_rpkm_relabun_1_forplot %>% mutate(across('home', str_replace, 'Home.', 'STL-'))
amr_class_rpkm_relabun_1_forplot$day = substr(amr_class_rpkm_relabun_1_forplot$sampleinfo,12,12)
amr_class_rpkm_relabun_1_forplot$day<-as.integer(amr_class_rpkm_relabun_1_forplot$day)
amr_class_rpkm_relabun_1_forplot<-transform(amr_class_rpkm_relabun_1_forplot, class=factor(class,levels = rev(amr_class_rpkm_relabun_1$class)))
color_custom9<-c("#D3D3D3","#68affc", "#0362a0", "#60e9dc", "#2f937a", "#76dd78", "#20502e", "#cfdd73", "#39970e")
amr_class_rpkm_relabun_1_plot <- ggplot() +geom_area(aes(x = day, y=relabun,fill = class), data = amr_class_rpkm_relabun_1_forplot,position = 'stack') + 
  facet_wrap(~home,nrow = 1)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12, family = "Arial", colour = "black"))+
  guides(fill=guide_legend(nrow = 3,title="AMR class",title.theme = element_text(size = 12, family = "Arial", colour = "black")))+
  scale_fill_manual(values = color_custom9) + 
  theme(legend.text = element_text(size=12, family = "Arial", colour = "black"), legend.position = "bottom",axis.text = element_text(size = 12, family = "Arial", colour = "black"),axis.title=element_text(size=12, family = "Arial", colour = "black"))+
  ylab("Relative abundance")+xlab("Day of the sampling")+scale_x_continuous(n.breaks = 8)

amr_class_rpkm_relabun_1_plot <- ggplot() +geom_area(aes(x = day, y=relabun,fill = class), data = amr_class_rpkm_relabun_1_forplot,position = 'stack') + 
  facet_wrap(~home,nrow = 1)+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'), strip.background = element_rect(fill = 'white'), strip.text = element_text(size = 12, colour = "black"))+
  guides(fill=guide_legend(nrow = 3,title="ARG class",title.theme = element_text(size = 12, colour = "black")))+
  scale_fill_manual(values = color_custom9) + 
  theme(legend.text = element_text(size=12, colour = "black"), legend.position = "bottom",axis.text = element_text(size = 12, colour = "black"),axis.title=element_text(size=12, colour = "black"))+
  ylab("Relative abundance")+xlab("Day of the sampling")+scale_x_continuous(n.breaks = 8)
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/amr/amr_class_rpkm.pdf", plot=amr_class_rpkm_relabun_1_plot, device = "pdf", height = 4, width = 9)


######amr host
mag_amr_list <- list.files(path="~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/amr", pattern="*nocov",full.names=TRUE) 
mag_amr_filtered_cat<-data.frame(matrix(ncol = 15, nrow = 0))
colnames(mag_amr_filtered_cat)<-paste0("V", 1:15)
for (i in 1:length(mag_amr_list)) {
  mag_amr<-read.csv(mag_amr_list[i], sep = "\t", header = F)
  if (nrow(mag_amr) > 0) {
    mag_amr$V14 <- mag_amr$V4 / mag_amr$V13
    mag_amr_filtered<-subset(mag_amr, mag_amr$V14>0.7)
    if (nrow(mag_amr_filtered) >0){
      mag_amr_filtered$V15<-rep(basename(mag_amr_list[i]), nrow(mag_amr_filtered))
      mag_amr_filtered_cat<-rbind(mag_amr_filtered_cat,mag_amr_filtered)
    }
  }
}
mag_amr_filtered_cat$V15 <- gsub("_megares_nocov", "", mag_amr_filtered_cat$V15)
gtdbtk<-read.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/amr/gtdbtk.bac120.summary.tsv", header = T, sep = "\t")
gtdbtk<-gtdbtk[,1:2]
colnames(gtdbtk)[1]<-"V15"
mag_amr_filtered_cat1<-merge(mag_amr_filtered_cat, gtdbtk, by="V15")
amr_host<-mag_amr_filtered_cat1[,c("V15","classification")]
amr_host_unique<-unique(amr_host)
write.csv(file = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/amr/mag_amr.csv",mag_amr_filtered_cat1)



###### walking distance between homes
# Your Google API key
# List of addresses
home8_addresses <- read.csv(file = "~/LinsData/011520homesandriver/shower_16S_paper/home_address8_test.csv", header = F, sep = "\t")
addresses <- home8_addresses$V1
addresses56 <- unlist(lapply(addresses, rep, times = 7))
api_key <- "AIzaSyAXJmn3TaULLm7i--uza3g6TBXO1Rowvvs"
register_google(key = api_key)

# Initialize an empty matrix to store distances
distance_matrix56 <- matrix(nrow = length(addresses56), ncol = length(addresses56),
                          dimnames = list(addresses56, addresses56))

# Loop through each pair of addresses and fetch the walking distance
for (i in 1:length(addresses56)) {
  for (j in 1:length(addresses56)) {
    if (i != j) { # Skip calculating distance from the same address to itself
      distance_result <- mapdist(from = addresses56[i], to = addresses56[j], mode = "walking", key = api_key)
      distance_matrix56[i, j] <- as.numeric(as.character(distance_result$km)) # Storing distance in km
    }
  }
}

# Print the distance matrix
print(distance_matrix)
colnames(distance_matrix)<-home8_addresses$V2
rownames(distance_matrix)<-home8_addresses$V2
distance_longformat56<-matrixConvert(distance_matrix56, colname = c("home1", "home2", "walking_dist"))
bray_16s_longformat<-matrixConvert(home122420_8_merge_nomito_nochloro_noeuk_nounassign_rarefied.vegdist.bray, colname = c("home1_16s", "home2_16s", "bc_dist_16s"))
bray_16s_geodist<-cbind(distance_longformat56,bray_16s_longformat)
bray_16s_geodist_plot<-ggplot(data = bray_16s_geodist, aes(x=walking_dist, y=bc_dist_16s))+geom_point(size=1)+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 12))+
  xlab("Pedestrian distance between homes (km)")+ylab("Bray-Curtis dissimilarity in ASV profiles")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_16s_bc_geodist.png", bray_16s_geodist_plot, device = "png", width = 4, height = 4)
summary(lm(bc_dist_16s~walking_dist,data = bray_16s_geodist))

distance_longformat$home<-paste(distance_longformat$home1, distance_longformat$home2, sep = ".")
shower_mpa_bc<-shower_mpa_species_forbc_bray_long_format
shower_mpa_bc$home1<-substr(shower_mpa_bc$mpa_sp1,1,6)
shower_mpa_bc$home2<-substr(shower_mpa_bc$mpa_sp2,1,6)
shower_mpa_bc_betweenhome<-subset(shower_mpa_bc, shower_mpa_bc$home1!=shower_mpa_bc$home2)
shower_mpa_bc_betweenhome$home<-paste(shower_mpa_bc_betweenhome$home1, shower_mpa_bc_betweenhome$home2, sep = ".")
shower_mpa_bc_betweenhome <- shower_mpa_bc_betweenhome %>% left_join(distance_longformat, by = "home")
shower_mpa_bc_geodist_plot<-ggplot(data = shower_mpa_bc_betweenhome, aes(x=walking_dist, y=mpa_bc_dist))+geom_point(size=1)+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(axis.title = element_text(size = 12), axis.text = element_text(size = 12))+
  xlab("Pedestrian distance between homes (km)")+ylab("Bray-Curtis dissimilarity in speces profiles")
ggsave(filename = "~/LinsData/shower_novaseq_gtac_2ndsequencing/NW_review/shower_mpa_bc_geodist.png", shower_mpa_bc_geodist_plot, device = "png", width = 4, height = 4)
summary(lm(mpa_bc_dist~walking_dist,data = shower_mpa_bc_betweenhome))


######bray TOC relationship
bray_toc_plot<-ggplot(data = toc_bc, aes(x=toc_dist, y=bc_dist))+geom_point(size=6)+
  theme(panel.background = element_rect(fill='white', colour = 'black'))+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 12))+
  xlab("Euclidean distance in TOC")+ylab("Bray Curtis distance in species profile")
ggsave(filename = "~/LinsData/011520homesandriver/shower_16S_paper/bray_toc.png", bray_toc_plot, device = "png", width = 4, height = 4)
summary(lm(bc_dist~toc_dist,data = toc_bc))


#####MRM
home122420_8_merge_nomito_nochloro_noeuk_nounassign_genus_rarefied_bc<-vegdist(otu_table(home122420_8_merge_nomito_nochloro_noeuk_nounassign_genus_rarefied), method = "bray")
cl_dist1<-dist(sample_data(home122420_8_merge_nomito_nochloro_noeuk_nounassign_rarefied)$chlorine)
temp_dist1<-dist(sample_data(home122420_8_merge_nomito_nochloro_noeuk_nounassign_rarefied)$temp)
sample_data(home122420_8_merge_nomito_nochloro_noeuk_nounassign_rarefied)$age<-c(rep(127,7), rep(112,7), rep(104,7), rep(101,7), rep(99,7), rep(57,7), rep(95,7), rep(32,7))
age_dist1<-dist(as.data.frame(sample_data(home122420_8_merge_nomito_nochloro_noeuk_nounassign_rarefied)$age))
geodist1<-as.dist(distance_matrix56)
MRM(home122420_8_merge_nomito_nochloro_noeuk_nounassign_genus_rarefied_bc ~ cl_dist1 + temp_dist1 + geodist1 + age_dist1, nperm=999)
MRM(home122420_8_merge_nomito_nochloro_noeuk_nounassign_genus_rarefied_bc ~ cl_dist1 + temp_dist1 + geodist1, nperm=999)
MRM(home122420_8_merge_nomito_nochloro_noeuk_nounassign_genus_rarefied_bc ~ cl_dist1 + geodist1, nperm=999)
