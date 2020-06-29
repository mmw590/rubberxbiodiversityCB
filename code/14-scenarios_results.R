#### rubberxbiodiversityCB 
#### 14-scenarios_results.R #### 

library(dplyr)
library(tidyr)
library(cowplot)
library(ggplot2)
library(data.table)
library(forcats) #for fct_relevel

stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

# 1. Figure 3 - Scenarios Results ####
# 1.1 Country-Restricted Output ####
scen_dflist <- list.files('output/country_scenario/', full.names=TRUE)
scen_dflist <- grep("countrysim_scen_df", scen_dflist, value=TRUE)
scen_dflist <- grep("vulnA", scen_dflist, value=TRUE, invert=TRUE)
scen_dflist
scen_dflist <- lapply(scen_dflist, fread)

scen_df_longC <- rbindlist(scen_dflist)

scen_df_longC <- scen_df_longC %>% rename(lost_area = V1, rangeloss_sum=V2, pctrangeloss_sum=V3, nspplostrange=V4, nspplostrange0.10=V5, cumsuit=V6)
scen_df_longC <- scen_df_longC %>% mutate(rangelossMha_mean_allspp=rangeloss_sum/4264*0.01) #Manually calculate the mean % range loss across all spp
scen_df_longC <- scen_df_longC %>% mutate(pctrangeloss_mean_allspp=pctrangeloss_sum/4264) #Manually calculate the mean % range loss across all spp


scen_df_longC <- scen_df_longC %>% mutate(scenario = rep(c('1-Production', '2-Conservation','3a-Compromise Biodiversity', '3b-Compromise Suitability', '3c-Compromise Both'), each=nrow(scen_dflist[[1]]) ), lost_area = lost_area*0.01, rangeloss_sum=rangeloss_sum*0.01) # lost_area in Mha

head(scen_df_longC) 
table(scen_df_longC$scenario)


# + Numbers for Results (country) ####
# Comparing the average rubber bioclimatic suitability of converted land area across the different scenarios
cumsuit1 <- scen_df_longC %>% group_by(scenario) %>% summarize(cumsuit.mean=mean(cumsuit), cumsuit.se=stderr(cumsuit))

scen_df_longC %>% group_by(scenario) %>% summarize(rangeloss_mean=mean(rangeloss_sum, na.rm=TRUE), rangeloss_max=max(rangeloss_sum, na.rm=TRUE))
scen_df_longC %>% group_by(scenario) %>% summarize(pctrangeloss_mean=mean(pctrangeloss_sum, na.rm=TRUE), pctrangeloss_max=max(pctrangeloss_sum, na.rm=TRUE))



#### 1.2 Unrestricted simulations output ####
scen_dflist <- list.files('output/', full.names=TRUE)
scen_dflist <- grep("scen_df", scen_dflist, value=TRUE)
scen_dflist <- grep("vulnA", scen_dflist, value=TRUE, invert=TRUE)
scen_dflist
scen_dflist <- lapply(scen_dflist, fread)

scen_df_long <- rbindlist(scen_dflist)

scen_df_long <- scen_df_long %>% rename(lost_area = V1, rangeloss_sum=V2, pctrangeloss_sum=V3, nspplostrange=V4, nspplostrange0.10=V5, cumsuit=V6)
scen_df_long <- scen_df_long %>% mutate(rangelossMha_mean_allspp=rangeloss_sum/4264*0.01) #Manually calculate the mean % range loss across all spp
scen_df_long <- scen_df_long %>% mutate(pctrangeloss_mean_allspp=pctrangeloss_sum/4264) #Manually calculate the mean % range loss across all spp


scen_df_long <- scen_df_long %>% mutate(scenario = rep(c('1-Production', '2-Conservation','3a-Compromise Biodiversity', '3b-Compromise Suitability', '3c-Compromise Both'), each=nrow(scen_dflist[[1]]) ), lost_area = lost_area*0.01, rangeloss_sum=rangeloss_sum*0.01) # lost_area in Mha

head(scen_df_long) 
table(scen_df_long$scenario)



# + Numbers for Results (Comparison btw country and unrestricted) ####
cumsuit2 <- scen_df_long %>% group_by(scenario) %>% summarize(cumsuit.mean=mean(cumsuit), cumsuit.se=stderr(cumsuit))

scen_df_long %>% group_by(scenario) %>% summarize(rangeloss_mean=mean(rangeloss_sum, na.rm=TRUE), rangeloss_max=max(rangeloss_sum, na.rm=TRUE))
scen_df_long %>% group_by(scenario) %>% summarize(pctrangeloss_mean=mean(pctrangeloss_sum, na.rm=TRUE), pctrangeloss_max=max(pctrangeloss_sum, na.rm=TRUE))

# Average suitability of converted land in Scenarios 1 and 3a-c 
(cumsuit2$cumsuit.mean-cumsuit1$cumsuit.mean)/cumsuit1$cumsuit.mean*100


# Relative to country-restricted simulations, unrestricted simulations led to small-moderate reductions in species impacts under Scenario 1 (Production), but did not change the trends for Scenario 2 (Conservation) (Figure 3BC, EF). 

check <- scen_df_long %>% filter(lost_area %in% c(3.90)) %>% dplyr::select(lost_area, cumsuit, rangelossMha_mean_allspp, nspplostrange0.10, scenario)

checkC <- scen_df_longC %>% filter(lost_area %in% c(3.90)) %>% dplyr::select(lost_area, cumsuit, rangelossMha_mean_allspp, nspplostrange0.10, scenario)

checkC$cumsuit-check$cumsuit
(checkC$rangelossMha_mean_allspp-check$rangelossMha_mean_allspp)
(checkC$rangelossMha_mean_allspp-check$rangelossMha_mean_allspp)/checkC$rangelossMha_mean_allspp

checkC$nspplostrange0.10; check$nspplostrange0.10
(checkC$nspplostrange0.10-check$nspplostrange0.10)
(checkC$nspplostrange0.10-check$nspplostrange0.10)/checkC$nspplostrange0.10




##### Fig3 A-C (Country) ####
theme_set(theme_cowplot(font_size=8))
scen_df_plotC <- scen_df_longC  

rect1 <- data.frame (xmin=1.66, xmax=6.70, ymin=-Inf, ymax=Inf)
rect2 <- data.frame (xmin=2.45, xmax=3.90, ymin=-Inf, ymax=Inf)

# Avg suitability 
scen_df_plotC[scen_df_plotC$lost_area==0, ]$cumsuit <- NA

scenfigC_cumsuit <- ggplot(data=scen_df_plotC, aes(x=lost_area, y=cumsuit,  group=scenario, colour=scenario, linetype=scenario)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE)  +  
  geom_line() +
  scale_color_manual(values=c("blue", "red", "purple", "purple", "purple", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('Average suitability of converted cells    ') +
  theme(legend.position="none") +
  ylim(c(0,1)) 


# Sum of % range loss
max(scen_df_plotC$rangeloss_sum)

scenfigC <- ggplot(data=scen_df_plotC, aes(x=lost_area, y=rangeloss_sum,  group=scenario, colour=scenario, linetype=scenario)) + 
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +  
  geom_line() +
  scale_color_manual(values=c("blue", "red", "purple", "purple", "purple", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('Cumulative forested range loss (Mha)  ') +
  theme(legend.position="none") +
  ylim(c(0,max(scen_df_plotC$rangeloss_sum))) # +
# geom_vline(xintercept=c(2.7, 3, 3.5), color="red")


# No. of affected spp
scenfigC_affspp <- ggplot(data=scen_df_plotC, aes(x=lost_area, y=nspplostrange0.10,  group=scenario, colour=scenario, linetype=scenario)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) + 
  geom_line() +
  scale_color_manual(values=c("blue", "red", "purple", "purple", "purple", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('No. of affected species') +
  theme(legend.position="none") +
  ylim(c(0,100)) # +
#  geom_vline(xintercept=c(2.7, 3, 5.7), color="red")

#max(scen_df_plotC$nspplostrange0.10)


#### Fig3 D-F (Unrestricted) ####
scen_df_plot <- scen_df_long  

# Avg suitability 
scen_df_plot[scen_df_plot$lost_area==0, ]$cumsuit <- NA

scenfig_cumsuit <- ggplot(data=scen_df_plot, aes(x=lost_area, y=cumsuit,  group=scenario, colour=scenario, linetype=scenario)) +  
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_line() +
  scale_color_manual(values=c("blue", "red", "purple", "purple", "purple", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('Average suitability of converted cells    ') +
  theme(legend.position="none") +
  ylim(c(0,1)) + xlim(c(0,7))


# Sum of Mha range loss
scenfig <- ggplot(data=scen_df_plot, aes(x=lost_area, y=rangeloss_sum,  group=scenario, colour=scenario, linetype=scenario)) + 
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +  
  geom_line() +
  scale_color_manual(values=c("blue", "red", "purple", "purple", "purple", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('Cumulative forested range loss (Mha)  ') +
  theme(legend.position="none") +
  ylim(c(0,max(scen_df_plotC$rangeloss_sum))) # +
# geom_vline(xintercept=c(2.7, 3, 3.5), color="red")


# No. of affected spp
scenfig_affspp <- ggplot(data=scen_df_plot, aes(x=lost_area, y=nspplostrange0.10,  group=scenario, colour=scenario, linetype=scenario)) +
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) + 
  geom_line() +
  scale_color_manual(values=c("blue", "red", "purple", "purple", "purple", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('No. of affected species') +
  theme(legend.position=c(0, 0.85), legend.title = element_blank(), legend.spacing=unit(0, 'mm'), legend.key.height = unit(0.1, 'cm'))+
  ylim(c(0,100))   #+
 # geom_vline(xintercept=c(2.7, 3, 5.7), color="red")


#### comb fig (2x3) ####
#scenfig_comb <- plot_grid(scenfig_cumsuit, scenfig, scenfig_affspp, scenfigC_cumsuit, scenfigC, scenfigC_affspp, ncol=3, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size=8  )

scenfig_comb <- plot_grid(scenfigC_cumsuit, scenfigC, scenfigC_affspp, scenfig_cumsuit, scenfig, scenfig_affspp,  ncol=3, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size=8  )

cowplot::save_plot("output/results/fig3.png", scenfig_comb, base_width=6.85, base_height = 6.85/3*2, dpi=300)






###### 2. Figure 4 - Species profile #####

# Load spp list
suit_vuln_spp <- fread('output/suit_vuln_scenario_combspp.csv') #few seconds
sppnames <- data.frame(SCINAME=names(suit_vuln_spp)) 
rm(suit_vuln_spp)

# Load IUCN red list
forspp_corr <- read.csv("data/forestdependentspecies_iucnredlist2019.csv")
head(forspp_corr)

forspp_corr2 <- forspp_corr %>% distinct(SCINAME, category, class) 
nrow(forspp_corr2) == n_distinct(forspp_corr$SCINAME) #check if TRUE

sppnames_iucn <- left_join(sppnames, forspp_corr2)
sppnames_iucn %>% group_by(category) %>% summarize(n=n())



### + Load spprangeloss tbls from simulations & merge with sppnames/iucn categories ####
dflist <- list.files('output/country_scenario', full.names=TRUE)
dflist <- grep('spprangelosstbl_scen', dflist, value=TRUE)
dflist
dflist <- lapply(dflist, fread)

spprangeloss_dfC <- rbindlist(dflist)

spprangeloss_dfC <- spprangeloss_dfC %>% mutate(scenario = rep(c('1-Production', '2-Conservation','3a-Compromise Biodiversity', '3b-Compromise Suitability', '3c-Compromise Both'), each=nrow(dflist[[1]]) )) 

spprangeloss_dfC <- spprangeloss_dfC %>% mutate(percent_rangeloss_low = (1-(range_remaining_low/ori_range))*100, percent_rangeloss_high = (1-(range_remaining_high/ori_range))*100,
                                              percent_rangeloss_6.70 = (1-(range_remaining_high_EWT/ori_range))*100)

spprangeloss_dfC$SCINAME <- rep(sppnames_iucn$SCINAME, length(dflist) )
spprangeloss_dfC$iucn_category <- rep(sppnames_iucn$category,length(dflist) )
spprangeloss_dfC$class <- rep(sppnames_iucn$class,length(dflist) )

spprangeloss_dfC$iucn_category <- fct_relevel(spprangeloss_dfC$iucn_category, "CR", "EN", "VU", "NT",  "LC", "DD")



### + Numbers for Results ####
head(spprangeloss_dfC)

(spprangeloss_sum_10 <- spprangeloss_dfC %>% group_by(scenario) %>% 
    summarize(naffspp10_low=sum(percent_rangeloss_low>=10), naffspp10_high=sum(percent_rangeloss_high>=10)) )

( spprangeloss_sum_50 <- spprangeloss_dfC %>% group_by(scenario) %>% 
    summarize(naffspp50_low=sum(percent_rangeloss_low>=50), naffspp_high50=sum(percent_rangeloss_high>=50)) )


# Taxa breakdown
( spprangeloss_sum_10 <- spprangeloss_dfC %>% group_by(scenario, class) %>% 
    summarize(naffspp_high10=sum(percent_rangeloss_high>=10)) )

( spprangeloss_sum_50 <- spprangeloss_dfC %>% group_by(scenario, class) %>% 
    summarize(naffspp_high50=sum(percent_rangeloss_high>=50)) )




###### + Fig4 Histogram for paper ####
unique(spprangeloss_df$scenario)

hist_spprangeloss_df_0_highC <- spprangeloss_dfC %>% filter(percent_rangeloss_high>=10)  %>% 
  mutate(rangeloss.class = cut(percent_rangeloss_high, breaks=seq(10,105,5),include.lowest = TRUE, right = FALSE, labels=c(seq(10.1,100.1,5)) ) , orirange.class=cut(ori_range, breaks=c(0,10,100,1000,10000,100000), labels=c("0-0.1 Mha", "0.1-1 Mha", "1-10 Mha", "10-100 Mha", "100-1000 Mha")) ) %>% filter(scenario %in% c("1-Production", "3a-Compromise Biodiversity"))

hist_spprangeloss_df_0_highC$rangeloss.class <- as.numeric(as.character(hist_spprangeloss_df_0_highC$rangeloss.class))

head(hist_spprangeloss_df_0_highC)

theme_set(theme_cowplot(font_size=8))
rect4 <- data.frame (xmin=50, xmax=Inf, ymin=0, ymax=Inf)

str(hist_spprangeloss_df_0_highC$scenario)
as.integer(as.factor(hist_spprangeloss_df_0_highC$scenario))

hist_rangeloss_scen_high <- ggplot(hist_spprangeloss_df_0_highC,aes(rangeloss.class, fill=iucn_category)) + 
  geom_rect(data=rect4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_histogram(binwidth=5, boundary=0) + 
  facet_wrap(scenario ~., ncol=1) +
  scale_x_continuous(breaks=seq(10,100,10))  +
  theme(legend.position = c(0.45, 0.85), legend.direction='vertical', 
        legend.title=element_text(size=9, face="bold"), legend.text=element_text(size=9), 
        axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), 
        axis.text = element_text(size=8), strip.text = element_text(size = 10) ) +
  ylab('No. of species affected by 3.90 Mha of expansion') + xlab(' ') +
  labs(fill="Red List Status")


hist_rangeloss_scen_tax_high <- ggplot(hist_spprangeloss_df_0_highC,aes(rangeloss.class, fill=class)) + 
  geom_rect(data=rect4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_histogram(binwidth=5, boundary=0) + 
  facet_wrap(scenario ~., ncol=1) +
  scale_x_continuous(breaks=seq(10, 100, 10))  +
  theme(legend.position = c(0.45, 0.88),         
        legend.title=element_text(size=9, face="bold"), legend.text=element_text(size=9),  
        axis.title.y = element_text(size=0), axis.title.x = element_text(size=10), 
        axis.text = element_text(size=8), strip.text = element_text(size = 10) ) +
  ylab('') + xlab('% range loss') +
  labs(fill="Taxonomic class")

hist_rangeloss_scen_range_high <- 
  ggplot(hist_spprangeloss_df_0_highC,  aes(rangeloss.class, fill=orirange.class)) + 
  geom_rect(data=rect4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_histogram(binwidth=5, boundary=0) + 
  facet_wrap(scenario ~., ncol=1) +
  scale_x_continuous(breaks=seq(10, 100, 10))  +
  theme(legend.position = c(0.45, 0.86),
        legend.title=element_text(size=9, face="bold"), legend.text=element_text(size=9), 
        axis.title.y = element_text(size=0), axis.title.x = element_text(size=10), 
        axis.text = element_text(size=8), strip.text = element_text(size = 10) ) +
  ylab('') + xlab(' ') +
  labs(fill="Original range size")

scen_histC <- plot_grid( hist_rangeloss_scen_high, hist_rangeloss_scen_tax_high, hist_rangeloss_scen_range_high, ncol=3)


### plot labels 
scen_histC_labs <- scen_histC + draw_plot_label(c("A", "B", "C", "D", "E","F"), x=rep(c(0.03,0.34,0.67),2), y=rep(c(1,0.55),each=3), fontface="bold", size=10)

cowplot::save_plot("output/results/fig4.png", scen_histC_labs, base_width=6.85, base_height = 4, dpi=300)





#### + specific spp that get hammered ####
checkC <- spprangeloss_dfC %>%  filter(percent_rangeloss_high>=50) %>% group_by(scenario, percent_rangeloss_high, SCINAME) #many small ranged spp affected by aoc vulnA



