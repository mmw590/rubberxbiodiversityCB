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

# 1. Combine all scen_df output ####
# 1.1 Country-Restricted Output ####
scen_dflist <- list.files('output/country_scenario/', full.names=TRUE)
scen_dflist <- grep("countrysim_scen_df", scen_dflist, value=TRUE)
scen_dflist <- grep("test", scen_dflist, value=TRUE, invert=TRUE)
scen_dflist
scen_dflist <- lapply(scen_dflist[c(5,1:4)], fread)

scen_df_longC <- rbindlist(scen_dflist)

scen_df_longC <- scen_df_longC %>% rename(lost_area = V1, rangeloss_sum=V2, pctrangeloss_sum=V3, nspplostrange=V4, nspplostrange0.10=V5, cumsuit=V6)
scen_df_longC <- scen_df_longC %>% mutate(rangelossMha_mean_allspp=rangeloss_sum/4264*0.01) #Manually calculate the mean % range loss across all spp
scen_df_longC <- scen_df_longC %>% mutate(pctrangeloss_mean_allspp=pctrangeloss_sum/4264) #Manually calculate the mean % range loss across all spp


scen_df_longC <- scen_df_longC %>% mutate(scenario = rep(c('1-Production', '2-Conservation (all spp)','3a-Comp. Biodiv. (all spp)', '3b-Comp. Suit. (all spp)', '3c-Comp. Both (all spp)'), each=nrow(scen_dflist[[1]]) ), lost_area = lost_area*0.01, rangeloss_sum=rangeloss_sum*0.01) # lost_area in Mha

head(scen_df_longC) 
table(scen_df_longC$scenario)


# + Numbers for Results (country) ####
# Comparing the average rubber suitability of converted land across the different scenarios for the country-restricted simulations, Scenario 2 (Conservation) only converted land with very low suitability (mean ± 1SE for first 7 Mha converted = 0.0063 ± 0.0006; Figure III.A). The conservation scenario is thus impractical for rubber expansion, with poor suitability leading to lower yields and higher land area demands to meet production targets. While not as high as the production scenario (0.629 ± 0.012), the average suitability of land converted in compromise scenarios were moderately high (3a = 0.487 ± 0.008; 3b = 0.484 ± 0.007; 3c = 0.487 ± 0.008; Figure III.A), suggesting higher acceptability to rubber producers.  
cumsuit1 <- scen_df_longC %>% group_by(scenario) %>% summarize(cumsuit.mean=mean(cumsuit), cumsuit.se=stderr(cumsuit))

scen_df_longC %>% group_by(scenario) %>% summarize(rangeloss_mean=mean(rangeloss_sum, na.rm=TRUE), rangeloss_max=max(rangeloss_sum, na.rm=TRUE))
scen_df_longC %>% group_by(scenario) %>% summarize(rangeloss_mean=mean(pctrangeloss_sum, na.rm=TRUE), rangeloss_max=max(pctrangeloss_sum, na.rm=TRUE))


#### 1.2 Unrestricted simulations output ####
scen_dflist <- list.files('output/', full.names=TRUE)
scen_dflist <- grep("scen_df", scen_dflist, value=TRUE)
scen_dflist
scen_dflist <- lapply(scen_dflist[c(5,1:4)], fread)
scen_df_long <- rbindlist(scen_dflist)

scen_df_long <- scen_df_long %>% rename(lost_area = V1, rangeloss_sum=V2, pctrangeloss_sum=V3, nspplostrange=V4, nspplostrange0.10=V5, cumsuit=V6)
scen_df_long <- scen_df_long %>% mutate(rangelossMha_mean=rangeloss_sum/nspplostrange*100) 
scen_df_long <- scen_df_long %>% mutate(pctrangeloss_mean=pctrangeloss_sum/nspplostrange*100) #Manually calculate the mean % range loss of only spp that lost any amt of range
scen_df_long <- scen_df_long %>% mutate(rangelossMha_mean_allspp=rangeloss_sum/4264*0.01) #Manually calculate the mean % range loss across all spp
scen_df_long <- scen_df_long %>% mutate(pctrangeloss_mean_allspp=pctrangeloss_sum/4264) #Manually calculate the mean % range loss across all spp


scen_df_long <- scen_df_long %>% mutate(scenario = rep(c('1-Production', '2-Conservation (all spp)','3a-Comp. Biodiv. (all spp)', '3b-Comp. Suit. (all spp)', '3c-Comp. Both (all spp)'), each=nrow(scen_dflist[[1]]) ), lost_area = lost_area*0.01, rangeloss_sum=rangeloss_sum*0.01) # lost_area in Mha

head(scen_df_long) 
table(scen_df_long$scenario)


# + Numbers for Results ####
cumsuit2 <- scen_df_long %>% group_by(scenario) %>% summarize(cumsuit.mean=mean(cumsuit), cumsuit.se=stderr(cumsuit))

scen_df_long %>% group_by(scenario) %>% summarize(rangeloss_mean=mean(rangelossMha_mean, na.rm=TRUE), rangeloss_max=max(rangeloss_mean, na.rm=TRUE))
scen_df_long %>% group_by(scenario) %>% summarize(rangeloss_mean=mean(pctrangeloss_sum, na.rm=TRUE), rangeloss_max=max(pctrangeloss_sum, na.rm=TRUE))


# Average suitability of converted land was 6% and 17-19%% higher under Scenarios 1 and 3a-c in unrestricted simulations (Figure III.D). 
(cumsuit2$cumsuit.mean-cumsuit1$cumsuit.mean)/cumsuit1$cumsuit.mean*100
#6%, 0.6%, 19%, 17%, 19%



##### Figure 3 A-C (Country) ####
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
  scale_color_manual(values=c("blue", "red", "orange", "orange", "orange", "gray")) +
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
  scale_color_manual(values=c("blue", "red", "orange", "orange", "orange", "gray")) +
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
  scale_color_manual(values=c("blue", "red", "orange", "orange", "orange", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('No. of affected species') +
  theme(legend.position="none") +
  ylim(c(0,100)) # +
#  geom_vline(xintercept=c(2.7, 3, 5.7), color="red")


#max(scen_df_plotC$nspplostrange0.10)

#### + Unrestricted Figs D-F ####
scen_df_plot <- scen_df_long  

# Avg suitability 
scen_df_plot[scen_df_plot$lost_area==0, ]$cumsuit <- NA

scenfig_cumsuit <- ggplot(data=scen_df_plot, aes(x=lost_area, y=cumsuit,  group=scenario, colour=scenario, linetype=scenario)) +  
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_line() +
  scale_color_manual(values=c("blue", "red", "orange", "orange", "orange", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('Average suitability of converted cells    ') +
  theme(legend.position="none") +
  ylim(c(0,1)) + xlim(c(0,7))


# Sum of Mha range loss
scenfig <- ggplot(data=scen_df_plot, aes(x=lost_area, y=rangeloss_sum,  group=scenario, colour=scenario, linetype=scenario)) + 
  geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_rect(data=rect2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +  
  geom_line() +
  scale_color_manual(values=c("blue", "red", "orange", "orange", "orange", "gray")) +
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
  scale_color_manual(values=c("blue", "red", "orange", "orange", "orange", "gray")) +
  scale_linetype_manual(values=c(1,1,1,2,3,1)) +
  xlab('Rubber expansion (Mha)') + ylab('No. of affected species') +
  theme(legend.position=c(0, 0.85), legend.title = element_blank(), legend.spacing=unit(0, 'mm'), legend.key.height = unit(0.1, 'cm'))+
  ylim(c(0,100))   #+
# geom_vline(xintercept=c(2.7, 3, 5.7), color="red")


#### comb fig (2x3) ####
#scenfig_comb <- plot_grid(scenfig_cumsuit, scenfig, scenfig_affspp, scenfigC_cumsuit, scenfigC, scenfigC_affspp, ncol=3, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size=8  )

scenfig_comb <- plot_grid(scenfigC_cumsuit, scenfigC, scenfigC_affspp, scenfig_cumsuit, scenfig, scenfig_affspp,  ncol=3, labels = c('A', 'B', 'C', 'D', 'E', 'F'), label_size=8  )

cowplot::save_plot("output/results/textS1_Fig3_A-F.png", scenfig_comb, base_width=6.85, base_height = 6.85/3*2, dpi=300)



#less variation between the compromise scenarios because there's only 1.84 Mha AOC in levels 1-5 (scen3a-3b) or levels 1-3 (scen3c) - see Text S1 Table I


###### 2. Table of iucn spp range loss #####

# Load spp list
suit_vuln_spp <- fread('output/suit_vuln_scenario_combspp.csv') #few seconds
sppnames <- data.frame(SCINAME=names(suit_vuln_spp)) 
rm(suit_vuln_spp)

# Load IUCN red list
forspp_corr <- read.csv("data/forestdependentspecies_iucnredlist2019.csv")
head(forspp_corr)
#forspp_corr[duplicated(forspp_corr$SCINAME) | duplicated(forspp_corr$SCINAME, fromLast = TRUE), ]

forspp_corr2 <- forspp_corr %>% distinct(SCINAME, category, class) 
nrow(forspp_corr2) == n_distinct(forspp_corr$SCINAME) #check if TRUE

sppnames_iucn <- left_join(sppnames, forspp_corr2)
sppnames_iucn %>% group_by(category) %>% summarize(n=n())


# NUMBERS FOR METHODS ####
# We ran the conversion simulations on 1155 forest dependent amphibian species, 2376 bird species, and 734 mammal species occurring in our study region. 
sppnames_iucn %>% group_by(class) %>% summarize(n=n())



# Load spprangeloss tbls from simulations & merge with sppnames/iucn categories ####
dflist <- list.files('output/country_scenario', full.names=TRUE)
dflist <- grep('spprangelosstbl', dflist, value=TRUE)
dflist <- grep('test', dflist, value=TRUE, invert=TRUE)
dflist
dflist <- lapply(dflist[c(1,6:9)], fread)

spprangeloss_dfC <- rbindlist(dflist)

spprangeloss_dfC <- spprangeloss_dfC %>% mutate(scenario = rep(c('1-Production', '2-Conservation (all spp)','3a-Comp. Biodiv. (all spp)', '3b-Comp. Suit. (all spp)', '3c-Comp. Both (all spp)'), each=nrow(dflist[[1]]) )) 

spprangeloss_dfC <- spprangeloss_dfC %>% mutate(percent_rangeloss_low = (1-(range_remaining_low/ori_range))*100, percent_rangeloss_high = (1-(range_remaining_high/ori_range))*100,
                                                percent_rangeloss_6.70 = (1-(range_remaining_high_EWT/ori_range))*100)

spprangeloss_dfC$SCINAME <- rep(sppnames_iucn$SCINAME, length(dflist) )
spprangeloss_dfC$iucn_category <- rep(sppnames_iucn$category,length(dflist) )
spprangeloss_dfC$class <- rep(sppnames_iucn$class,length(dflist) )

spprangeloss_dfC$iucn_category <- fct_relevel(spprangeloss_dfC$iucn_category, "CR", "EN", "VU", "NT",  "LC", "DD")

#spprangeloss_dfC <- spprangeloss_dfC %>% mutate(rangeloss.class = cut(percent_rangeloss_low, breaks=seq(0,100,10)), orirange.class=cut(ori_range, breaks=c(0,10,100,1000,10000,100000)))


#### + Unrescrtricted simul ####
dflist <- list.files('output/', full.names=TRUE)
dflist <- grep('spprangelosstbl', dflist, value=TRUE)
dflist
dflist <- lapply(dflist[c(1,6:9)], fread)

spprangeloss_df <- rbindlist(dflist)

spprangeloss_df <- spprangeloss_df %>% mutate(scenario = rep(c('1-Production', '2-Conservation (all spp)','3a-Comp. Biodiv. (all spp)', '3b-Comp. Suit. (all spp)', '3c-Comp. Both (all spp)'), each=nrow(dflist[[1]]) )) 

spprangeloss_df <- spprangeloss_df %>% mutate(percent_rangeloss_low = (1-(range_remaining_low/ori_range))*100, percent_rangeloss_high = (1-(range_remaining_high/ori_range))*100,
                                              percent_rangeloss_6.70 = (1-(range_remaining_high_EWT/ori_range))*100)

spprangeloss_df$SCINAME <- rep(sppnames_iucn$SCINAME, length(dflist) )
spprangeloss_df$iucn_category <- rep(sppnames_iucn$category,length(dflist) )
spprangeloss_df$class <- rep(sppnames_iucn$class,length(dflist) )

spprangeloss_df$iucn_category <- fct_relevel(spprangeloss_df$iucn_category, "CR", "EN", "VU", "NT",  "LC", "DD")


### Table with useful info ####
spprangeloss_sum <- spprangeloss_dfC %>% group_by(scenario, iucn_category) %>% 
  summarize(naffspp_low=sum(percent_rangeloss_low>0),
            naffspp0.10_low=sum(percent_rangeloss_low>=10), 
            naffspp0.20_low=sum(percent_rangeloss_low>=20),
            mean_pct_rangeloss_low=mean(percent_rangeloss_low), 
            se_low=stderr(percent_rangeloss_low),
            naffspp_high=sum(percent_rangeloss_high>0),
            naffspp0.10_high=sum(percent_rangeloss_high>=10), 
            naffspp0.20_high=sum(percent_rangeloss_high>=20),
            mean_pct_rangeloss_high=mean(percent_rangeloss_high), 
            se_high=stderr(percent_rangeloss_high),
            naffspp_6.70=sum(percent_rangeloss_6.70>0),
            naffspp0.10_6.70=sum(percent_rangeloss_6.70>=10), 
            naffspp0.20_6.70=sum(percent_rangeloss_6.70>=20),
            mean_pct_rangeloss_6.70=mean(percent_rangeloss_6.70), 
            se_6.70=stderr(percent_rangeloss_6.70)
  )


spprangeloss_sum %>% dplyr::select(scenario, iucn_category, naffspp_low) %>% spread(scenario,naffspp_low)
spprangeloss_sum %>% dplyr::select(scenario, iucn_category, mean_pct_rangeloss_low) %>% spread(scenario,mean_pct_rangeloss_low)



# Numbers for Results ####
head(spprangeloss_dfC)
#Taking a snapshot of species impacts for the country-restricted simulations at 3.90 Mha expansion (upper bound of industry projections), 74 species (28 amphibians, 26 birds, 20 mammals) would lose ≥10% of their forested range under Scenario 1 (Production), of which six species would lose ≥50% of their forested range (Figure 4). No species were affected under Scenario 2 (Conservation). Scenarios 3a-c (Compromise) would dramtically cut down the number of affected species to 11, none of which would lose ≥50% of their forested range (Figure 4). 

(spprangeloss_sum_10 <- spprangeloss_dfC %>% group_by(scenario) %>% 
    summarize(naffspp10_low=sum(percent_rangeloss_low>=10), naffspp10_high=sum(percent_rangeloss_high>=10)) )

( spprangeloss_sum_50 <- spprangeloss_dfC %>% group_by(scenario) %>% 
    summarize(naffspp50_low=sum(percent_rangeloss_low>=50), naffspp_high50=sum(percent_rangeloss_high>=50)) )


# Taxa breakdown
( spprangeloss_sum_10 <- spprangeloss_dfC %>% group_by(scenario, class) %>% 
    summarize(naffspp_high10=sum(percent_rangeloss_high>=10)) )

( spprangeloss_sum_50 <- spprangeloss_dfC %>% group_by(scenario, class) %>% 
    summarize(naffspp_high50=sum(percent_rangeloss_high>=50)) )
# 3 mammals lose >=50% range in scenario1


# Unrestricted simulations at 3.90 Mha expansion yielded similar patterns across scenarios: again, no species were affected under Scenario 2; whilst Scenarios 3a-c cut the number of affected species under Scenario 1 by half from 43 to 21.
(spprangeloss_sum_10 <- spprangeloss_df %>% group_by(scenario) %>% 
    summarize(naffspp10_low=sum(percent_rangeloss_low>=10), naffspp10_high=sum(percent_rangeloss_high>=10)) )

( spprangeloss_sum_50 <- spprangeloss_df %>% group_by(scenario) %>% 
    summarize(naffspp50_low=sum(percent_rangeloss_low>=50), naffspp_high50=sum(percent_rangeloss_high>=50)) )

# Taxa breakdown
( spprangeloss_sum_10 <- spprangeloss_df %>% group_by(scenario, class) %>% 
    summarize(naffspp_high10=sum(percent_rangeloss_high>=10)) )

( spprangeloss_sum_50 <- spprangeloss_df %>% group_by(scenario, class) %>% 
    summarize(naffspp_high50=sum(percent_rangeloss_high>=50)) )
# 3 mammals lose >=50% range in scenario1


###### + + Fig4 Histogram for paper (AOC NOV) /first CB submission ####
unique(spprangeloss_dfC$scenario)

#mutate(rangeloss.class = cut(percent_rangeloss_high, breaks=seq(0,100,10), right=FALSE, include.lowest = TRUE, labels=c(seq(10,100,10)))


hist_spprangeloss_df_0_highC <- spprangeloss_dfC %>% filter(percent_rangeloss_high>=10)  %>% 
  mutate(rangeloss.class = cut(percent_rangeloss_high, breaks=seq(10,105,5),include.lowest = TRUE, right = FALSE, labels=c(seq(10.1,100.1,5)) ) , orirange.class=cut(ori_range, breaks=c(0,10,100,1000,10000,100000), labels=c("0-0.1 Mha", "0.1-1 Mha", "1-10 Mha", "10-100 Mha", "100-1000 Mha")) ) %>% filter(scenario %in% c("1-Production", "3a-Comp. Biodiv. (all spp)"))

hist_spprangeloss_df_0_highC$rangeloss.class <- as.numeric(as.character(hist_spprangeloss_df_0_highC$rangeloss.class))
hist_spprangeloss_df_0_highC$scenario <- recode(hist_spprangeloss_df_0_highC$scenario,  "3a-Comp. Biodiv. (all spp)" = "3-Compromise") #change the scenario label for each histogram

head(hist_spprangeloss_df_0_highC)

#hist_spprangeloss_df_0_highC %>% filter(percent_rangeloss_high>=50, scenario == "3-Compromise")

theme_set(theme_cowplot(font_size=8))
rect4 <- data.frame (xmin=50, xmax=Inf, ymin=0, ymax=Inf)

str(hist_spprangeloss_df_0_highC$scenario)
as.integer(as.factor(hist_spprangeloss_df_0_highC$scenario))

hist_rangeloss_scen_high <- ggplot(hist_spprangeloss_df_0_highC,aes(rangeloss.class, fill=iucn_category)) + 
  #ggplot(transform(hist_spprangeloss_df_0_highC, scenario=c("A           1-Production         ", "D           3-Compromise      ")[as.integer(as.factor(hist_spprangeloss_df_0_highC$scenario))]), aes(rangeloss.class, fill=iucn_category)) +
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
  #ggplot(transform(hist_spprangeloss_df_0_highC, scenario=c("B             1-Production           ", "E             3-Compromise        ")[as.integer(as.factor(hist_spprangeloss_df_0_highC$scenario))]), aes(rangeloss.class, fill=class)) +
  geom_rect(data=rect4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_histogram(binwidth=5, boundary=0) + 
  facet_wrap(scenario ~., ncol=1) +
  #xlim(c(10,100)) +
  scale_x_continuous(breaks=seq(10, 100, 10))  +
  theme(legend.position = c(0.45, 0.88),         
        legend.title=element_text(size=9, face="bold"), legend.text=element_text(size=9),  
        axis.title.y = element_text(size=0), axis.title.x = element_text(size=10), 
        axis.text = element_text(size=8), strip.text = element_text(size = 10) ) +
  #guides(fill=guide_legend(ncol=1), title="Taxonomic class") +
  ylab('') + xlab('% range loss') +
  labs(fill="Taxonomic class")

hist_rangeloss_scen_range_high <- 
  ggplot(hist_spprangeloss_df_0_highC,  aes(rangeloss.class, fill=orirange.class)) + 
  #ggplot(transform(hist_spprangeloss_df_0_highC,  scenario=c("C             1-Production          ", "F             3-Compromise       ")[as.integer(as.factor(hist_spprangeloss_df_0_highC$scenario))]),  aes(rangeloss.class, fill=orirange.class)) + 
  geom_rect(data=rect4, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="grey", alpha=0.3, inherit.aes = FALSE) +
  geom_histogram(binwidth=5, boundary=0) + 
  facet_wrap(scenario ~., ncol=1) +
  #xlim(c(10,100)) +
  scale_x_continuous(breaks=seq(10, 100, 10))  +
  theme(legend.position = c(0.45, 0.86),
        legend.title=element_text(size=9, face="bold"), legend.text=element_text(size=9), 
        axis.title.y = element_text(size=0), axis.title.x = element_text(size=10), 
        axis.text = element_text(size=8), strip.text = element_text(size = 10) ) +
  ylab('') + xlab(' ') +
  labs(fill="Original range size")

scen_histC <- plot_grid( hist_rangeloss_scen_high, hist_rangeloss_scen_tax_high, hist_rangeloss_scen_range_high, ncol=3)
ggsave('output/results/', scen_histC, width=6.85, height=4, units="in", dpi=300)

scen_histC_labs <- scen_histC + draw_plot_label(c("A", "B", "C", "D", "E","F"), x=rep(c(0.03,0.34,0.67),2), y=rep(c(1,0.55),each=3), fontface="bold", size=10)
cowplot::save_plot("output/results/textS1_fig4.png", scen_histC_labs, base_width=6.85, base_height = 4, dpi=300)




## more extra plots ####
spprangeloss_df_0_filter <- filter(spprangeloss_df, scenario %in% c('avg.suit.acc', 'avg.vuln.carb', ''))
spprangeloss_df_0_norand <- filter(spprangeloss_df, scenario != 'random')
spprangeloss_df_10_norand <- filter(spprangeloss_df, scenario != 'random', percent_rangeloss_low >10)
spprangeloss_df_10_filter <- filter(spprangeloss_df, percent_rangeloss_low >10)


hist_rangeloss_scen <- ggplot(data=spprangeloss_df_10_norand, aes(percent_rangeloss_low, fill=iucn_category)) + geom_histogram(bins=9) + facet_wrap(. ~scenario, ncol=2)
hist_rangeloss_scen <- ggplot(data=spprangeloss_df_10_filter, aes(percent_rangeloss_low, fill=iucn_category)) + geom_histogram(bins=9) + facet_wrap(. ~scenario, ncol=2)


ggplot(data=filter(spprangeloss_df, percent_rangeloss_high>0), aes(percent_rangeloss_high, fill=scenario)) + geom_histogram(bins=100)



###### specific spp that get hammered ####
check <- spprangeloss_dfC %>%  filter(percent_rangeloss_high>=50) %>% group_by(scenario, percent_rangeloss_high, SCINAME) #many small ranged spp affected by aoc vulnA
check$SCINAME
#1] "Amnirana fonensis"     "Amnirana occidentalis" "Crocidura nimbae"      "Crocidura zaphiri"    
#[5] "Hipposideros marisae"  "Rhinolophus ziama"   

# Rhinolophus ziama (ori range 7)
# Hipposideros (ori range 39)
# Crocidura zaphiri  (ori range 15)- DD- ethiopia kenya - Zaphir's Shrew
# Phrynobatrachus annulatus (Ringed River Frog), Cote d Ivoire, Ghana, bit in Liberia/Guinea - EN -
# scen 1 affects Amnirana occidentalis (LC), scen 3 does but lesser extent, ori range (20). Range strateches from eastern Sierra Leone to western Ghana
# scen4 affect Dobsonia pannietensis (ori range 45, all in PNG) 


dput(unique(spprangeloss_df_0$rangeloss.class))

check %>% group_by(scenario, ori_range) %>% summarize(n=n())

#The production-driven scenario would also affect threatened species, including the endangered Ziama Horseshoe Bat (Rhinolophus ziama) and the vulnerable Aellen’s Roundleaf Bat (Hipposideros marisae), both small-ranged species found in the forests of Guinea, Liberia and neighbouring countries [14]. 

