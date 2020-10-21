source("figures/figure_settings.R")

load("data/fig2_data.Rdat")

f1 <- f2.nophys
f2 <- f2.phys

f1$set <- "nophys"
f2$set <- "phys"

f <- rbind(f1,f2)

ps1 <- f %>%
  gather("indi","val",ncs_inf_full,ncs_boot) %>%
  mutate(indi_g = case_when(
    indi == "ncs_boot" ~ "Bootstrapped",
    indi == "ncs_inf_full" ~ "All authors",
  ), set = case_when(
    set == "phys" ~ "Only physics",
    set == "nophys" ~ "Without physics"
  )) %>%
  ggplot(aes(x = y, y = val,color=indi_g,group=indi_g)) + 
  facet_wrap(~set) +
  geom_point(aes(shape=indi_g)) + 
  geom_line(linetype = "dotted", size=.5) + 
  scale_y_continuous("Proportion of all citations",limits=c(0,.31)) + 
  scale_x_continuous("Year") + 
  scale_fill_manual("Sample",values=elite_cols[1:2]) + 
  scale_color_manual("Sample",values=elite_cols[1:2]) +
  scale_shape_manual("Sample",values=c(1,16)) + 
  labs(title = "B. Comparison to stable author sample size (bootstrapped)", subtitle = "Reporting citation concentration for top 1% researchers.")

ps2 <- f %>% ggplot(aes(x=y,y=nfolks,color=set,fill=set)) + 
  geom_point() +
  geom_line() + 
  scale_y_continuous("N, authors", limits=c(0,NA),labels = scales::scientific) + 
  scale_x_continuous("Year") + 
  scale_color_manual(guide=F,values=elite_cols[1:2]) + 
  scale_fill_manual(guide=F,values=elite_cols[1:2]) + 
  labs(title = "A. Total authors when bootstrapping seed authors")

load("data/fig_s2_core_data.Rdat")

f2.phys.core$set <- "phys"
f2.nophys.core$set <- "nophys"
f2.phys$set <- "phys"
f2.nophys$set <- "nophys"

f2.phys.core$src <- "core"
f2.nophys.core$src <- "core"
f2.phys$src <- "all"
f2.nophys$src <- "all"


f <- rbind(f2.phys.core,f2.nophys.core,f2.phys,f2.nophys)

ps3 <- f %>% 
  mutate(set = case_when(
    set == "phys" ~ "Only physics",
    set == "nophys" ~ "Without physics"
  ), src = case_when(
    src == "core" ~ "Core journals",
    src == "all" ~ "All journals"
  )) %>%
  ggplot(aes(x = y, y = ncs_inf_full, color = as.factor(src), group = as.factor(src))) + 
  facet_wrap( ~ set) +
  geom_point(aes(shape=src)) + 
  geom_line(linetype = "dotted", size=.5) + 
  scale_x_continuous("Year") + 
  scale_y_continuous("Proportion of all citations", limits = c(0,.31)) + 
  scale_color_manual("Source",values=elite_cols[1:2]) + 
  scale_fill_manual("Source",values=elite_cols[1:2]) + 
  scale_shape_manual("Source",values=c(1,16)) + 
  labs(title = "C. Comparison between all journals and limiting to journals active in 2000 (core)", subtitle = "Reporting citation concentration for top 1% researchers.")

#agg_tiff("figures/fig_S3.tiff",res=300,width=15,height=22,compression = "lzw",units="cm")
plot <- (ps2 / ps1 + plot_layout(guides = "collect")) / ps3 
#invisible(dev.off())
