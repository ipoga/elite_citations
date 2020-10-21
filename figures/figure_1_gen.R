source("figures/figure_settings.R")

load("data/fig1_cumul_data.Rdat")
load("data/fig2_data.Rdat")

f1.cumul <- f1.cumul %>% filter(y <= 2015)

p1 <- f1.cumul %>%
  gather("indi","val",ncs_inf,ncs_inf_full) %>%
  mutate(indi_g = case_when(
    indi == "ncs_inf" ~ "Fractional",
    indi == "ncs_inf_full" ~ "Full",
  )) %>%
  ggplot(aes(x = y, y = val, color = as.factor(1), group = indi_g)) + 
  geom_point(aes(shape=indi_g)) + 
  geom_line(linetype = "dotted", size=.5) +
  #geom_smooth(method='lm',aes(fill=as.factor(1)),alpha=.1,size=.5) +
  scale_y_continuous("Proportion of all citations", limits = c(0,.3)) + 
  scale_x_continuous("Year") + 
  scale_color_manual(guide = F, values=elite_cols[1]) +
  scale_fill_manual(guide = F, values=elite_cols[1]) + 
  scale_shape_manual(guide = F, values = c(1,16)) + 
  labs(title = "A. Citation share of top 1% authors, everything")

p1a <- f2.phys %>% filter(y <= 2015) %>%
  gather("indi","val",ncs_inf,ncs_inf_full) %>%
  mutate(indi_g = case_when(
    indi == "ncs_inf" ~ "Fractional",
    indi == "ncs_inf_full" ~ "Full",
  )) %>%
  ggplot(aes(x = y, y = val, color = as.factor(1), group = indi_g)) + 
  geom_point(aes(shape=indi_g)) + 
  geom_line(linetype = "dotted", size=.5) +
  #geom_smooth(method='lm',aes(fill=as.factor(1)),alpha=.1,size=.5) +
  scale_y_continuous("Proportion of all citations", limits = c(0,.3)) + 
  scale_x_continuous("Year") + 
  scale_color_manual(guide = F, values=elite_cols[1]) +
  scale_fill_manual(guide = F, values=elite_cols[1]) + 
  scale_shape_manual(guide = F, values = c(1,16)) + 
  labs(title = "B. Only physics and astronomy")

p1b <- f2.nophys %>% filter(y <= 2015) %>%
  gather("indi","val",ncs_inf,ncs_inf_full) %>%
  mutate(indi_g = case_when(
    indi == "ncs_inf" ~ "Fractional",
    indi == "ncs_inf_full" ~ "Full",
  )) %>%
  ggplot(aes(x = y, y = val, color = as.factor(1), group = indi_g)) + 
  geom_point(aes(shape=indi_g)) + 
  geom_line(linetype = "dotted", size=.5) +
  #geom_smooth(method='lm',aes(fill=as.factor(1)),alpha=.1,size=.5) +
  scale_y_continuous("Proportion of all citations", limits = c(0,.3)) + 
  scale_x_continuous("Year") + 
  scale_color_manual(guide = F, values=elite_cols[1]) +
  scale_fill_manual(guide = F, values=elite_cols[1]) + 
  scale_shape_manual("Count", values = c(1,16)) + 
  labs(title = "C. No physics and astronomy")

#agg_tiff("figures/fig_1.tiff",res=300,width=15,height=14,compression = "lzw",units="cm")
plot <- p1 / (p1a + p1b) + plot_layout(guides = "collect")
#invisible(dev.off())
