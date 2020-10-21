source("figures/figure_settings.R")

load("data/figS1_data.Rdat")

p <- fs1 %>%
  gather("unit","value",n,nr,wos_n,wos_nr) %>%
  mutate(src = case_when(unit == "n" ~ "Used in this study",
                         unit == "nr" ~ "Used in this study",
                         unit == "wos_n" ~ "Full WoS",
                         unit == "wos_nr" ~ "Full WoS"),
         lbl = case_when(unit == "n" ~ "Number of papers",
                         unit == "nr" ~ "Number of covered references",
                         unit == "wos_n" ~ "Number of papers",
                         unit == "wos_nr" ~ "Number of covered references")) %>%
  ggplot(aes(x=pub_year,y=value,group=src,color=src)) + 
  facet_wrap( ~ lbl, scales = "free_y") +
  geom_point(shape=4) + 
  scale_y_continuous("",labels = scales::scientific) + 
  scale_x_continuous("Publication year") + 
  scale_color_manual("Sample",values=elite_cols[1:2])


#agg_tiff("figures/fig_S1.tiff",res=300,width=15,height=9,compression = "lzw",units="cm")
plot <- p
#invisible(dev.off())
