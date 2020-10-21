source("figures/figure_settings.R")

load("data/fig3_ann_gini_data.Rdat")

# Set current dataset

f3c <- f3c.nophys

p <- f3c %>%
  gather("key","val",g,gb) %>%
  mutate(key = case_when(key == "g" ~ "Full sample",
                          key == "gb" ~ "Resampled")) %>%
  ggplot(aes(x=y,y=val,color=key)) +
  geom_point() +
  geom_line() + 
  scale_x_continuous("Year") + 
  scale_y_continuous("Gini index") + 
  theme(
    panel.grid.minor = element_line(colour = "grey90", linetype="dashed",size=.25)
  ) + 
  scale_color_manual("Sample", values=elite_cols[1:2])

#agg_tiff("figures/fig_S6.tiff",res=300,width=15,height=9,compression = "lzw",units="cm")
plot <- p
#invisible(dev.off())