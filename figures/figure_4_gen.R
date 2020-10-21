source("figures/figure_settings.R")

load("data/fig4_data.Rdat")

f4 <- f4.nophys %>% filter(y <= 2015)

load("data/fig4a_data.Rdat")

# Set current dataset

f4d$period <- paste("'",substr(f4d$y,3,4),"-'",substr(f4d$y+4,3,4),sep="")
f4e$period <- paste("'",substr(f4e$y,3,4),"-'",substr(f4e$y+4,3,4),sep="")

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

g <- f4e %>%
  select(-y)
g$pct <- .005
g$val <- rep(c(.6,.75,.9),3)
g$g <- paste("Gini (",g$period,") = ",round(g$g,2),sep="")

intop <- f4d %>%
  select(-y) %>%
  gather(key="indi",value="val",ncs_inf)

intop$pct <- 1 - intop$pct
intop$val <- 1 - intop$val

p1 <- intop %>%
  filter(pct != 0) %>%
  ggplot(aes(x=pct,y=val,group=period,color=period)) +
  geom_line() + 
  facet_wrap(~ field) +
  scale_x_log10("Inverse percentile rank of authors, log10") + 
  scale_y_continuous("Citation share") + 
  theme(
    panel.grid.minor = element_line(colour = "grey90", linetype="dashed",size=.25)
  ) + 
  scale_color_manual("Year range", values=elite_cols[1:3]) + 
  labs(title = "A. Cumulative citation density per field")

p2 <- f4 %>% 
  filter(oecd != "Humanities" & oecd != "Social Sciences" & oecd != "Engineering and Technology") %>%
  ggplot(aes(x = y, y = g, color = oecd, group = oecd)) + 
  geom_line() +
  scale_x_continuous("Year") + 
  scale_y_continuous("Gini Index") + 
  scale_color_manual("OECD Field",values = c(elite_cols[1:3])) + 
  labs(title = "B. Gini index per field and year")

#agg_tiff("figures/fig_4.tiff",res=300,width=15,height=12,compression = "lzw",units="cm")
plot <- p1 / p2
#invisible(dev.off())
