source("figures/figure_settings.R")

load("data/fig3_data.Rdat")
load("data/fig3_ann_gini_data.Rdat")

# Set current dataset
f3a <- f3a.nophys
f3b <- f3b.nophys
f3c <- f3c.nophys

f3b$period <- paste(f3b$y,f3b$y+4,sep="-")
f3a$period <- paste(f3a$y,f3a$y+4,sep="-")

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

g <- f3a %>%
  select(-y)
g$pct <- .01
g$val <- c(.7,.8,.9)
g$g <- paste("Gini (",g$period,") = ",round(g$g,2),sep="")

intop <- f3b %>%
  select(-y) %>%
  gather(key="indi",value="val",ncs_inf)

intop$pct <- 1 - intop$pct
intop$val <- 1 - intop$val

p1 <- intop %>%
  filter(pct != 0) %>%
  ggplot(aes(x=pct,y=val,group=period,color=period)) +
  geom_line() + 
  scale_x_log10("Inverse percentile rank of authors, log10") + 
  #geom_label(data=g,aes(x = pct, y = val, label=g, group = 1),show.legend = F,size=3) +
  scale_y_continuous("Citation share") + 
  theme(
    panel.grid.minor = element_line(colour = "grey90", linetype="dashed",size=.25)
  ) + 
  scale_color_manual("Year range", values=elite_cols[1:3]) + 
  labs(title = "A. Cumulative citation density")

p2 <- f3c %>%
  ggplot(aes(x=y,y=g)) +
  geom_point() +
  geom_line() + 
  scale_x_continuous("Year") + 
  scale_y_continuous("Gini index") + 
  theme(
    panel.grid.minor = element_line(colour = "grey90", linetype="dashed",size=.25)
  ) + 
  scale_color_manual("Year range", values=elite_cols[1:3],guide=F) + 
  labs(title = "B. Citation inequality, annual Gini index")

load("data/fig2b_data.Rdat")

#agg_tiff("figures/fig_3.tiff",res=300,width=15,height=9,compression = "lzw",units="cm")
plot <- (p1 + p2 + plot_layout(guides = "collect"))
#invisible(dev.off())
