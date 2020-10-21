source("figures/figure_settings.R")

load("data/fig5_data.Rdat")
load("data/fig5b_data.Rdat")

f5a <- f5a.nophys
f5b <- f5b.nophys
f5d <- f5d.nophys
f5e <- f5e.nophys

f5b$period <- paste(f5b$y,f5b$y+4,sep="-")
f5a$period <- paste(f5a$y,f5a$y+4,sep="-")
f5e$period <- paste(f5e$y,f5e$y+4,sep="-")
f5d$period <- paste(f5d$y,f5d$y+4,sep="-")

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

#scale_x_log10(breaks = breaks, minor_breaks = minor_breaks) + 
#scale_y_log10(breaks = breaks) + 


g <- f5a %>%
  select(-y)
g$pct <- .1
g$val <- c(.2,.3,.4)
g$g <- paste("Gini (",g$period,") = ",round(g$g,2),sep="")

intop <- f5b %>%
  select(-y) %>%
  gather(key="indi",value="val",ncs_inf)

intop$pct <- 1 - intop$pct
intop$val <- 1 - intop$val

p1 <- intop %>%
  filter(pct != 0) %>%
  ggplot(aes(x=pct,y=val,group=period,color=period)) +
  geom_label(data=g,aes(x = pct, y = val, label=g, group = 1),show.legend = F,size=2.5) +
  geom_line() + 
  scale_x_log10("Inverse percentile rank of countries") + 
  scale_y_continuous("Cumulative citation density",limits = c(0,1)) + 
  theme(
    panel.grid.minor = element_line(colour = "grey90", linetype="dashed",size=.25)
  ) + 
  scale_color_manual("Year range", values=elite_cols[1:3]) + 
  labs(title = "A. Cumulative citation density, countries")

p2 <- f5b %>%
  select(-y) %>%
  filter(pct != 1) %>%
  gather(key="indi",value="val",ncs_inf) %>%
  ggplot(aes(x=pct,y=val,group=period,color=period)) +
  geom_label(data=g,aes(x = pct, y = val, label=g, group = 1),show.legend = F,size=2.5) +
  geom_line() + 
  scale_x_continuous("Country percentile") + 
  scale_y_continuous("",limits = c(0,1)) + 
  theme(
    panel.grid.minor = element_line(colour = "grey90", linetype="dashed",size=.25)
  ) + 
  scale_color_manual("Year range", values=elite_cols[1:3],guide=F) + 
  labs(title = "B. Citation inequality, countries")

g <- f5d %>%
  select(-y)
g$pct <- .1
g$val <- c(.2,.3,.4)
g$g <- paste("Gini (",g$period,") = ",round(g$g,2),sep="")

intop <- f5e %>%
  select(-y) %>%
  gather(key="indi",value="val",ncs_inf)

intop$pct <- 1 - intop$pct
intop$val <- 1 - intop$val

p3 <- intop %>%
  filter(pct != 0) %>%
  ggplot(aes(x=pct,y=val,group=period,color=period)) +
  geom_label(data=g,aes(x = pct, y = val, label=g, group = 1),show.legend = F,size=2.5) +
  geom_line() + 
  scale_x_log10("Inverse percentile rank of institutions") + 
  scale_y_continuous("Cumulative citation density",limits = c(0,1)) + 
  theme(
    panel.grid.minor = element_line(colour = "grey90", linetype="dashed",size=.25)
  ) + 
  scale_color_manual("Year range", values=elite_cols[1:3], guide = F) + 
  labs(title = "C. Cumulative citation density, institutions")

p4 <- f5e %>%
  select(-y) %>%
  filter(pct != 1) %>%
  gather(key="indi",value="val",ncs_inf) %>%
  ggplot(aes(x=pct,y=val,group=period,color=period)) +
  geom_label(data=g,aes(x = pct, y = val, label=g, group = 1),show.legend = F,size=2.5) +
  geom_line() + 
  scale_x_continuous("Institution percentile") + 
  scale_y_continuous("", limits = c(0,1)) + 
  theme(
    panel.grid.minor = element_line(colour = "grey90", linetype="dashed",size=.25)
  ) + 
  scale_color_manual("Year range", values=elite_cols[1:3],guide=F) + 
  labs(title = "D. Citation inequality, institutions")

#agg_tiff("figures/fig_5.tiff",res=300,width=15,height=8,compression = "lzw",units="cm")
plot <- (p1 + p3) + plot_layout(guides = "collect")
#invisible(dev.off())
