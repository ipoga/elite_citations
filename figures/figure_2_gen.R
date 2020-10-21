source("figures/figure_settings.R")

load("data/fig2_data.Rdat")

#f2.phys <- f2.phys %>% filter(y <= 2015)
f2 <- f2.nophys %>% filter(y <= 2015)

p1 <- f2 %>%
  gather("indi","val",n_prop_1,p_prop_1) %>%
  mutate(indi_g = case_when(
    indi == "n_prop_1" ~ "Full",
    indi == "p_prop_1" ~ "Fractional",
  )) %>%
  ggplot(aes(x = y, y = val, color = as.factor(1), group = indi_g)) + 
  geom_point(aes(shape = indi_g)) + 
  geom_line(linetype = "dotted", size=.5) +
  #geom_smooth(method='lm',aes(fill=indi_g),alpha=.1,size=.5) +
  scale_y_continuous("Proportion of all publications", limits = c(0,NA)) + 
  scale_x_continuous("Year") + 
  scale_color_manual(guide = F, values=elite_cols[1]) +
  scale_fill_manual(guide = F, values=elite_cols[1]) + 
  scale_shape_manual(guide = F, values = c(1,16)) + 
  labs(title = "A. Publication share")

p2 <- f2 %>%
  gather("key","indi",n_1,n_75,p_1,p_75,n_50,p_50) %>%
  mutate(key_type = case_when(
    key == "n_1" | key == "n_75" | key == "n_50" ~ "Full",
    key == "p_1" | key == "p_75" | key == "p_50" ~ "Fractional"
    
  ), lvl = case_when(
    key == "p_1" | key == "n_1" ~ "99th",
    key == "p_75" | key == "n_75" ~ "75th",
    key == "n_50" | key == "p_50" ~ "50th"
  )) %>%
  group_by(key) %>%
  mutate(indi_i = indi / indi[y == 2000] * 100) %>%
  ggplot(aes(x = y, y = indi, color = lvl, shape = key_type)) +
  geom_point() + 
  geom_line(linetype = "dotted", size=.5) +
  facet_grid(key_type ~ ., scales = "free_y") +
  #geom_smooth(method='lm',formula = 0 + y ~ x,aes(fill=lvl,linetype=key_type),alpha=.1,size=.5) +
  scale_x_continuous("Year") + 
  scale_y_continuous("Mean papers per author") + 
  scale_linetype_discrete("Count",labels=c("Full","Fractional")) +
  scale_color_manual("Percentile",values=elite_cols[3:1]) +
  scale_shape_manual("Count", values=c(1,16)) +
  scale_fill_manual(guide = F, values=elite_cols[3:1],labels=c("99th","75th","50th")) + 
  labs(title = "B. Mean papers per author")

p3 <- f2 %>% gather("key","indi",mnics_frac_1,mnics_full_1) %>%
  mutate(key_type = case_when(
    key == "mnics_frac_1" | key == "mnics_frac_75" | key == "mnics_frac_50" ~ "Fractional",
    key == "mnics_full_1" | key == "mnics_full_75" | key == "mnics_full_50" ~ "Full"
    
  ), lvl = case_when(
    key == "mnics_full_1" | key == "mnics_frac_1" ~ "99th",
    key == "mnics_full_75" | key == "mnics_frac_75" ~ "75th",
    key == "mnics_full_50" | key == "mnics_frac_50" ~ "50th"
  )) %>%
  group_by(key) %>%
  ggplot(aes(x = y, y = indi, color = lvl, shape = key_type)) +
  geom_point() + 
  facet_grid(key_type ~ ., scales = "free_y") +
  geom_line(linetype = "dotted", size=.5) +
  #geom_smooth(method='lm',formula = 0 + y ~ x,aes(fill=lvl,linetype=key_type),alpha=.1,size=.5) +
  scale_x_continuous("Year") + 
  scale_y_continuous("MNICS",limits = c(0,NA)) + 
  scale_linetype_discrete("Count",guide=F) +
  scale_color_manual(guide = F, "Percentile",values=elite_cols[1]) +
  scale_shape_manual(guide = F, "Count",labels=c("Full","Fractional"), values=c(1,16)) +
  scale_fill_manual(guide = F, values=elite_cols[1]) + 
  labs(title = "C. MNICS for top 1% authors")

p4 <- f2 %>%
  gather("key","indi",prop_collab_1,prop_collab_50,prop_collab_75) %>%
  mutate(lvl = case_when(
    key == "prop_collab_1" | key == "network_size.a_1" ~ "99th",
    key == "prop_collab_75" | key == "network_size.a_75" ~ "75th",
    key == "prop_collab_50" | key == "network_size.a_50" ~ "50th"
  )) %>%
  ggplot(aes(x = y, y = indi, color = lvl)) + 
  geom_point() + 
  geom_line(linetype = "dotted", size=.5) +
  scale_x_continuous("Year") + 
  scale_y_continuous("Share with co-authors",limits = c(NA,1)) + 
  scale_linetype_discrete(guide = F,"Count",labels=c("Full","Fractional")) +
  scale_color_manual(guide = F, "Percentile",values=elite_cols[1:3]) +
  scale_shape_manual(guide = F, "Count",labels=c("Full","Fractional"), values=c(1,16)) +
  scale_fill_manual(guide = F, values=elite_cols[1:3]) + 
  labs(title = "D. Mean collaboration share")

p5 <- f2 %>%
  gather("key","indi",network_size.a_1,network_size.a_50,network_size.a_75) %>%
  mutate(lvl = case_when(
    key == "prop_collab_1" | key == "network_size.a_1" ~ "99th",
    key == "prop_collab_75" | key == "network_size.a_75" ~ "75th",
    key == "prop_collab_50" | key == "network_size.a_50" ~ "50th"
  )) %>%
  ggplot(aes(x = y, y = indi, color = lvl)) + 
  geom_point() + 
  geom_line(linetype = "dotted", size=.5) +
  scale_x_continuous("Year") + 
  scale_y_continuous("Number of co-authors") + 
  scale_linetype_discrete("Count",labels=c("Full","Fractional"),guide=F) +
  scale_color_manual("Percentile",values=elite_cols[3:1],guide=F) +
  scale_shape_manual("Count",labels=c("Full","Fractional"), values=c(1,16),guide=F) +
  scale_fill_manual(guide = F, values=elite_cols[3:1]) + 
  labs(title = "E. Co-author network")

p6 <- f2 %>%
  gather("key","indi",m_authors_1,m_authors_75,m_authors_50,md_authors_75,md_authors_1,md_authors_50) %>%
  mutate(key_type = case_when(
    key == "m_authors_1" | key == "m_authors_75" | key == "m_authors_50" ~ "Mean",
    key == "md_authors_1" | key == "md_authors_75" | key == "md_authors_50" ~ "Median"
    
  ), lvl = case_when(
    key == "m_authors_1" | key == "md_authors_1" ~ "99th",
    key == "m_authors_75" | key == "md_authors_75" ~ "75th",
    key == "m_authors_50" | key == "md_authors_50" ~ "50th"
  )) %>%
  group_by(key) %>%
  ggplot(aes(x = y, y = indi, color = lvl, shape = key_type)) +
  geom_line(linetype = "dotted", size=.5) +
  geom_point() + 
  #geom_smooth(method='lm',aes(fill=key_type,linetype=lvl),alpha=.1,size=.5) +
  scale_x_continuous("Year") + 
  scale_y_continuous("Growth, co-authors") + 
  facet_grid(key_type ~ ., scales = "free_y") +
  #scale_linetype_discrete("Percentile",labels=c("99th","75th","50th")) +
  scale_color_manual("Percentile",values=elite_cols[3:1],guide = F) +
  scale_shape_manual("Summary",label = c("Mean","Median"),values = c(0,15)) +
  scale_fill_manual(guide = F, values=elite_cols[1:2]) + 
  labs(title = "F. Mean co-authors per author")
  
#agg_tiff("figures/fig_2.tiff",res=300,width=15,height=14,compression = "lzw",units="cm")
plot <- (p1 + p2 + p3) / (p4 + p5 + p6) + plot_layout(guides = "collect")
#invisible(dev.off())