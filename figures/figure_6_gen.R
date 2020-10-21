source("figures/figure_settings.R")

umerge <- function(u1,u2,d) {
  for (y in c(2000,2005,2010)) {
    e <- d$n_elite[d$inst == u1 & d$year == y] + d$n_elite[d$inst == u2 & d$year == y]
    t <- d$n_total[d$inst == u1 & d$year == y] + d$n_total[d$inst == u2 & d$year == y]
    p <- e / t
    d$n_elite[d$inst == u1 & d$year == y] <- e
    d$n_total[d$inst == u1 & d$year == y] <- t
    d$p_elite[d$inst == u1 & d$year == y] <- p
  }
  d <- d %>% filter(inst != u2)
  return(d)
}

load("data/fig7_data.Rdat")

f7c <- f7c.nophys %>% filter(n_total > 0)

f7c <- umerge("washington univ","univ washington",f7c)

f7c <- f7c %>% mutate(cinst = case_when(
  inst == "univ bonn" ~ "Bonn Univ.",
  inst == "univ amsterdam" ~ "Univ. Amsterdam",
  inst == "univ melbourne" ~ "Univ. Melbourne",
  inst == "stanford univ" ~ "Stanford Univ.",
  inst == "vrije univ amsterdam" ~ "VU Amsterdam",
  inst == "univ cambridge" ~ "Univ. Cambridge",
  inst == "mit" ~ "MIT",
  inst == "mcmaster univ" ~ "McMaster Univ.",
  inst == "univ calif berkeley" ~ "UC Berkeley",
  inst == "univ calif san francisco" ~ "UC San Francisco",
  inst == "univ oxford" ~ "Univ. Oxford",
  inst == "ucl" ~ "Univ. Coll. London",
  inst == "harvard univ" ~ "Harvard Univ.",
  inst == "duke univ" ~ "Duke Univ.",
  inst == "univ copenhagen" ~ "Copenhagen Univ.",
  inst == "washington univ" ~ "Univ. Washington",
  inst == "univ sydney" ~ "Univ. Sydney",
  inst == "univ bristol" ~ "Univ Bristol",
  inst == "kings coll london" ~ "Kings Coll. London",
  inst == "univ manchester" ~ "Univ. Manchester",
  inst == "leiden univ" ~ "Leiden Univ.",
  inst == "caltech" ~ "CalTech",
  inst == "eth" ~ "ETH",
  inst == "univ calif san diego" ~ "UC San Diego",
  inst == "univ hong kong" ~ "Univ. Hong Kong",
  inst == "univ toronto" ~ "Univ. Toronto",
  inst == "univ michigan" ~ "Univ. Michigan",
  inst == "univ pittsburgh" ~ "Univ. Pittsburg",
  inst == "johns hopkins univ" ~ "Johns Hopkins Univ.",
  inst == "kyoto univ" ~ "Kyoto Univ.",
  inst == "scripps res inst" ~ "Scripps Res. Inst.",
  inst == "brazil" ~ "Brazil",
  inst == "brazil" ~ "Brazil",
  inst == "brazil" ~ "Brazil",
  inst == "brazil" ~ "Brazil",
  TRUE ~ inst
), hosp = case_when(
  inst == "dana farber canc inst" ~ 1,
  inst == "erasmus mc" ~ 1,
  inst == "brigham & womens hosp" ~ 1,
  inst == "fred hutchinson canc res ctr" ~ 1,
  inst == "massachusetts gen hosp" ~ 1,
  inst == "univ texas md anderson canc ctr" ~ 1,
  inst == "mayo clin" ~ 1,
  inst == "karolinska inst" ~ 1,
  inst == "mem sloan kettering canc ctr" ~ 1,
  inst == "nci" ~ 1,
  inst == "niaid" ~ 1,
  inst == "st jude childrens res hosp" ~ 1,
  inst == "london sch hyg & trop med" ~ 1,
  inst == "johns hopkins bloomberg sch publ hlth" ~ 1,
  inst == "inst canc res" ~ 1,
  inst == "univ zurich hosp" ~ 1,
  inst == "nih" ~ 1,
  inst == "inst gustave roussy" ~ 1,
  inst == "german canc res ctr" ~ 1,
  inst == "nhlbi" ~ 1,
  inst == "" ~ 1,
  TRUE ~ 0
))

f7c$inst <- f7c$cinst
f7c <- f7c %>% filter(hosp == 0)

f7d <- f7c %>%
  group_by(inst) %>%
  summarise(n_elite = sum(n_elite), n_total = sum(n_total))
f7d$p_elite <- f7d$n_elite / f7d$n_total

f7c$cshow <- f7c$inst
f7c$cshow[f7c$year != 2005] <- ""

f7c <- f7c %>% filter(n_total >= 200 & n_elite >= 30)
f7d <- f7d %>% filter(n_total >= 2000 & n_elite >= 30)

f7c$show <- 0
f7d$show <- 0
f7d <- f7d %>% arrange(-p_elite)
for (i in 1:10) {
  f7d$show[i] <- 1
}
cx <- f7d %>% filter(show != 0) %>% select(inst,show)

for (i in 1:nrow(cx)) {
  c <- cx$inst[i]
  s <- cx$show[i]
  f7c$show[f7c$inst == c] <- s
}

p6a <- f7c %>%
  filter(show == 1) %>%
  ggplot(aes(x = n_elite,y = p_elite, color = inst)) + 
  geom_point(shape = as.factor(1)) + 
  geom_line(arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) + 
  scale_x_log10("N, elite researchers",limits = c(10,1000)) + 
  scale_y_continuous("Proportion, elite researchers",limits = c(0,.05)) +
  scale_shape_manual("Period",values = c(1),guide=F) +
  geom_text_repel(aes(label=cshow),size=2.5,family = "Raleway",force = 2) + 
  scale_color_discrete(guide=F) + 
  labs(title = "A. Highest proportion elite")

f7c$show <- 0
f7d$show <- 0
f7d <- f7d %>% arrange(-n_elite)
for (i in 1:10) {
  f7d$show[i] <- 1
}
cx <- f7d %>% filter(show != 0) %>% select(inst,show)

for (i in 1:nrow(cx)) {
  c <- cx$inst[i]
  s <- cx$show[i]
  f7c$show[f7c$inst == c] <- s
}

p6b <- f7c %>%
  filter(show == 1) %>%
  ggplot(aes(x = n_elite,y = p_elite, color = inst)) + 
  geom_point(shape = as.factor(1)) + 
  geom_line(arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) + 
  scale_x_log10("N, elite researchers",limits = c(10,1000)) + 
  scale_y_continuous("Proportion, elite researchers",limits = c(0,.05)) +
  scale_shape_manual("Period",values = c(1),guide=F) +
  geom_text_repel(aes(label=cshow),size=2.5,family = "Raleway",force = 2) + 
  scale_color_discrete(guide=F) + 
  labs(title = "B. Highest number elite")

# changes over time, positive and negative

f5t <- f7d
f5t$chg <- 0

for (i in 1:nrow(f5t)) {
  c <- f5t$inst[i]
  a <- f7c$p_elite[f7c$inst == c & f7c$year == 2010]
  b <- f7c$p_elite[f7c$inst == c & f7c$year == 2000]
  if (length(a) == 0 | length(b) == 0) {
    a <- 0
    b <- 0  
  }
  f5t$chg[i] <- a - b
}

f5t <- f5t %>% filter(chg != 0)

f7c$show <- 0
f5t$show <- 0
f5t <- f5t %>% arrange(-chg)
for (i in 1:10) {
  f5t$show[i] <- 1
}
cx <- f5t %>% filter(show != 0) %>% select(inst,show)

for (i in 1:nrow(cx)) {
  c <- cx$inst[i]
  s <- cx$show[i]
  f7c$show[f7c$inst == c] <- s
}

p6c <- f7c %>%
  filter(show == 1) %>%
  ggplot(aes(x = n_elite,y = p_elite, color = inst)) + 
  geom_point(shape = as.factor(1)) + 
  geom_line(arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) + 
  scale_x_log10("N, elite researchers",limits = c(10,1000)) + 
  scale_y_continuous("Proportion, elite researchers",limits = c(0,.05)) +
  scale_shape_manual("Period",values = c(1),guide=F) +
  geom_text_repel(aes(label=cshow),size=2.5,family = "Raleway",force = 2) + 
  scale_color_discrete(guide=F) + 
  labs(title = "C. Largest growth, proportion")


f7c$show <- 0
f5t$show <- 0
f5t <- f5t %>% arrange(chg)
for (i in 1:10) {
  f5t$show[i] <- 1
}
cx <- f5t %>% filter(show != 0) %>% select(inst,show)

for (i in 1:nrow(cx)) {
  c <- cx$inst[i]
  s <- cx$show[i]
  f7c$show[f7c$inst == c] <- s
}

p6d <- f7c %>%
  filter(show == 1) %>%
  ggplot(aes(x = n_elite,y = p_elite, color = inst)) + 
  geom_point(shape = as.factor(1)) + 
  geom_line(arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) + 
  scale_x_log10("N, elite researchers",limits = c(10,1000)) + 
  scale_y_continuous("Proportion, elite researchers",limits = c(0,.05)) +
  scale_shape_manual("Period",values = c(1),guide=F) +
  geom_text_repel(aes(label=cshow),size=2.5,family = "Raleway",force = 2) + 
  scale_color_discrete(guide=F) + 
  labs(title = "D. Smallest growth, proportion")



#agg_tiff("figures/fig_7.tiff",res=300,width=15,height=16,compression = "lzw",units="cm")
plot <- (p6a + p6b) / (p6c + p6d)
#invisible(dev.off())

