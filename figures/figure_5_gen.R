source("figures/figure_settings.R")

load("data/fig5_data.Rdat")
load("data/fig6_data.Rdat")

f5c <- f5c.nophys
f6a <- f6a.nophys

f5c <- f5c %>% mutate(ccountry = case_when(
  country == "usa" ~ "USA",
  country == "netherlands" ~ "Netherlands",
  country == "england" ~ "England",
  country == "switzerland" ~ "Switzerland",
  country == "belgium" ~ "Belgium",
  country == "australia" ~ "Australia",
  country == "denmark" ~ "Denmark",
  country == "scotland" ~ "Scotland",
  country == "canada" ~ "Canada",
  country == "new zealand" ~ "New Zealand",
  country == "japan" ~ "Japan",
  country == "peoples r china" ~ "PR China",
  country == "france" ~ "France",
  country == "italy" ~ "Italy",
  country == "germany" ~ "Germany",
  country == "south africa" ~ "South Africa",
  country == "portugal" ~ "Portugal",
  country == "greece" ~ "Greece",
  country == "wales" ~ "Wales",
  country == "israel" ~ "Israel",
  country == "taiwan" ~ "Taiwan",
  country == "poland" ~ "Poland",
  country == "russia" ~ "Russia",
  country == "india" ~ "India",
  country == "brazil" ~ "Brazil",
  TRUE ~ country
))

f5c$country <- f5c$ccountry

f6a <- f6a %>% mutate(ccountry = case_when(
  country == "usa" ~ "USA",
  country == "netherlands" ~ "Netherlands",
  country == "england" ~ "England",
  country == "switzerland" ~ "Switzerland",
  country == "belgium" ~ "Belgium",
  country == "australia" ~ "Australia",
  country == "denmark" ~ "Denmark",
  country == "scotland" ~ "Scotland",
  country == "canada" ~ "Canada",
  country == "new zealand" ~ "New Zealand",
  country == "japan" ~ "Japan",
  country == "peoples r china" ~ "PR China",
  country == "france" ~ "France",
  country == "italy" ~ "Italy",
  country == "germany" ~ "Germany",
  country == "south africa" ~ "South Africa",
  country == "portugal" ~ "Portugal",
  country == "greece" ~ "Greece",
  country == "wales" ~ "Wales",
  country == "israel" ~ "Israel",
  country == "taiwan" ~ "Taiwan",
  country == "poland" ~ "Poland",
  country == "russia" ~ "Russia",
  country == "india" ~ "India",
  country == "brazil" ~ "Brazil",
  TRUE ~ country
))

f6a$country <- f6a$ccountry

f5c$cshow <- f5c$country
f5c$cshow[f5c$year != 2010] <- ""

f5c <- f5c %>% filter(n_total >= 1000 & n_elite >= 10)
f6a <- f6a %>% filter(n_total >= 2000 & n_elite >= 30)

f5c$show <- 0
f6a$show <- 0
f6a <- f6a %>% arrange(-p_elite)
for (i in 1:10) {
  f6a$show[i] <- 1
}
cx <- f6a %>% filter(show != 0) %>% select(country,show)

for (i in 1:nrow(cx)) {
  c <- cx$country[i]
  s <- cx$show[i]
  f5c$show[f5c$country == c] <- s
}

p6a <- f5c %>%
  filter(show == 1) %>%
  ggplot(aes(x = n_elite,y = p_elite, color = country)) + 
  geom_point(shape = as.factor(1)) + 
  geom_line(arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) + 
  scale_x_log10("N, elite researchers",limits = c(10,10000)) + 
  scale_y_continuous("Proportion, elite researchers",limits = c(0,.025)) +
  scale_shape_manual("Period",values = c(1),guide=F) +
  geom_text_repel(aes(label=cshow),size=2.5,family = "Raleway",force = 2) + 
  scale_color_discrete(guide=F) + 
  labs(title = "A. Highest proportion elite")

f5c$show <- 0
f6a$show <- 0
f6a <- f6a %>% arrange(-n_elite)
for (i in 1:10) {
  f6a$show[i] <- 1
}
cx <- f6a %>% filter(show != 0) %>% select(country,show)

for (i in 1:nrow(cx)) {
  c <- cx$country[i]
  s <- cx$show[i]
  f5c$show[f5c$country == c] <- s
}

p6b <- f5c %>%
  filter(show == 1) %>%
  ggplot(aes(x = n_elite,y = p_elite, color = country)) + 
  geom_point(shape = as.factor(1)) + 
  geom_line(arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) + 
  scale_x_log10("N, elite researchers",limits = c(10,10000)) + 
  scale_y_continuous("Proportion, elite researchers",limits = c(0,.025)) +
  scale_shape_manual("Period",values = c(1),guide=F) +
  geom_text_repel(aes(label=cshow),size=2.5,family = "Raleway",force = 2) + 
  scale_color_discrete(guide=F) + 
  labs(title = "B. Highest number elite")

# changes over time, positive and negative

f5t <- f6a
f5t$chg <- 0

for (i in 1:nrow(f5t)) {
  c <- f5t$country[i]
  a <- f5c$p_elite[f5c$country == c & f5c$year == 2010]
  b <- f5c$p_elite[f5c$country == c & f5c$year == 2000]
  if (length(a) == 0 | length(b) == 0) {
    a <- 0
    b <- 0  
  }
  f5t$chg[i] <- a - b
}

f5t <- f5t %>% filter(chg != 0)

f5c$show <- 0
f5t$show <- 0
f5t <- f5t %>% arrange(-chg)
for (i in 1:10) {
  f5t$show[i] <- 1
}
cx <- f5t %>% filter(show != 0) %>% select(country,show)

for (i in 1:nrow(cx)) {
  c <- cx$country[i]
  s <- cx$show[i]
  f5c$show[f5c$country == c] <- s
}

p6c <- f5c %>%
  filter(show == 1) %>%
  ggplot(aes(x = n_elite,y = p_elite, color = country)) + 
  geom_point(shape = as.factor(1)) + 
  geom_line(arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) + 
  scale_x_log10("N, elite researchers",limits = c(10,10000)) + 
  scale_y_continuous("Proportion, elite researchers",limits = c(0,.025)) +
  scale_shape_manual("Period",values = c(1),guide=F) +
  geom_text_repel(aes(label=cshow),size=2.5,family = "Raleway",force = 2) + 
  scale_color_discrete(guide=F) + 
  labs(title = "C. Largest growth, proportion")


f5c$show <- 0
f5t$show <- 0
f5t <- f5t %>% arrange(chg)
for (i in 1:10) {
  f5t$show[i] <- 1
}
cx <- f5t %>% filter(show != 0) %>% select(country,show)

for (i in 1:nrow(cx)) {
  c <- cx$country[i]
  s <- cx$show[i]
  f5c$show[f5c$country == c] <- s
}

p6d <- f5c %>%
  filter(show == 1) %>%
  ggplot(aes(x = n_elite,y = p_elite, color = country)) + 
  geom_point(shape = as.factor(1)) + 
  geom_line(arrow = arrow(angle = 20, length = unit(.2, "cm"), type = "open")) + 
  scale_x_log10("N, elite researchers",limits = c(10,10000)) + 
  scale_y_continuous("Proportion, elite researchers",limits = c(0,.025)) +
  scale_shape_manual("Period",values = c(1),guide=F) +
  geom_text_repel(aes(label=cshow),size=2.5,family = "Raleway",force = 2) + 
  scale_color_discrete(guide=F) + 
  labs(title = "D. Smallest growth, proportion")

#agg_tiff("figures/fig_6.tiff",res=300,width=15,height=16,compression = "lzw",units="cm")
plot <- (p6a + p6b) / (p6c + p6d)
#invisible(dev.off())
