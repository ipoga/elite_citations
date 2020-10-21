source("figures/figure_settings.R")

load("data/figS4_data.Rdat")

fs3$country[fs3$country == "usa"] <- "USA"
fs3$country[fs3$country == "denmark"] <- "Denmark"
fs3$country[fs3$country == "germany"] <- "Germany"
fs3$country[fs3$country == "peoples r china"] <- "Peoples Rep. China"
fs3$country[fs3$country == "taiwan"] <- "Taiwan"
fs3$country[fs3$country == "japan"] <- "Japan"
fs3$country[fs3$country == "south korea"] <- "South Korea"

p <- fs3 %>%
  filter(country %in% c("USA","Denmark","Germany","Peoples Rep. China","Taiwan","Japan","South Korea")) %>%
  ggplot(aes(x = year, y = rel_mean_papers, color = country)) + 
  geom_line() + 
  geom_point() +
  scale_x_continuous("Year") + 
  scale_y_continuous("Relative, weighted mean papers per author") + 
  scale_color_discrete("Country")

#agg_tiff("figures/fig_S4.tiff",res=300,width=15,height=11,compression = "lzw",units="cm")
plot <- p
#invisible(dev.off())
