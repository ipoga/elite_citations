require(tidyverse)
require(ggrepel)
require(patchwork)
require(ragg)

windowsFonts(Raleway=windowsFont("TT Raleway"))

paper_theme <- theme_classic() + theme(panel.background = element_rect(fill = "white"),
                                       panel.grid.major = element_line(colour = "grey80", linetype="dashed", size=.25)) + 
  theme(
    legend.position="bottom",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    plot.title = element_text(size = 7, face = "bold"),
    plot.subtitle = element_text(size = 7),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.spacing = unit(0,"cm"),
    legend.box.spacing = unit(0,"cm"),
    panel.spacing = unit(.5,"lines"),
    strip.text = element_text(size = 7),
  text = element_text(family = "Raleway"))
theme_set(paper_theme)
elite_cols <- c("#000000","#A61A00","#1446A0","#92AC86","#97EAD2")