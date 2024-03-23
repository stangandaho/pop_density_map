#packages loading
library(tidyverse)
library(ggtext)
library(ggimage)

#data creation
source("sdg_data.R")

# Set colors and fonts----
sdg_colors <- c("#eb1b2d", "#d3a028", "#269b48", "#c31e33", "#ef402b", "#00aed9", "#fdb713", "#8f1738", "#f36d25", "#e11383", "#f99d25", "#cf8d29", "#48773e", "#007dbc", "#3db049", "#01558b", "#173668")
#font set 
source("fonts.R")

ggplot(data = SDG_data, aes(x = sdg, y = score))+
  geom_col(aes(fill =  sdg, color = sdg), width = 1, show.legend = FALSE, color = "white")+
  labs(title = paste0("<span style = 'color: #28b368; font-size:50px;'>S</span>ustainable  <span style = 'color: #28b368;font-size:50px;'>D</span>evelopment  <span style = 'color: #28b368;font-size:50px;'>G</span>oals"),
       caption = paste0(caption = "<p style='font-family:mr; color:#656565;font-size:20px; text-align:left;'>Stanislas Mahussi Gandaho<br>@stangandaho | Linkedin</p>"))+
  geom_image(aes(image = if_else(sdg=="Life on Land", path, "./images/empty.png"), y = score - 1.33), size = 0.070, by = "width")+
  coord_polar(direction = 1, start = 0, clip = "off")+
  geom_image(data = data.frame(x = "Industry, Innovation, and Infrastructure",
                               y = -2.5,
                               z = "images/stanr.png"), aes(x = x, y = y, image = z), 
             size = 0.20)+
  ylim(-2.5,8.2)+
  theme_void()+
  scale_fill_manual(values = if_else(SDG_data$sdg=="Life on Land", sdg_colors, "#e6ddd8"))+
  # scale_color_manual(values = if_else(SDG_data$sdg=="Life on Land", sdg_colors, "#e6ddd8"))+
  theme(
    plot.background = element_rect(fill = "#ffffff", colour = "NA"),
    panel.background = element_rect(fill = "#ffffff", colour = "NA"),
    plot.margin = margin(0,0,0,0, unit = "lines"),
    plot.title = ggtext::element_markdown(family = "Tw Cen MT Condensed Extra Bold", colour = "#3f003d", hjust = 0.5, margin = margin(t = 1, b = 1, unit = "lines"), size = 17),
    plot.title.position = "plot",
    plot.caption = element_markdown(color = "#818181", 
                                    #margin = margin(b = 12, r = 12)
                                    ),
    plot.caption.position = "plot"
    
  )

ggsave("SDG2.png", dpi = 300, width = 10, height = 12, units = "cm")

library(phyloseq)