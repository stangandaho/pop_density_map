library(sf)
library(dplyr)
library(ggplot2)
library(showtext)
library(ggtext)

# Import africa 
africa <- read_sf("./sources/Shapefiles/Africa_adm0/Africa_adm.shp")
aes_limit <- africa %>% 
  filter(COUNTRY %in% c("MALI", "NIGER", "BURKINA FASO"))

# plot
# set color
fill1 <- "#c3c3c3"; fill2 <- "#179d00"; bg <- "#ffebd3"
pt_back <- "#9affa5"; pt_front <- "#3ddc00"


# set font
font_add("mb", "./fonts/Montserrat-Bold.ttf")
font_add("mr", "./fonts/Montserrat-Regular.ttf")
showtext_opts(dpi = 96)
showtext_auto()
font_size <- 6; font_color <- "#656565"


ggplot()+
  geom_sf(data = africa, fill = fill1, color = fill1)+
  geom_sf(data = aes_limit, fill = fill2, color = fill1, linewidth = 0.3)+
  geom_sf(data = st_centroid(aes_limit), color = pt_back, size = 3.5)+
  geom_sf(data = st_centroid(aes_limit), color = pt_front, size = 2)+
  ## Burkina Faso
  annotate("segment", x = -1.747736, xend = -1.747736, y = 12.27281, yend = 3, colour = pt_front)+
  annotate("text", x = -1.747736, y = 2, label = "Burkina Faso", 
           colour = font_color, family = "mb", size = font_size)+
  ## Mali
  annotate("segment", x = -3.571977, xend = -13, y = 17.31911, yend = 17.31911, colour = pt_front)+
  annotate("segment", x = -13, xend = -13, y = 17.31911, yend = 3, colour = pt_front)+
  annotate("text", x = -13, y = 2, label = "Mali", colour = font_color, family = "mb", size = font_size)+
  ## Niger
  annotate("segment", x = 9.34192, xend = 9.34192, y = 17.41705, yend = 3, colour = pt_front)+
  annotate("text", x = 9.34192, y = 2, label = "Niger", colour = font_color, family = "mb", size = font_size)+
  ## Year
  annotate("text", x = -11, y = -18, label = "AES", colour = fill2, family = "mb", size = 30, angle = 90)+
  annotate("text", x = -4, y = -18, label = "2024", colour = fill1, family = "mb", size = 25, angle = 90)+
  ## Caption
  labs(caption = "<p style='font-family:mr; color:#656565;font-size:20px; text-align:left;'>Stanislas Mahussi Gandaho<br>@stangandaho | Linkedin</p>")+
  theme_void()+
  theme(
    plot.margin = margin(0,0,0,0),
    plot.background = element_rect(color = bg, fill = bg),
    panel.background = element_rect(color = bg, fill = bg),
    plot.caption.position = "panel",
    plot.caption = element_markdown(margin = margin(b=0.3, r = 1, unit = "lines"), hjust = 1)
  )

## Save
ggsave("output/AES.jpg", dpi=300, width = 30, height = 30, units = "cm")

