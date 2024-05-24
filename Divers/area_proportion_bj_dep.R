# Load package
library(sf)
library(ggplot2)
library(dplyr)
library(MetBrewer)
library(showtext)
library(ggtext)

# Import departement vector file
ben_dep <- read_sf("sources/Shapefiles/ben_admbnda_adm1_1m_salb_20190816.shp") %>% 
  select(ADM1_FR) %>% 
  # Add area colmun
  mutate(area = as.numeric(st_area(.)),
         # Calculate area proportion
         area_pr = round(area*100/sum(area), 2)
         ) %>% 
  # Convert to dataframe
  as.data.frame() %>% 
  select(- c(geometry, area)) %>% 
  # Sort the proportion
  arrange(desc(area_pr)) %>% 
  # Convert ADM1_FR into factor to keep order
  mutate(ADM1_FR = factor(ADM1_FR, levels = ADM1_FR))

# Plot
## Size and position
font_size <- 40
main_pt_size <- 5
sub_pt_size <- main_pt_size - 2
label_y_position <-  max(ben_dep$area_pr) - 4
## Font setting
font_add("mm", "fonts/Montserrat-Medium.ttf")
font_add("mb", "fonts/Montserrat-Bold.ttf")
font_add("mi", "fonts/Montserrat-Italic.ttf")
showtext_auto()
showtext_opts(dpi = 300)


ggplot(data = ben_dep)+
  geom_col(mapping = aes(x = ADM1_FR, y = label_y_position, colour = ADM1_FR), 
           width = 0.01, show.legend = F)+
  geom_point(mapping = aes(x = ADM1_FR, y = label_y_position, colour = ADM1_FR), 
             size = main_pt_size, show.legend = F)+
  geom_point(mapping = aes(x = ADM1_FR, y = label_y_position), 
             size = sub_pt_size, color = "white")+
  geom_col(mapping = aes(x = ADM1_FR, y = area_pr, fill = ADM1_FR), show.legend = F)+
  geom_text(mapping = aes(x = ADM1_FR, y = max(area_pr) + 5, 
                          label = paste0(ADM1_FR, "\n(", area_pr, "%)")),
            family = "mm", size = font_size - 30, color = "gray20", lineheight = 1)+
  labs(title = paste0("<p>Proportion of Area by <span style = 'color:#c30c3c'>Department</span> <br>in Benin</p>"),
       caption = "By: Stanislas Mahussi Gandaho \n@stangandaho")+
  theme_void()+
  scale_color_met_d(name = "Renoir")+
  scale_fill_met_d(name = "Renoir")+
  ylim(c(-20, 30))+
  coord_polar()+
  theme(
    plot.margin = margin(t = 1, b = 2, r = 0, l = 1, unit = "lines"),
    plot.background = element_rect(fill = "#fdf4dc", color = NA),
    plot.title = element_markdown(family = "mb", hjust = .5, size = font_size + 40),
    plot.caption = element_text(size = font_size - 15, color = "gray20", hjust = 1, family = "mi")
  )


ggsave("output/prop_area_dep_benin.jpeg", width = 20, height = 20)

showtext_opts(dpi = 300)
rm(list = ls())
