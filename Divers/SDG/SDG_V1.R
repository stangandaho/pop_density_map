# Loading necessaries packages
library(tidyverse)
library(showtext)
library(ggimage)
library(ggtext)

# Creation of data frame----
sdg <- c("No Poverty", "Zero Hunger", "Good Health and Wellbeing", 
         "Quality Education", "Gender Equality", "Clean Water and Sanitation", 
         "Affordable and Clean Energy", "Decent Work and Economic Growth",
         "Industry, Innovation, and Infrastructure","Reduced Inequalities",
         "Sustainable Cities and Communities", 
         "Responsible Consumption and Products", "Climate Action", 
         "Life Bellow Water", "Life on Land", "Peace, Justice and Strong Institutions", "Partnerships for the Goals")
score <- c(4,4,4,5.5,4,4,4.75,5.5,4,4.75,5.5,4.75,5.5,4.75,8,5.5,5.5)

SDG_data <- data.frame(sdg, score+1)
SDG_data$sdg <- factor(SDG_data$sdg, levels = SDG_data$sdg)

# Add images----
wikipedia <- "https://en.wikipedia.org/wiki/Sustainable_Development_Goal_"
goal <- 1:17
symbol <- paste0(wikipedia, goal,"#/media/File:480px-Sustainable_Development_Goal_", goal, ".png")
SDG_data <- SDG_data |>
  #mutate(image = symbol) |> 
  mutate(path = file.path("images", paste(SDG_data$sdg, tools::file_ext(symbol), sep = ".")))

labels <- setNames(paste0("<img src='", SDG_data$path, "' width='35' />"), SDG_data$sdg)

# Set colors and fonts----
sdg_colors <- c("#eb1b2d", "#d3a028", "#269b48", "#c31e33", "#ef402b", "#00aed9", "#fdb713", "#8f1738", "#f36d25", "#e11383", "#f99d25", "#cf8d29", "#48773e", "#007dbc", "#3db049", "#01558b", "#173668")
font_add("Berlin Sans FB Demi", "BRLNSDB.TTF")
font_add("Nirmala UI", "NirmalaB.ttf")
font_add("Tw Cen MT Condensed Extra Bold", "TCCEB.TTF")
showtext_auto()


p <- ggplot(data = SDG_data, aes(x = sdg, y = score))+
  geom_col(aes(fill =  sdg, color = sdg), width = 1, show.legend = FALSE)+
labs(title = paste0("<span style='color:#393939; font-size:115px'>Sustainable Development Goals</span> <span style='color:#949494; font-size:85px'>plotting in </span> <img src='./images/R.png' width='30' height='25'/>"))+
  geom_image(aes(image = path, y = score - 1), size = 0.055, by = "width")+
  coord_polar(direction = 1, start = 0, clip = "off")+
  geom_image(data = data.frame(x = "Industry, Innovation, and Infrastructure",y = -2, z = "images/SDN.png"), aes(x = x, y = y, image = z), size = 0.17)+
  ylim(-2,8)+
  theme_void()+
  labs(caption = paste0("<img src='./images/social/instagram.png' width='20' height='20'/> <img src='./images/social/twitter.png' width='18' height='18'/> <img src='./images/social/linkedin.png' width='15' height='15'/> </br>  @stangandaho"))+
  scale_fill_manual(values = sdg_colors)+
  scale_color_manual(values = sdg_colors)+
  theme(
    plot.background = element_rect(fill = "#f9e0d9", colour = "#f9e0d9"),
    panel.background = element_rect(fill = "#f9e0d9", colour = "#f9e0d9"),
    plot.margin = margin(0,0,0,0),
    plot.title = ggtext::element_markdown(family = "Tw Cen MT Condensed Extra Bold", colour = "gray30", hjust = 0.5, margin = margin(t = 20)),
    plot.title.position = "plot",
    plot.caption = element_markdown(size = 60, color = "#818181", 
                                    margin = margin(b = 12, r = 12)),
    # plot.caption.position = "plot"
    
  )
p
ggsave("SDG1.png", dpi = "retina", width = 21, height = 23, units = "cm", plot = p)
fonts <- font_files()
View(fonts)


# "element_markdown"       "element_textbox"       
# "element_textbox_simple" "geom_richtext"         
# "geom_textbox"           "GeomRichtext"          
# "GeomRichText"           "GeomTextBox"  
?element_textbox
