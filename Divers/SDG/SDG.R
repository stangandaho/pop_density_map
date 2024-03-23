                      #################################
                      ### Stanislas Mahussi Gandaho ###
                      ### stangandaho@gmail.com     ###
                      ### @stangandaho - LinkedIn   ###
                      #################################

                        #Sustainable Development Goals
                      
# Load package                     
library(dplyr)
library(ggplot2)
library(showtext)
library(ggimage)
library(ggtext)

# Set working directory
sub_main_dir <- "./Divers/SDG"
if (!dir.exists(sub_main_dir)) {dir.create(sub_main_dir)}
setwd(sub_main_dir)
# Creation SDG data frame
sdg <- c("No Poverty", "Zero Hunger", "Good Health and Wellbeing", 
        "Quality Education", "Gender Equality", "Clean Water and Sanitation", 
        "Affordable and Clean Energy", "Decent Work and Economic Growth",
        "Industry, Innovation, and Infrastructure","Reduced Inequalities",
        "Sustainable Cities and Communities", 
        "Responsible Consumption and Products", "Climate Action", 
        "Life Bellow Water", "Life on Land", "Peace, Justice and Strong Institutions", "Partnerships for the Goals")
score <- c(4,4,4,5.5,4,4,4.75,5.5,4,4.75,5.5,4.75,5.5,4.75,4.75,5.5,5.5)+2.5
SDG_data <- data.frame(sdg, score)
SDG_data$sdg <- factor(SDG_data$sdg, levels = SDG_data$sdg)

# Data frame adding symbol image of each goal
wikipedia <- "https://en.wikipedia.org/wiki/Sustainable_Development_Goal_"
goal <- 1:17
symbol <- paste0(wikipedia, goal,"#/media/File:480px-Sustainable_Development_Goal_", goal, ".png")

SDG_data <- SDG_data |>
  mutate(image = symbol) |> 
  mutate(path = file.path("images", paste(SDG_data$sdg, tools::file_ext(symbol), sep = ".")))
labels <- setNames(paste0("<img src='", SDG_data$path, "' width='35' />"), SDG_data$sdg)
 
# Download images in local
for (i in 1:17) {
  download.file(SDG_data$image[i], SDG_data$path[i])
}

# Or download manually images and store them in images folder. Don't forget to 
# rename to same name as goals in data frame.

# Set colors
sdg_colors <- c("#eb1b2d", "#d3a028", "#269b48", "#c31e33", "#ef402b", "#00aed9", 
                "#fdb713", "#8f1738", "#f36d25", "#e11383", "#f99d25", "#cf8d29", 
                "#48773e", "#007dbc", "#3db049", "#01558b", "#173668")

# Add fonts to current session
font_add("mb", "../../fonts/Montserrat-Bold.ttf")
font_add("mr", "../../fonts/Montserrat-Regular.ttf")
font_add("pld", "../../fonts/Palm-Leaf-Demo.otf")
showtext_auto()

# Plot
title_fs <- 300
caption_fs <- 60
ggplot(data = SDG_data, aes(x = sdg, y = score))+
  geom_col(aes(fill =  sdg, color = sdg), width = 1, show.legend = FALSE)+
  coord_polar(direction = 1, start = 0, clip = "off")+
  geom_image(aes(image = path, y = score - 1), size = 0.055, by = "width")+
  geom_image(data = data.frame(x = "Industry, Innovation, and Infrastructure",y = -2, z = "images/SDN.png"), aes(x = x, y = y, image = z), size = 0.17)+
  ylim(-2,8)+
  theme_void()+
  labs(title = paste0("<span style = font-family:pld; font-size:230px>SDG</span>s"),
       caption = paste0(caption = "<p style='font-family:mr; color:#FFFFFF;font-size:70px; text-align:left;'>Stanislas Mahussi Gandaho | @stangandaho - Linkedin</p>")
       )+
  scale_fill_manual(values = sdg_colors)+
  scale_color_manual(values = sdg_colors)+
  theme(
    plot.background = element_rect(fill = "gray10", colour = "gray10"),
    panel.background = element_rect(fill = "gray10", colour = "gray10"),
    plot.margin = margin(0,0,0,0),
    plot.title = ggtext::element_markdown(colour = "white", family = "mb",
                                          hjust = 0, margin = margin(t = 20), size = title_fs),
    plot.title.position = "plot",
    plot.caption = element_markdown(size = caption_fs, color = "white", 
                                    margin = margin(b = 12, r = 12)),
    plot.caption.position = "plot"
    
  )

# Save plot
ggsave("../../output/SDG.jpeg", width = 35, height = 40, units = "cm")
  
