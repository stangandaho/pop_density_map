sdg <- c("No Poverty", "Zero Hunger", "Good Health and Wellbeing", 
         "Quality Education", "Gender Equality", "Clean Water and Sanitation", 
         "Affordable and Clean Energy", "Decent Work and Economic Growth",
         "Industry, Innovation, and Infrastructure","Reduced Inequalities",
         "Sustainable Cities and Communities", 
         "Responsible Consumption and Products", "Climate Action", 
         "Life Bellow Water", "Life on Land", "Peace, Justice and Strong Institutions", "Partnerships for the Goals")
score <- c(4,4,4,5.5,4,4,4.75,5.5,4,4.75,5.5,4.75,5.5,4.75,8,5.5,5.5)

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
# color
sdg_colors <- c("#eb1b2d", "#d3a028", "#269b48", "#c31e33", "#ef402b", "#00aed9", "#fdb713", "#8f1738", "#f36d25", "#e11383", "#f99d25", "#cf8d29", "#48773e", "#007dbc", "#3db049", "#01558b", "#173668")