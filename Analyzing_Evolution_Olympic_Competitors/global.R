library(dplyr)
library(plotly)
library(ggplot2)
library(RColorBrewer)

# Load data
data <- read.csv("athlete_events.csv", header = TRUE, sep=',', na.strings = c("", "NA"))

womenInOlympics <- data %>%
  filter(Sex == 'F') %>%
  group_by(Year, Season) %>%
  summarize(Count = n()) %>%
  arrange(Year)

gold_medals <- data%>%
  filter(Medal=='Gold') 

notNullMedals <- gold_medals %>%
  filter(!is.na(Height) & !is.na(Weight))

createColorScaleAndNormalize <- function(data, count_col, color_palette = "YlOrRd", num_colors = 100) {
  # 创建颜色梯度
  color_palette <- colorRampPalette(brewer.pal(9, color_palette))(num_colors)
  colorscale <- lapply(seq(0, 1, length.out = num_colors), function(i) {
    list(i, color_palette[i * (num_colors - 1) + 1])
  })
  
  # norm
  max_count <- max(data[[count_col]], na.rm = TRUE)
  min_count <- min(data[[count_col]], na.rm = TRUE)
  normalized_col_name <- paste0(count_col, "Normalized")
  data[[normalized_col_name]] <- (data[[count_col]] - min_count) / (max_count - min_count)
  #print(data)
  list(data = data, colorscale = colorscale)
}
