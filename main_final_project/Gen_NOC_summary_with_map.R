library(dplyr)
# 數據合併並過濾
data_regions <- data %>%
    left_join(noc, by="NOC") %>%
    filter(!is.na(region), Medal %in% c("Gold", "Silver", "Bronze"))

# 統計獎牌數量
medal_counts <- data_regions %>%
    group_by(Year, Event, NOC,region, Medal) %>%
    summarise(count = 1, .groups = 'drop') %>%
    pivot_wider(names_from = Medal, values_from = count, values_fill = list(count = 0))

medal_counts <- medal_counts %>%
    group_by(NOC, region) %>%
    summarise(
    Gold = sum(Gold, na.rm = TRUE),
    Silver = sum(Silver, na.rm = TRUE),
    Bronze = sum(Bronze, na.rm = TRUE),
    .groups = 'drop'
    )

# 合併並準備世界地圖數據
world_map <- map_data("world")[, c(1, 2, 5)]  # 簡化讀取方式
map_data_final <- merge(world_map, medal_counts, by = "region", all.x = TRUE)

# 處理 NA 值
map_data_final[is.na(map_data_final)] <- 0

# 去除重複、NA數據和NOC=0的數據
unique_data <- distinct(map_data_final, region, .keep_all = TRUE) %>%
    filter(!is.na(NOC), NOC != "0")

region_centers <- world_map %>%
    group_by(region) %>%
    summarise(
    center_long = mean(long, na.rm = TRUE),
    center_lat = mean(lat, na.rm = TRUE)
    )
unique_data <- unique_data %>%
    left_join(region_centers, by = "region") %>%
    mutate(long = center_long, lat = center_lat) %>%
    select(-center_long, -center_lat)  # 移除暫存的中心位置列
#ADD top 3 sports COLUMN
unique_nocs <- unique(data$NOC)
col_names <- c("NOC","Sport1","Gold1","Silver1","Bronze1","Sport2","Gold2","Silver2","Bronze2","Sport3","Gold3","Silver3","Bronze3")
tmp_data <- data.frame(matrix(ncol = length(col_names), nrow = length(unique_nocs)))
colnames(tmp_data) <- col_names
tmp_data$NOC <- unique_nocs

# 为每个NOC计算前三种运动的奖牌
results <- lapply(unique_nocs, function(NOC) {
    subset_data <- data[data$NOC == NOC & !is.na(data$Medal),]
    summary <- subset_data %>%
    group_by(Year, Sport, Event, Medal) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(Sport) %>%
    summarise(
        Gold = sum(Medal == "Gold"),
        Silver = sum(Medal == "Silver"),
        Bronze = sum(Medal == "Bronze"),
        .groups = 'drop'
    ) %>%
    mutate(Total = Gold * 2 + Silver * 1.5 + Bronze * 1) %>%
    arrange(desc(Total)) %>%
    slice_head(n = 3) %>%
    ungroup()
    # 将结果转换为向量
    c(NOC, unlist(c(summary[1,1], summary[1,2:4], summary[2,1], summary[2,2:4], summary[3,1], summary[3,2:4])))
})

# 组合结果回数据框
results_df <- do.call(rbind, results)
colnames(results_df) <- col_names
results_df=data.frame(results_df)
unique_data <- left_join(unique_data, results_df, by = "NOC")

#save csv
write.csv(unique_data, "NOC_summary_with_map.csv", row.names = TRUE)








