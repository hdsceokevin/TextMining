

#--------------------------------------------------------------------------
# 2. 탐색점 데이터 분석(EDA) 및 시각화
#--------------------------------------------------------------------------

# Rdata 확인
list.files(pattern = "Rdata")

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")


# 날짜별 포스팅 수 확인
postByDate <-
  aggregate(tmdf$id, by = list(date = tmdf$postdate), FUN = "length")
postByDate$date <- as.Date(postByDate$date, format = "%Y.%m.%d")
colnames(postByDate)[2] <- "count"
head(postByDate, 10)

ggplot(data = postByDate, aes(x = date, y = count)) +
  geom_line() +
  labs(x = "날짜", y = "블로그 수", title = "일자별 블로그 수") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  theme(
    plot.title        = element_text(
      color = "black",
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    axis.title.x      = element_text(size = 12, face = "bold"),
    axis.title.y      = element_text(size = 12, face = "bold"),
    axis.text.x       = element_text(angle = 90),
    axis.ticks.length = unit(0, "cm"),
    panel.background  = element_blank(),
    panel.grid        = element_blank()
  )


# Rdata로 저장하기
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
