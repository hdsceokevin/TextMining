

#--------------------------------------------------------------------------
# 7. 단어 빈도수 시각화
#--------------------------------------------------------------------------

# Rdata 확인
list.files(pattern = "Rdata")

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")


# dtmW에 언급된 단어(term)별 빈도수 생성
(wordsFreq <- round(colSums(as.matrix(dtmW)), 2))

# 시각화용 빈도수 테이블(데이터프레임) 생성
# 사용된 단어의 총 개수 확인
length(wordsFreq)

# 내림차순 정렬하고, 상위 10개와 하위 10개 확인
wordsFreq <- wordsFreq[order(wordsFreq, decreasing = T)]
head(wordsFreq, 20)
tail(wordsFreq, 20)

# 특정 빈도 사이값을 갖는 단어 구하기
# findFreqTerms(dtm, lowfreq=20, highfreq=320)

# 단어 빈도를 막대그래프로 그리기 위해 데이터 프레임으로 변환 후 내림차순 정렬
wordDf <-
  data.frame(word = names(wordsFreq),
             freq = wordsFreq,
             row.names = NULL)
wordDf <- wordDf[order(wordDf$freq, decreasing = T), ]

# 총 빈도수 상위 20개 단어로 막대그래프 그리기 (내림차순 정렬)
library(ggplot2)
ggplot(head(wordDf, 21)[-1, ], aes(x = reorder(word, -freq), y = freq)) +
  #theme_clean() +
  theme(
    plot.title        = element_text(
      color = "black",
      size = 14,
      face = "bold",
      hjust = 0.5
    ),
    axis.title.x      = element_text(color = "blue", size = 12, face =
                                       "bold"),
    axis.title.y      = element_text(color = "#993333", size = 12, face =
                                       "bold"),
    axis.ticks.length = unit(0, "cm"),
    panel.background  = element_blank(),
    panel.grid        = element_blank()
  ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = freq), vjust = -1) +
  theme(axis.text.x = element_text(family = "NanumGothic")) +
  labs(x = "", y = "빈도수", title = "빈도수 높은 단어 현황")


#--------------------------------------------------------------------------
# 워드 클라우드 그리기
#--------------------------------------------------------------------------

# 팔레트 지정
library(RColorBrewer)
display.brewer.all()
(pal <- brewer.pal(n = 8, name = "Set2"))  # n:사용할 색깔 수, name:색깔 조합 이름
# http://colorbrewer2.org/ 참고
display.brewer.pal(8, "Set2")

library(wordcloud2)
(
  wc2 <-
    wordcloud2(
      wordDf[-1, ],
      size = 1,
      fontFamily = "NanumGothic",
      color = pal,
      backgroundColor = "white",
      minRotation = -pi / 4,
      maxRotation = pi / 4,
      shuffle = T,
      rotateRatio = 0.25,
      shape = "circle",
      ellipticity = 0.6
    )
)

# html 형태의 이미지를 화면 캡쳐하는 방식으로 이미지 저장
library(htmlwidgets)
saveWidget(wc2, "wc2.html", selfcontained = F)

# webshot() 함수는 인터넷에 렌더링된 모습을 캡쳐해서 png 파일로 저장
# 이 방법은 브라우저에 html이 렌더링되는 과정을 흉내낸 것이므로 시간이 소요됨
# 즉, webshot()의 delay 인자값을 충분히 크게 할당해야 함
# 하지만 출력되는 결과는 매우 실망스러울 수 있으므로,
# Viewer 창에 출력된 wordcloud 이미지를 확대(zoom)하여 copy하는 것을 추천함
library(webshot)
webshot::install_phantomjs()

webshot(
  "wc2.html",
  paste0("./Image/Wordcloud for ", docName, ".png"),
  delay = 120,
  vwidth = 1080,
  vheight = 1080
)


#--------------------------------------------------------------------------
# tree Map 그리기
#--------------------------------------------------------------------------

# png 파일 저장할 폴더 만들기
dir.create("./Image")

# png 파일로 저장
png(
  paste0("./Image/Tree Map for ", docName, ".png"),
  width = 1080,
  height = 1080,
  pointsize = 20
)

library(treemap)
treemap(
  wordDf[-1, ],
  # 대상 데이터 설정
  title = "Word Tree Map",
  # 제목 설정
  index = c("word"),
  # 박스 안에 들어갈 변수 설정
  vSize = "freq",
  # 박스 크기 기준
  fontfamily.labels = "NanumGothic",
  # 폰트 설정
  fontsize.labels = 14,
  # 폰트 크기 설정
  palette = pal,
  # 위에서 만든 팔레트 정보 입력
  border.col = "white"
)               # 경계선 색깔 설정

dev.off()


# Rdata로 저장
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
