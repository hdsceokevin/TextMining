

#--------------------------------------------------------------------------
# 9. 연관 키워드 네트워크 맵 그리기
#--------------------------------------------------------------------------

# Rdata 확인
list.files(pattern = "Rdata")

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")


# 군집별 코퍼스 데이터를 csv 파일로 저장
for (i in 1:k) {
  subCorpus <-
    topicDocs[topicDocs$topicNo == i, c("id", "parsedContent")]
  filePathName <-
    paste("./output/", docName, "_Sub_Corpus_", i, ".csv", sep = "")
  write.csv(subCorpus, filePathName, row.names = FALSE)
}


# 군집별 csv 파일을 불러와서 앞 단계에 실시했던 것들을 약식을 실행하는 함수 생성
drawNetworkMap <- function(cvsFile, sparse, corLimit) {
  if (is.null(cvsFile))
    return(NULL)
  
  inputData <- readr::read_csv(cvsFile)
  
  # 코퍼스 생성 및 텍스트 전처리
  library(tm)
  corpus <- VCorpus(VectorSource(inputData$parsedContent))
  stopwords <- read.table("stopwords.txt")$V1
  corpus <- tm_map(corpus, removeWords, stopwords)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  
  # 동의어 처리
  for (j in seq(corpus)) {
    corpus[[j]] <- gsub("강립", "강림", corpus[[j]])
  }
  
  # DTM 생성을 위한 객체 속성 변경
  corpus <- tm_map(corpus, PlainTextDocument)
  
  # Document Term Matrix 생성 : Tf-Idf값을 가중치로 이용
  dtm <- DocumentTermMatrix(
    corpus,
    control = list(
      removeNumbers = FALSE,
      wordLengths = c(2, Inf),
      weighting = function(x)
        weightTfIdf(x, normalize = T)
    )
  )
  
  # 단어 양옆 스페이스 제거 및 2음절 이상 단어만 남기기
  colnames(dtm) <- trimws(colnames(dtm))
  dtm <- dtm[, nchar(colnames(dtm)) >= 2]
  dtm <- removeSparseTerms(dtm, as.numeric(sparse))
  
  # 매트릭스 크기 조절하기
  # 상관계수가 0.1 이하인 것은 0으로 강제 치환하여 네트워크 맵에 출력되지 않도록 조절
  corTerms <- cor(as.matrix(dtm))
  corTerms[corTerms <= corLimit] <- 0
  
  # 네트워크 객체 생성
  # 만약 상관계수행렬이 비대칭형인 경우, matrix.type="bipartite" 추가!!
  # 상관계수행렬이 비대칭이 되는 경우는 예를 들어 rowSums()가 0인 행을 삭제하면 발생
  library(network)
  netTerms <- network(corTerms, directed = FALSE)
  
  # 중심중개성 계산
  # 중심중개성은 네트워크에서 서로 연결되는 각 노드간 가장 짧은 거리를 갖는 노드가 
  # 큰 값을 가짐. 사람으로 치면 인맥이 넓은 사람을 의미! 
  # [참조] https://en.wikipedia.org/wiki/Betweenness_centrality
  # [주의] betweenness() 함수는 igraph 라이브러리에도 있고,
  # 아래와 같이 sna 패키지를 붙여서 실행하지 않으면 에러가 발생함!!
  # search()를 실행하여 igraph 함수가 load 되어 있으면 detach하는 것도 한 방법임
  # detach("package:igraph", unload=TRUE)
  btnTerms <- sna::betweenness(netTerms)
  
  # 중개중심성이 상위 10%인 노드의 색상을 별도로 할당하여 시각화 처리함
  netTerms %v% "mode" <-
    ifelse(btnTerms >= quantile(btnTerms, 0.9), "Top10%", "Rest90%")
  
  nodeColors <- c("Top10%" = "yellow",
                  "Rest90%" = "grey90")
  
  # 엣지크기 지정 (이번 예제에서는 상관계수의 2배)
  set.edge.value(netTerms, "edgeSize", corTerms * 2)
  
  # 네트워크 지도 그리기
  library(GGally)
  ggnet2(
    # 네트워크 객체
    net = netTerms,
    # 겹치는 노드를 흩어지게 처리
    layout.par = list(cell.jitter = 0.001),
    # 노드에 레이블 출력 여부
    label = TRUE,
    # 레이블 폰트 크기
    label.size = 4,
    # 노드의 색상 지정
    # "white"처럼 전체 노드에 대해 하나의 특정 색상을 지정할 수 있음
    node.color = "mode",
    # 노드의 색상
    palette = nodeColors,
    # 노드의 크기를 degree cetrality 값에 따라 차등
    # [주의] degree() 함수도 igraph 라이브러리에 영향을 받음
    size = sna::degree(netTerms),
    # 엣지의 굵기를 단어간 상관계수에 따라 차등
    edge.size = "edgeSize",
    # 노드의 연결된 개수 최소값 지정
    size.min = 2,
    # 범례 위치 지정
    legend.position = "None",
    # 네트워크 모양 지정 ("circle","kamadakawai","fruchtermanreingold","circrand")
    mode = "fruchtermanreingold",
    # 폰트 지정
    family = "NanumGothic"
  )
}


# 특정 그룹으로 네트워크맵 그리기
# sparse 크기가 작을수록 DTM에 포함되는 단어의 개수가 크게 감소! 
# corLimit 크기가 클수록 강하게 연결되어 있는 단어들만 사용됨!
drawNetworkMap(cvsFile = "./output/Blog_지름신_201701_Sub_Corpus_8.csv",
               sparse = 0.92,
               corLimit = 0.2)


# Rdata로 저장
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
