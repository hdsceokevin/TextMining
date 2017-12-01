

#--------------------------------------------------------------------------
# 8. Topic clustering with LDA
#--------------------------------------------------------------------------

# Rdata 확인
list.files(pattern = "Rdata")

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")


# LDA를 실행하기에 앞서 Tf 기준으로 작성된 DTM 크기를 줄일 필요가 있음
# 크기를 줄이는 기준으로 각 단어별 Tf-Idf 값을 계산하여 기준 이하는 제외함
# DocumentTermMatrix()에 Tf-Idf 가중치를 부여하여 생성한 dtmW이 이미 있으나
# Tf-Idf만 추출할 수 없기 때문에 별도로 계산하는 것임

# dtm의 요소 중 i,j,v를 활용하여 Tf-Idf 값 계산
# dtm은 일부 값(term frequency)을 제외하고 0이 채워져 있는 행렬이므로
# 값이 있는 위치와 해당 값을 i, j, v에 할당해놓음
# dtm$i : dtm의 행번호
# dtm$j : dtm의 열번호
# dtm$v : 해당 위치의 값(value)

# Tf를 계산하는 방식은 여러 가지임 ("https://en.wikipedia.org/wiki/Tf%E2%80%93idf")
# 이번 예제에서는 각 셀 값을 해당 행의 합으로 나눈 후, 단어(열) 기준으로 평균한 값을 이용함
library(slam)
Tf <- tapply(dtm$v / row_sums(dtm)[dtm$i], dtm$j, mean)

# Idf는 전체 문서의 개수를 해당 단어가 사용된 문서의 개수로 나눈 후 로그를 씌운 값
library(tm)
Idf <- log2(nDocs(dtm) / col_sums(dtm > 0))

# 각 단어별 Tf-Idf 값을 계산
termTfIdf <- Tf * Idf

# 그래프로 분포 확인
boxplot(termTfIdf)
ggplot(data = as.data.frame(termTfIdf), aes(x = termTfIdf)) +
  geom_histogram(breaks = seq(0, 0.5, 0.01),
                 col = "black",
                 fill = "white")

# Tf-Idf 값 범위 확인
range(termTfIdf)
summary(termTfIdf)

# 특정 Tf-Idf 값을 기준으로 dtm 크기를 줄인 dtm1 만들기
# 이 때, 기준이 되는 Tf-Idf 값은 1분위수로 정함
dtm1 <- dtm[, termTfIdf > summary(termTfIdf)[2]]
dtm1 <- dtm1[row_sums(dtm1) > 0, ]


# LDA 실행 전 문서명, seed 및 클러스트 수(k) 설정
docName  # "Blog_지름신_201701"
SEED <- 123
k <- 8

# LDA 실행
library(topicmodels)
lda <- LDA(dtm1, k, control = list(seed = SEED))

# 토픽별 핵심단어 20개씩 할당
# 토픽별 우선순위는 없으나, 같은 토픽 내 단어는 순서가 중요!!
(topicTerms <- terms(lda, 20))

# Topic terms 저장할 폴더 유무 확인 : output
# 해당 폴더가 없으면 새로 생성
if (is.null(dir(pattern = "output")) == TRUE)
  dir.create("./output")

# 토픽별 핵심 단어 파일로 출력하기
(filePathName <-
    paste0("./output/", docName, "_", k, "_LDA_Result_terms.xlsx"))

# 폴더 생성 후 파일 저장
library(xlsx)
write.xlsx(topicTerms, filePathName, row.names = F)


# 결과 객체 생성
# 문서별 토픽 번호 할당 (1개씩!)
topicNo <- topics(lda, 1)
head(topicNo)

# 문서별 토픽 확률 계산
topicPr <- posterior(lda)$topics
head(topicPr)

# 확률 최대값 컬럼 생성
topicMaxPr <- apply(topicPr, 1, max)
head(topicMaxPr)

# 최종 데이터프레임 생성
topicDocs <- data.frame(
  id = rownames(topicPr),
  topicNo = topicNo,
  topicMaxpr = topicMaxPr,
  row.names = NULL
)

topicDocs <- merge(topicDocs, parsedDf, by = "id", all.x = T)
head(topicDocs)
table(topicDocs$topicNo)

# 특정 군집에 속한 블로그만 확인
topicDocs[topicDocs$topicNo == '7', "id"]
topicDocs[topicDocs$topicNo == '8', "parsedContent"]

# 문서별 토픽 번호 및 확률 저장
(filePathName <-
    paste0("./output/", docName, "_", k, "_LDA_Result_docs.xlsx"))
write.xlsx(topicDocs, filePathName, row.names = F)


# 단어별 토픽 확률값 출력(일부만!)
posterior(lda)$terms[1:10]

# 토픽 클러스터 시각화
# phi는 각 단어별 토픽에 포함될 확률값
phi <- posterior(lda)$terms %>% as.matrix

# theta는 각 문서별 토픽에 포함될 확률값
theta <- posterior(lda)$topics %>% as.matrix

# vocab는 전체 단어 리스트
vocab <- colnames(phi)

# 각 문서별 content 글자수
docLen <- c()
for (i in 1:nrow(topicDocs)) {
  docLen <- c(docLen, nchar(topicDocs$parsedContent[i]))
}

# 각 단어별 빈도수 계산
dtm1Mat <- as.matrix(dtm1)
freqMat <- data.frame(
  term = colnames(dtm1Mat),
  freq = colSums(dtm1Mat),
  row.names = NULL
)
head(freqMat)


# 위에서 구한 값들을 파라메터 값으로 넘겨서 LDA 시각화
source("createNamJson_v2.R")
jsonLDA <- createNamJson(
  phi = phi,
  theta = theta,
  vocab = vocab,
  doc.length = docLen,
  term.frequency = freqMat$freq,
  mds.method = canberraPCA
)  # canberraPCA가 안될 때, jsPCA 할당

# jsonLDA라고 하는 동적 시각화 결과를 제대로 출력하기 위해서
# Windows 사용자는 "톰캣"을 설치해야 하고, MAC 사용자는 그냥 실행 가능!
# 웹브라우저는 크롬을 사용할 것을 추천합니다.
# [참고] 톰캣 설치 ("http://taesikman1.tistory.com/56")
library(LDAvis)
library(servr)
serVis(jsonLDA, open.browser = T)
# 위 명령을 실행하면 Viewer 윈도우에 LDA 시각화 그래프가 생성됨
# 마우스 오른쪽 클릭 후, "Open frame in a new window"를 선택하면 확대 가능
# 크게 확대된 새 창에서 각 클러스터 위에 마우스를 대면
# 오른쪽에 클러스터를 구성하는 단어 리스트가 출력됨 (순서 중요!)


# Rdata로 저장
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
