

#--------------------------------------------------------------------------
# 6. 연관 키워드 추출
#--------------------------------------------------------------------------

# Rdata 확인
list.files(pattern = "Rdata")

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")


# 관심 있는 단어와의 연관 키워드 구하기 (피어슨 상관계수 기준)
# 최소상관계수(이번 예제에서는 0.1) 이상인 단어만 출력!!
library(tm)
findAssocs(x = dtm,
           terms = query,
           corlimit = 0.1)

# 상관계수 행렬 구하기
# 먼저 DTM을 행렬로 변환!
dtmMat <- as.matrix(dtm)
dim(dtmMat)
dtmMat[1:10, ]

# cor() 함수를 이용하여 단어 간 상관계수행렬을 구할 수 있음
# DTM 행렬이 Term*Term 행렬로 변환됨
corTerms <- cor(dtmMat)
corTerms[1:10, 1:10]

# 키워드 유무 확인
grep(query, colnames(corTerms), value = T)

# 연관 키워드가 있는 컬럼의 전체 단어를 한꺼번에 출력
(corRef <- corTerms[, query])

# 연관 키워드 중 상관계수 기준으로 상위 20개만 보기
corRef <- corRef[order(corRef, decreasing = T)]
corRef[1:21]


# TF-IDF 값으로 연관 키워드 추출하기
# 먼저 Tf-IDF 가중치로 DTM 생성
library(tm)
(dtmW <- DocumentTermMatrix(
  corpus,
  control = list(
    wordLengths = c(2, Inf),
    weighting = function(x)
      weightTfIdf(x, normalize = T)
  )
))

# 단어(=컬럼명) 양옆 스페이스 제거
(colnames(dtmW) <- trimws(colnames(dtmW)))

# 단어 길이가 2 이상인 컬럼만 남기기
dtmW <- dtmW[, nchar(colnames(dtmW)) >= 2]
dim(dtmW)

# 네트워크의 크기를 줄이기 위해 sparse 인자의 숫자를 줄임
# 역시 이 과정을 통해 DTM의 단어 개수가 2389로 크게 줄었음
(dtmW <- removeSparseTerms(dtmW, sparse = as.numeric(0.99)))

# tm 패키지의 findAssocs() 함수 이용
# 상관계수가 0.1 이상인 단어들만 추출!!
findAssocs(x = dtmW, term = query, corlimit = 0.1)

# 상관계수행렬 구하기
dtmWMat <- as.matrix(dtmW)
corTermsW <- cor(dtmWMat)

# 키워드 유무 확인
grep(query, colnames(corTermsW), value = T)

# 연관 키워드가 있는 컬럼의 전체 단어를 한꺼번에 출력
(corRefW <- corTermsW[, query])

# 연관 키워드 중 상관계수 기준으로 상위 20개만 보기
corRefW <- corRefW[order(corRefW, decreasing = T)]
corRefW[1:21]


# xlsx 파일로 저장하기
library(xlsx)
write.xlsx(corRefW,
           paste0(odir, "/", docName, "_Related Keywords by TFIDF.xlsx"))


# Rdata로 저장하기
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
