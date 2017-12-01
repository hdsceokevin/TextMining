

#--------------------------------------------------------------------------
# 5. 말뭉치(corpus) 생성
#--------------------------------------------------------------------------

# Rdata 확인
list.files(pattern = "Rdata")

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")


# 말뭉치를 생성하기 전에 텍스트를 벡터 소스로 변경
# 벡터 소스는 벡터의 개별 원소를 각각의 문서로 인식
library(tm)
(corpus <- VCorpus(VectorSource(parsed)))

# 결과 객체(corpus)는 content와 meta를 요소로 갖는 리스트
# content에는 한글 텍스트, meta에는 데이터 속성이 할당됨
# 첫 번째 결과 확인
# str(corpus[[1]])
corpus[[1]]$content
corpus[[1]]$meta

# 불용어(stopwords) 삭제
# 한글 불용어는 국민대 강승식 교수 자료 + 블로그에서 발견된 단어로 작성한 것임
# 도메인별로 불용어가 다를 수 있으므로 주의를 기울여야 함!!
# 강승식 교수 자료는 "http://jjeong.tistory.com/739" 참고
stopwords <- read.table("stopwords.txt")$V1
corpus <- tm_map(corpus, removeWords, stopwords)
class(corpus[[1]])

# 소문자로 변경 (영어가 포함되었을 경우. 대문자는 toupper)
# corpus <- tm_map(corpus, tolower)
# class(corpus[[1]])

# 특수문자 제거
corpus <- tm_map(corpus, removePunctuation)
class(corpus[[1]])

# 숫자 삭제
corpus <- tm_map(corpus, removeNumbers)
class(corpus[[1]])

# whitespace 제거
corpus <- tm_map(corpus, stripWhitespace)
class(corpus[[1]])

# 동의어 처리
for (j in seq(corpus)) {
  corpus[[j]] <- gsub("강립", "강림", corpus[[j]])
}

# gsub() 함수를 이용하여 텍스트 변환시 PlainTextDocument가 character로 바뀜!!
# 이 경우 DTM을 생성할 수 없으므로 나중에 PlainTextDocument로 변환해주어야 함
class(corpus[[1]])

# 말뭉치 작업 결과 눈으로 대충 확인
for (i in 1:10) {
  print(corpus[[i]])
}

# PlainTextDocument 형식으로 변환
corpus <- tm_map(corpus, PlainTextDocument)
class(corpus[[1]])


# Document Term Matrix 생성 (Term frequency)
# 단어 Length는 2로 세팅
# 생성된 dtm을 출력해보면 여러 정보를 확인할 수 있음
# 이 DTM은 4555개 문서, 38658개 단어로 구성된 행렬 (38658 차원임!)
# sparsity는 전체 행렬 중 0이 차지하는 비중을 의미
# weighting은 행렬을 구성하는 각각의 값(value)을 계산한 방식을 나타냄
(dtm <-
    DocumentTermMatrix(corpus, control = list(wordLengths = c(2, Inf))))

# 단어(=컬럼명) 양옆 스페이스 제거
(colnames(dtm) <- trimws(colnames(dtm)))

# 문서 이름(row name) 지정
dtm$dimnames$Docs <- paste(docName, 1:length(parsed), sep = "_")
dtm$dimnames$Docs[1:10]
dtm$dimnames$Terms[1:10]


# [참고] Term Document Matirx 생성 (DTM에서 행과 열만 바뀐 matrix)
# (tdm <- TermDocumentMatrix(corp, control=list(removeNumbers=T, wordLengths=c(2,Inf))))

# 작업의 편의를 위해 대부분이 0인 sparse Terms 일부 삭제
# 설정하는 값의 크기가 작을수록 term의 개수 감소
# 각각의 열(단어) 기준으로 sparsity가 0.99를 넘는 열을 삭제
# 그 결과 문서의 개수는 4555개로 변함 없으나, 단어의 개수는 2389로 크게 감소
(dtm <- removeSparseTerms(dtm, as.numeric(0.99)))

# DTM을 데이터 프레임 형식으로 저장
# 나중에 토픽 클러스터링 또는 분류모형을 위해 미리 작업!!
dtmDf <- as.data.frame(as.matrix(dtm))
dim(dtmDf)
head(dtmDf)


# Rdata로 저장하기
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
