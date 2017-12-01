

#--------------------------------------------------------------------------
# 1. 크롤러로 수집한 텍스트 데이터 정리
#--------------------------------------------------------------------------

# 작업경로 설정
wdir <- "./Input"

# 텍스트 데이터 목록 확인
(fileList <- list.files(path = wdir))

# 작업일자 지정
(today <- gsub("-", "", Sys.Date()))

# 글의 종류와 날짜 지정
type <- "Blog"
query <- "지름신"
month <- "201701"
(docName <- paste(type, query, month, sep = "_"))

# 위에서 지정한 파일만 가지고 오기
(fileList <- grep(docName, fileList, value = T))

# 엑셀파일 불러오기
text <- data.frame()

library(readxl)
for (i in 1:length(fileList)) {
  blog <- read_excel(paste(wdir, fileList[i], sep = "/"))
  blog <- as.data.frame(blog)
  text <- rbind(text, blog)
}

rm(blog)

# 컬럼명 변경하기
# 앞으로 사용하려는 형태소 분석기에 특정된 컬럼명인 "content"으로 변경!
colnames(text)[7] <- "content"

# 컬럼 순서 변경
# text <- text[, c("keyword", "title", "postdate", "link", "bloger", "summary", "content")]

# 텍스트마이닝 데이터 만들기
# 20170101 이후 포스팅만
tmdf <- text[text$postdate >= "2017.01.01",]
tmdf$id <- seq(1, nrow(tmdf))

# 링크로 중복 제거
library(dplyr)
tmdf <- distinct(tmdf, link, .keep_all = T)

# 필요한 컬럼만 남기기
tmdf <- tmdf[, c("id", "keyword", "postdate", "bloger", "content")]

# content 컬럼이 NA인 건 제거
tmdf <- tmdf[is.na(tmdf$content) == F, ]


# Rdata로 저장하기
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
