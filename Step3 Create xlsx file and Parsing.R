

#--------------------------------------------------------------------------
# 3. 형태소 분석용 엑셀파일 생성
#--------------------------------------------------------------------------

# Rdata 확인
list.files(pattern = "Rdata")

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")


# 텍스트마이닝에 필요한 컬럼만 남기기
tmdf1 <- tmdf[, c("id", "content")]

# 중복 제거
tmdf1 <- unique(tmdf1)
nrow(tmdf1)

# xlsx 파일명과 파일경로 지정
dir.create("./output")
odir <- "./output"
(fileName <- paste0(docName, "_", "for_parsing", ".xlsx"))
(filePath <- paste(odir, fileName, sep = "/"))

# 형태소 분석할 대상 파일을 xlsx 파일로 저장
# [주의] xlsx는 글자수 32,767 이상일 경우 저장이 되지 않음
# 글자수를 확인한 후 기준을 초과할 경우 csv 또는 text로 저장
# sum(nchar(tmdf1$content))
library(xlsx)
write.xlsx(tmdf1, filePath, row.names = F, sheetName = "Sheet1")
rm(tmdf1)

# Rdata로 저장하기
save.image(paste0(docName, ".Rdata"))


# 형태소 분석 (R 객체로)
# [참고1] NLP4kec 패키지 설치 방법은 "https://github.com/NamyounKim/NLP4kec" 참조!!
# [참고2] 원본 파일을 csv 파일로 저장하려면 text_parser_file() 함수를 이용!

# [주의] xlsx로 저장한 후 바로 형태소 분석이 되지 않음!! RStudio 종료 후 재시동해야 함

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")

library(NLP4kec)
parsed <- text_parser(path = filePath, language = "ko")

# 형태소 분석 완료 후 데이터 확인을 통해 필요 시 사전을 생성
# 사전에는 분리되면 안되는 완성된 단어를 담고 있어야 함 (예시, "가치소비")
# 사전은 "dictionary.txt"로 만들고, text_parser()의 korDicPath 인자에 할당함
# 예시: parsed <- text_parser(path=filePath, language="ko", korDicPath="dictionary.txt")


# 파싱된 데이터 확인
length(parsed)
parsed[1:10]

# 중복 제거 후 데이터 수 확인
length(unique(parsed))

# 문서 단위 중복 제거
parsed <- unique(parsed)


# 불필요한 단어 삭제
# 단어별 빈도수 확인을 위해 단어 단위로 분해
library(stringr)
words <- unlist(str_split(parsed, " "))
length(words)

# 알파벳/숫자, 특수문자, 공백, 한글자음/모음 유무 확인 후 제거
delPatterns <-
  c("[[:alnum:]]", "[[:punct:]]", "[[:space:]]", "[ㄱ-ㅎㅏ-ㅣ]")
for (i in 1:length(delPatterns)) {
  check <- grep(delPatterns[i], words, value = T)
  print(check)
  if (length(check) >= 1)
    words <- gsub(delPatterns[i], "", words)
}


# Rdata로 저장하기
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
