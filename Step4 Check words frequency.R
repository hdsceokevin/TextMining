

#--------------------------------------------------------------------------
# 4. 단어별 빈도수 확인 및 사용자 사전 추가 보완 (필요 시)
#--------------------------------------------------------------------------

# Rdata 확인
list.files(pattern = "Rdata")

# Rdata 불러오기
load("Blog_지름신_201701.Rdata")


# 단어별 빈도수 확인
wordsFreq <- table(words)

# 빈도수 높은 순서대로 정렬
wordsFreq <- wordsFreq[order(wordsFreq, decreasing = T)]

# 상위 100개 확인
wordsFreq[1:100]

# 띄어쓰기, 복합명사, 고유명사 등을 고려하여 변경할 단어가 있으면
# dictionary.txt에 추가한 후 형태소 분석 다시하기!!
# 예를 들어 "아이", "쇼핑"은 "아이쇼핑"으로 묶어야 함 (비록 콩글리쉬지만...)
# 위 단어 벡터 중 "하다", "있다", "것", "되다"와 같이 여러 문장에서
# 다양한 의미로 쓰이는 단어들은 나중에 불용어 사전에 추가해야 함


# 2음절 이상인 단어만 남기기
# str_split() 함수를 사용하면 리스트 형태로 저장됨
parsedList <- str_split(parsed, " ")
parsedList[[1]]

for (i in 1:length(parsedList)) {
  parsedList[[i]] <- Filter(function(x)
    nchar(x) >= 2, parsedList[[i]])
  parsedList[[i]] <- paste(parsedList[[i]], collapse = " ")
}

# 리스트를 벡터로 변환하고 처음 10개만 확인
parsed <- unlist(parsedList)
parsed[1:10]


# txt 파일로 저장하기 for cleansing
write.table(parsed,
            paste0(odir, "/", docName, "_for_cleansing.txt"),
            row.names = F)

# csv로 저장하기 for topic modeling
parsedDf <-
  data.frame(id = paste(docName, 1:length(parsed), sep = "_"),
             parsedContent = parsed)
write.csv(parsedDf,
          paste0(odir, "/", docName, "_for_topic_modeling.csv"),
          row.names = F)

# RDS 파일로 저장하기
saveRDS(parsed, paste0(odir, "/parsed.RDS"))

# RDS 파일 다시 불러올 때 사용
# rm(list=ls())
# parsed <- readRDS("./parsed.RDS")
# length(parsed)


# Rdata로 저장하기
save.image(paste0(docName, ".Rdata"))
list.files(pattern = "Rdata")
