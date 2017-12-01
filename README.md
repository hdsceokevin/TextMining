# Text Mining R Code

- 데이터 전처리부터 다양한 시각화 및 토픽 클러스터링(LDA)까지 코멘트가 되어있는 R 코드 입니다.
- 웹데이터 크롤링은 [Crawler Repo](http://github.com/DrKevin22/Crawler)에 올린 "다양한 블로그 수집하기"를 참조하시기 바랍니다. 
- 현재 웹크롤러는 파이썬 코드로 작성되어 있으며, 곧 R 코드로도 작성하여 공유할 예정입니다. 
- 시각화 방법으로는 일자별 블로그 게시글 수 막대그래프, 단어 빈출수로 작성한 워드클라우드, 트리맵 등이 있습니다. 
- 토픽 클러스터링에서는 분석가가 원하는 군집의 개수(k)만큼 전체 문서를 분류해주고 각 군집의 특징을 확인할 수 있도록 시각화 도구도 제공됩니다.

## Getting Started

시작하는 방법

- 아래 파일들을 순서대로 실행하시면 됩니다. (Step1 ~ Step8)
- stopwords.txt는 "불용어 사전"으로 코퍼스를 생성할 때 사용합니다. (step5)
- createNamJson_v2.R은 토픽 클러스터링 시각화에 사용됩니다. (step8 코드에 포함되어 있음)

### Files

```text
.
├── Step1 Text data preparation.R
├── Step2 Explorative Data Analysis.R
├── Step3 Create xlsx file and Parsing.R
├── Step4 Check words frequency.R
├── Step5 Create corpus and DTM.R
├── Step6 Extract related keywords.R
├── Step7 WordCloud and Treemap.R
├── Step8 Topic clustering (LDA).R
├── Step9 Network Map.R
├── createNamJson_v2.R
└── stopwords.txt
```
### Prerequisites

아래와 같은 패키지가 필요로 합니다.

```R
library(readxl)
library(ggplot2)
library(slam)
library(network)
library(sna)
library(tm)
library(xlsx)
library(NLP4kec)
library(dplyr)
library(RColorBrewer)
```

### Installing

파일을 실행시키면서 만나게 될때마다 설치하시면 됩니다.

## Contributing

- 네트워크맵은 추가했습니다.(Step9)
- 현재 공개되어 있는 코드에는 보완해야 할 부분이 많습니다.
- 앞으로 지속적으로 코드를 재구성할 계획입니다. 
- 혹시 오류를 발견하시면 제게 피드백 부탁드립니다.

## Authors

- **Kevin Seongho Na**
- [컨트리뷰터 리스트](https://github.com/DrKevin22/TextMining/graphs/contributors)

## Others

- 위의 R 코드들은 패스트캠퍼스 "비즈니스 활용 사례로 배우는 텍스트마이닝"의 강의 자료를 저의 스타일로 재편성한 것입니다. 
- 전체 공유에 문제가 될 경우 고지해주시면 감사하겠습니다.
