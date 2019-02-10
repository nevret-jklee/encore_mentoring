install.packages('dplyr')
install.packages('ggplot2')
install.packages('GGally')
install.packages('cluster')
install.packages('smbinning')
install.packages('FSelector')
install.packages('caret')
install.packages('cluster')
install.packages('tidyr')
install.packages('RColorBrewer')
install.packages('ROCR')

## Load library
library(dplyr)
library(ggplot2)
library(GGally)
library(cluster)
library(smbinning)
library(FSelector)
library(caret)
library(cluster)
library(tidyr)
library(RColorBrewer)
library(ROCR)

## @ 1. 비즈니스 이해

####
# 은행에 재정적인 문제가 있어 자산을 늘려야 하는 상황.
# 높은 금리의 장기적 정기 예금 상품에 대한 고객을 모집하기 위한 DM 실시중.
####


## @ 2. 전처리 & 탐색

## 1) 데이터 불러오기
getwd()
df <- read.csv('data/bank_customer.csv')

## 데이터 눈으로 확인
View(df)

## 변수 타입 등 확인
str(df)

## 요약 정보 확인
summary(df)


## 변수 타입 변경
df$education <- as.factor(df$education)

## 빈 값 확인
levels(df$job)
summary(df$job)     # d.f를 넣으면 변수 전체, 벡터 하나만 넣으면 그 벡터만

## 빈 값 변형
levels(df$job)[levels(df$job)==""] <- NA
summary(df$job); str(df)

## 결측값 제거
df <- df %>% 
  filter(complete.cases(df))

## 결측값 재확인
summary(df)
View(df)

## age의 최댓값이 380인 걸 확인
## 그래프 or 눈으로
## 정렬 후 눈으로 살펴보기
df %>% 
  select(age) %>% 
  arrange(desc(age)) %>% 
  View()

## 이상치 제거(380, 270)
df <- df %>% 
  filter(age < 200)


######
## @ 2-1. 현황분석 : EDA 요약

## 1) 전체 변수 시각화
## 꿀팁!**
library(ggplot2)
library(GGally)

# GGally의 ggpairs 함수
# : 모든 변수의 조합에 대한 그래프를 나타내줌.
p <- ggpairs(df %>% select(-cid)) +
  theme(axis.text.x=element_text(angle=270, hjust=0)) +
  theme_bw()

ggsave(plot=p, file='plot/고객_산포_행렬.png', width=80, height=80, unit='cm')


## 2) 변수 대칭화 : balance 변수

## 세제곱근 계산 함수 생성
sqrt3 <- function(x){
  dir <- ifelse(x>0, 1,
                ifelse(x<0, -1, 0))
  x <- abs(x)^(1/3)
  x <- x*dir
  return(x)
}

## balance 변환
df <- df %>% 
  mutate(balance_balanced = sqrt3(balance))

## balance_balanced 분포 확인
df %>% 
  ggplot(., aes(balance_balanced)) +    # .,에서 .의 의미는 앞의 df를 의미
  geom_histogram() +                    # 사실 없어도 무방함.
  theme_bw()

## multiple scatter plot(balanced)
p <- ggpairs(df %>% select(-cid)) +
  theme(axis.text.x=element_text(angle=270, hjust=0)) +
  theme_bw()

ggsave(plot=p, file='plot/고객_산포_행렬_balanced.png', width=80, height=80, unit='cm')

## 중간 결과 저장
save(df, file =
       'data/bank_customer_preprocessed.RData')


