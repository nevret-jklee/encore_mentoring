library(dplyr)
library(ggplot2)
library(GGally)

## @ 3. 데이터 병합

## 데이터 불러오기
raw <- read.csv('data/bank_campaign.csv')

## 중간 데이터 불러오기
load('data/bank_customer_clustered.RData')

## 고객+캠페인 정보 데이터 중 선택된 군집(3) 고객만 추출하여 병합
df <- df %>% 
  filter(cluster_DM=='yes') %>% 
  select(-balance_balanced, -cluster, -cluster_DM) %>% 
  left_join(., raw)


## @ 3-1. 데이터 살펴보기

## 데이터 눈으로 확인
View(df)

## 변수 타입 확인
str(df)

## 요약 정보 확인
summary(df)


## @ 3-2. 데이터 클렌징

## 변수 제거
df <- df %>% 
  select(-default, -loan)

## 변수 타입 변경
df$day <- as.factor(df$day)

## factor 순서 및 값 변환
df$month <- factor(df$month, levels=c('jan', 'feb', ',ar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'))

levels(df$month) <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

## 데이터 재확인
str(df)

## 빈 값 변형 : poutcome
levels(df$poutcome)
summary(df$poutcome)
levels(df$poutcome)[1] <- 'unknown'
summary(df$poutcome)

## 결측값 제거
## (습관적으로 해주는게 좋음)
df <- df %>% 
  filter(complete.cases(df))

## 결측값 재확인
summary(df)

## 학습 / 테스트셋 분리

set.seed(0)
train.id <- sample(1:nrow(df), size=round(nrow(df)*0.7))
train.id
df.train <- df[train.id, ]
df.test <- df[-train.id, ]

df <- df.train
head(df)

## @ 모델링.

## 1. 전체 변수 시각화
p <- ggpairs(df %>% select(-cid),
             mapping=ggplot2::aes(colour=y, alpha=0.5),
             cardinality_threshold=31) +
  theme(axis.text.x = element_text(angle=270, hjust=0)) +
  theme_bw()

ggsave(plot=p, file='plot/군집후_산포_행렬.png',
       width=80, height=80, unit='cm')

## 2. 변수 대칭화

##   1) balance 변수

## 세제곱근 계산 함수 생성
sqrt3 <- function(x){
  dir <- ifelse(x > 0, 1, 
                ifelse(x < 0, -1, 0))
  x <- abs(x)^(1/3)
  x <- x*dir
  return(x)
}

## balance 변환
# library(ggplot2)
df <- df %>% 
  mutate(balance_balanced = sqrt3(balance))

## balance_balanced 분포 확인
## : 확인 결과, 그래프가 균형을 이룸.
ggplot(df, aes(balance_balanced)) +
  geom_histogram(bins = 30) +
  theme_bw()


##   2) duration 변수

## duration 변환
df <- df %>% 
  mutate(duration_balanced = sqrt(sqrt(duration)))

## duration_balanced 분포 확인
## : 확인 결과, 그래프가 균형을 이룸.
df %>% 
  ggplot(., aes(duration_balanced)) +
  geom_histogram(bins = 30) +
  theme_bw()


##   3) campaign 변수

## campaign 변환
df <- df %>% 
  mutate(campaign_balanced = sqrt(campaign))

## campaign_balanced 분포 확인
## : 확인 결과, 변환 후에도 균형을 이루지 않음.
df %>% 
  ggplot(., aes(campaign_balanced)) +
  geom_histogram(bins = 30) + 
  theme_bw()


##   4) previous 변수

## previous 변환
df <- df %>% 
  mutate(previous_balanced = sqrt(previous))


## previous_balanced 분포 확인
## : 확인 결과, 변환 후에도 균형을 이루지 않음.
df %>% 
  ggplot(., aes(previous_balanced)) +
  geom_histogram(bins = 30) +
  theme_bw()


## 3. 변수 이산화

##   1) pdays 변수
##      : pdays 변수를 -1(참여하지 않았음)은 따로 처리한 후 지도 이산화 방법으로 이산화한 결과 [-1, -1], (-1, 213], (213, 370], (370, max] 구간의 네 개의 구간으로 이산화 되었다.

# install.packages("smbinning")
library(smbinning)

df <- df %>% 
  mutate(y_int = ifelse(y == 'yes', 1, 0))
head(df)

## pdays 변수 이산화
pdays_dt <- smbinning(df = df %>% 
                        filter(pdays != (-1)), y='y_int', x='pdays')
pdays_dt$ivtable

## 이산화된 pdays 변수 추가 
df <- df %>% 
  mutate(pdays_disced = as.factor(findInterval(df$pdays, c(-1, pdays_dt$bands[-1]), left.open=TRUE)))

str(df)
## 범주 입력 및 생성된 변수 확인
levels(df$pdays_disced) <- c('01[-1, -1]', '02(-1, 213]', '03(213, 298]', '04(298, 369]', '05(369, max]')
summary(df$pdays_disced)


##   2) campaign 변수
##      : campaign 변수를 지도 이산화 방법으로 이산화한 결과, [1,1], (1,4], (4, max] 세 개의 구간으로 이산화되었다.

## campaign 변수 이산화
campaign_dt <- smbinning(df, y = "y_int", x = "campaign")
campaign_dt$ivtable

## 이산화된 campaign 변수 추가
df <- df %>% 
  mutate(campaign_disced = 
           as.factor(findInterval(df$campaign,
                                  campaign_dt$bands[-1], left.open = TRUE)))

## 범주 입력 및 생성된 변수 확인
levels(df$campaign_disced) <- c('01[1,1]', '02(1,4]', '03(4,max]')
summary(df$campaign_disced)


##   3) previous 변수
##      : previous 변수를 지도 이산화 방법으로 이산화한 결과 [0,0], (0, max]의 의미상으로는 이전 캠페인 참여 여부를 나타내는 두 개의 구간으로 이산화 되었다.

## previous 변수 이산화
previous_dt <- smbinning(df=df, y="y_int", x="previous")
previous_dt$ivtable

## 이산화된 previous 변수 추가
df <- df %>% 
  mutate(previous_disced = ifelse(previous==0, '01[0,0]', '02(0,max]'))

## previous_disced 를 factor로 변경
df$previous_disced <- factor(df$previous_disced, levels=c('01[0,0]', '02(0,max]'))

## 생성된 변수 확인
summary(df$previous_disced)



## 이산화 변수 확인.
summary(df %>% 
          select(ends_with('disced')))

## y_int 제거
df <- df %>% 
  select(-y_int)


## @ 전체 변수 시각화 : 변수 대칭화/이산화 후
## 산포 행렬
p <- df %>% 
  select(-cid) %>% 
  ggpairs(., mapping=ggplot2::aes(colour=y, alpha=0.5), cardinality_threshold = 31) +
  theme(axis.text.x=element_text(angle=270, hjust=0)) +
  theme_bw()

ggsave(plot = p, file = 'plot/군집후_산포_행렬_전처리후.png', 
       width=80, height=80, unit='cm')


## @ y와의 관계 시각화
## duration, previous, poutcome, pdays 변수를 각 값 별 y의 비율을 나타내도록 시각화 결과, y값에 따라 차이가 나타나는 것을 확인할 수 있다.

# duration
ggplot(df, aes(x=duration_balanced, fill=y, alpha=0.5)) +
  geom_density(position='fill')

# previous
ggplot(df, aes(x=previous_disced, fill=y)) +
  geom_bar(position='fill')

# poutcome
ggplot(df, aes(x=poutcome, fill=y)) +
  geom_bar(position='fill')

# pdays
ggplot(df, aes(x=pdays_disced, fill=y)) +
  geom_bar(position='fill')


## month와 day 변수의 경우 값에 따라 y의 비율 차이를 나타냈지만,
## 이 차이는 각 값별 빈도수와 상관성을 보인다.

# month
ggplot(df, aes(x=month, fill=y)) +
  geom_bar(position='fill')

# day
ggplot(df, aes(x=day, fill=y)) +
  geom_bar(position='fill')

## month와 day 다시 확인
# month
ggplot(df, aes(x=month, fill=y)) +
  geom_bar()

# day
ggplot(df, aes(x=day, fill=y)) +
  geom_bar()


## 따라서 month와 day는 y와 연관성을 가지지만 월/일별 DM 빈도수와도 큰 연관성을 보여 모델의 해석상 문제가 예상되므로 분석에서 제거한다.

# month, day 제거
df <- df %>% 
  select(-month, -day)

## 중간 데이터 저장 ##
save(df, file='data/bank-clustered_balanced_disc_train.RData')

save(df.test, file='data/bank-clustered_balanced_disc_test.RData')
