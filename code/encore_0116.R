library(tidyverse)

## 중간 데이터 불러오기
load('data/bank_customer_preprocessed.RData')
## RData로 저장하는 법 익히자.



## @ 2-2. 현황분석 : 군집분석 요약

# 거리 계산(Gower 척도)
# 변수 타입 설정 및 거리 계산 변수 선택
# daisy 함수를 이용하기 위한 단계
tmp <- df %>% 
  mutate(age=as.numeric(age)) %>% 
  mutate(balance=as.numeric(balance)) %>% 
  mutate(education=as.ordered(education)) %>%    
  # as.ordered : factor와 비슷하지만 순서를 인정해주는 의미
  select(-cid, -balance_balanced)

## 거리 계산
library(cluster)
d <- daisy(tmp, metric='gower')


## 계층적 군집 분석
# 군집 모델 생성
cl <- hclust(d)

# 군집 모델 시각화(덴드로그램)
plot(cl)

# 군집 개수 선택
cluster <- cutree(cl, 4)

# 군집별 고객 수 확인
table(cluster)


## 시각화용 데이터 생성
# 원본 데이터 불러오기
raw <- read.csv('data/bank_campaign.csv')
View(raw)

# 데이터 병합(장기예금 구매여부 변수만 추가)
df.p <- left_join(df, raw %>% 
                    select(cid, y))

# 시각화에 사용하지 않을 변수 제거
df.p <- df.p %>% 
  select(-cid)

# 데이터 확인하기
str(df.p)


## 변수 특성 파악 : 범주형
# 군집별 주택융자(housing) 비율 바 차트
ggplot(df.p, aes(x=cluster, fill=housing)) +
  geom_bar(position='fill') +
  theme_bw()

# 군집별 장기예금 구매여부(y) 비율 바 차트
ggplot(df.p, aes(x=cluster, fill=y)) +
  geom_bar(position='fill') +
  theme_bw()

# 범주형 변수를 시각화하여 각 군집별 범주형 변수의 분포를 파악한 결과,
# 군집 3의 장기 예금 구매 비율이 높게 나타났으며, housing, default 변수 등이
# 각 군집별로 다른 분포를 나타냈다.

## 변수 특성 파악 : 숫자형
# 군집별 나이(age) 박스 플랏
ggplot(df.p, aes(x=cluster, y=age, group=cluster)) +
  geom_boxplot() +
  theme_bw()

# 군집별 자산(balance) 박스 플랏
ggplot(df.p, aes(x=cluster, y=balance_balanced, group=cluster)) +
  geom_boxplot() +
  theme_bw()

# 숫자형 변수를 시각화하여 각 군집별 분포를 파악한 결과,
# 군집 3에는 고령의 고객이 다수 포함되어 있으며, 군집 1, 2, 3에 비해 군집 4의
# 자산이 낮은 것으로 나타났다.

# 탐색 결과를 바탕으로 장기 예금 구매자의 비율이 높은 군집 3을 DM 진행 군집
# 으로 선정했으며, 이 군집은 자산이 높을 가능성이 있고 돈을 지출할 곳이
# 적다는 특성을 나타낸다.

## 군집 3 선택
df <- df %>% 
  mutate(cluster=cluster) %>% 
  mutate(cluster_DM=ifelse(cluster==3, 'yes', 'no'))

View(head(df))

## 군집 결과를 포함한 중간 데이터 저장
save(df, file='data/bank_customer_clustered.RData')