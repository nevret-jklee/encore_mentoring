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




  ### 예측 분석 : 전처리 & 모델링  ###
 
  ### 1. 파생변수 생성
  ### 2. 변수 선택

load('data/bank-clustered_balanced_disc_train.RData')

## @ 파생변수 생성

## 나이 분포 자세히 보기
## -> 28세, 60세에서 각각 장기 예금 가입 비율이 변하는 것을 확인.
ggplot(df, aes(x=age, fill=y, alpha=0.5)) +
  geom_density() +
  theme_bw()

## 이를 근거로 [0, 28), [28, 60), [60, 100]을 기준으로 이산화하는 변수 생성.

## age 변수 이산화.
df <- df %>% 
  mutate(age_disced = cut(age, c(0,28,60,100)))

## age_disced 범주 이름 변경
levels(df$age_disced) <- 
  paste(c('01', '02', '03'), levels(df$age_disced), sep="")

## age_disced 변수 확인
# 개수
ggplot(df, aes(x=age_disced, fill=y)) +
  geom_bar() +
  theme_bw()

# 비율
ggplot(df, aes(x=age_disced, fill=y)) +
  geom_bar(position='fill') +
  theme_bw()

## pdays 변수는 그대로 사용이 불가하므로, 원래 값의 역수를 계산하여 새로운 파생 변수를 생성한다.
## -1(참여 안함) 값을 무한대로 변경한 후 1/pdays 변수로 변경.
df <- df %>% 
  mutate(pdays_inversed = ifelse(pdays==-1, Inf, pdays)) %>% 
  mutate(pdays_inversed = (pdays_inversed^(-1)))

## @ 변수 선택
## 타겟변수 y와 각 예측 변수 간의 정보 획득량을 계산한 후 해당 값을 기준으로 변수를 선택한다.

# filter 방법을 이용한 변수 선택
library(FSelector)

w <- information.gain(y~., df %>% 
                        select(-cid))

# 결과 확인
as.data.frame(w) %>% 
  mutate(name=row.names(w)) %>% 
  arrange(desc(attr_importance)) %>% 
  View()

# 성능이 떨어지는 (원본과 가공 비교) 변수 제거
# 성능이 같은 경우, 가공된 변수를 제거
df <- df %>% 
  select(-balance_balanced, -duration_balanced, -campaign, -campaign_balanced, -previous_disced, -previous_balanced, -age_disced, -pdays, -pdays_disced)



   ### 예측 분석 : 예측 모델 생성 요약  ###

   ### 1. 모델 생성/평가
   ### 2. 모델 해석


## @ 모델 생성/평가
## 데이터를 학습시키기 전, 모델의 해석을 위해 정규화 과정을 진행한다.

# 학습 전 전처리 : 정규화
preSet <- preProcess(df, method = c("range"))
df.prep <- predict(preSet, df)


## 모델 생성/평가 : 일반 모델
## 모델의 1차 검증은 Cross-Validation을 통해 진행, 이를 위한 설정 값을 정함.

# 학습 / Cross validation 설정

set.seed(0)
ctrl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE,
                     summaryFunction = twoClassSummary)


# 로지스틱 회귀 분석을 통해 예측 모델을 생성한다.
fit <- train(y~.,
             data = df.prep %>% select(-cid), 
             method = "glm", 
             family = binomial,
             trControl = ctrl)

# 성능 확인
fit

   ## 학습된 로지스틱 회귀 모델의 성능을 확인한 결과,
   ## ROC는 0.86으로 준수한 편이지만 구입할 사람을 제대로 예측할 확률은
   ## 0.37로 낮게 나타났다.


## @ 모델 생성/평가 : 언더샘플링 후 학습한 모델

## 언더샘플링?
## : 타겟 변수가 두 개의 범주를 가지며, 두 범주의 수가 불균형한 경우 사용하며
##   개수가 많은 범주를 수가 적은 범주의 개수만큼 샘플링한다.

# 학습 / Cross validation 설정
set.seed(0)
ctrl.ud <- trainControl(method = "repeatedcv",
                        repeats = 5,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        sampling = "down")

# 로지스틱 회귀 모델을 이용한 학습
fit.ud <- train(y~.,
                data = df.prep %>% select(-cid),
                method = "glm",
                family = binomial,
                trControl = ctrl.ud)

# 성능 확인
fit.ud


   ## 언더샘플링된 데이터에서 학습된 로지스틱 회귀 모델의 성능을 확인한 결과,
   ## ROC는 0.86, 구입할 사람을 제대로 예측할 확률도 0.75. 준수함.


## @ 모델 저장/해석
## P-value 0.01을 기준으로 변수의 유의미함을 판단한 결과,
## 7개의 변수가 유의미하다고 판단되었고, 그 중 duration 변수는 그 영향력이
## 다른 변수들에 비해 매우 크게 나타났다.


## 모델 및 데이터셋 저장 ##
# 데이터
save(df, file = 'data/bank-clustered_balanced_disc_final_train.RData')

# 모델
save(preSet, file = 'data/preprocess_setting.RData')
save(fit.ud, file = 'data/fitted_model.RData')


## 모델 해석 
summary(fit.ud)


######################################


    ### 예측 분석 : 적용 요약  ###
    
    ### 1. 모델 테스트
    ### 2. 고객 스코어링


## @ 모델 테스트

# 중간 데이터 / 모델 불러오기
# 학습 / 테스트 데이터
load('data/bank-clustered_balanced_disc_final_train.RData')
load('data/bank-clustered_balanced_disc_test.RData')

# 전처리 세팅 / 모델
load('data/preprocess_setting.RData')
load('data/fitted_model.RData')


## 학습 데이터에서 모델을 학습할 때 사용한 최종데이터를 생성하는 프로세스를
## 테스트 데이터에 적용하여 모델의 성능을 테스트한다.

# 테스트셋 전처리
df.test <- df.test %>% 
  select(-month, -day) %>% 
  mutate(campaign_disced = 
           as.factor(findInterval(df.test$campaign,                                       c(1,4,max(df.test$campaign)), left.open = TRUE))) %>%
  mutate(pdays_inversed = ifelse(pdays==-1, Inf, pdays)) %>% 
  mutate(pdays_inversed = (pdays_inversed^(-1))) %>% 
  mutate(y = factor(y, levels = c('yes', 'no'))) %>% 
  select(-pdays, -campaign)

levels(df.test$campaign_disced) <- c('01[1,1]', '02(1,4]', '03(4,max]')


## 학습 데이터에서 정의된 정규화 기준으로 테스트 데이터를 정규화한다.

# 학습 전 전처리
# df.train <- df[train.id, ]
df.train.prep <- predict(preSet, df.train)
df.test.prep <- predict(preSet, df.test)
df.train.prep <- df.train.prep %>% 
  filter(complete.cases(df.train.prep))
str(df.train.prep)
str(df.test.prep)
## 모델 테스트 결과는 정확도 80%, 실제 구매자를 구매자로 예측할 확률 75%로 확인된다.

# 테스트셋 성능 평가
pred <- predict(fit.ud, df.test.prep, type = "prob", na.action = na.pass)

pred <- pred %>%
  mutate(yes_cm = ifelse(yes > 0.5, 'yes', 'no'))
str(pred$yes_cm)
str(df.test.prep$y)
pred$yes_cm <- as.factor(pred$yes_cm)
str(pred$yes_cm)

confusionMatrix(pred$yes_cm, df.test.prep$y)


## @ 고객 스코어링
##  학습된 모델을 적용하여 전체 데이터에 포함된 고객에 대한 스코어링을 진행한다.

# 데이터 순서 맞추기
df.train <- df.train %>% 
  select(order(colnames(.)))

df.test <- df.test %>% 
  select(order(colnames(.)))

# 통합 데이터 생성
df.prep <- rbind(df.train.prep, df.test.prep)
df.prep <- df.prep %>% 
  filter(complete.cases(df.prep))
prep <- predict(fit.ud, df.prep, type = "prob")
df <- rbind(df.train, df.test)
View(df)
df <- df %>% 
  filter(complete.cases(df))
str(df)
str(pred$yes)
df <- df %>% 
  mutate(score = pred$yes_cm) %>% 
  mutate(predicted = ifelse(score>0.5, 'yes', 'no'))

# 스코어링 결과 확인
df %>% View()


















































