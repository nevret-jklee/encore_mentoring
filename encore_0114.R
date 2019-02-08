################################
### 엔코아 멘토링 2019-01-14 ###
################################

#############
# 1. R 기초 #
#############

## 기본 벡터
x <- 45.3

## c() function을 이용한 vector 생성
x <- c(0.5, 0.6)                      # combine numeric values into a vector
x <- c(TRUE, FALSE)                   # logical
x <- c("a", "b", "c")                 # character

## vector() function을 이용한 vector 생성
x <- vector("numeric", length = 10)
x

## vector 각 요소 이름 부여
x <- c(1, 3, 4)
names(x) <- c("kim", "seo", "park")
x

#####################################################
# Data Type
# - Numeric
# - character
# - Logical (True / False)
# - Factor (Categorical, 범주형)
# - NA (Not Available)
# - NAN (Not A Number)
# - NULL (초기화 되지 않은 값)
# 
# Data Structure
# - Vector
# - Matrix
# - Array
# - List
# - Data frame
# 
# Functions
# - 특정 연산을 수행하기 위해 만들어진 특별한 objects
#####################################################

# Factor
sex <- factor(x="m", levels = c("m", "f"))
sex

sex <- factor(x=c("m", "m", "f"), levels = c("m", "f"))
sex

levels(sex)

# NA, NAN, NULL
a <- NA
is.na(a)

a <- 0/0
is.nan(a)

a <- NULL
is.null(a)

x <- c(45, 243, 78, 343)
length(x)

x[2]
x[-3]

names(x) <- c("kim", "seo", "park")     # names 함수는 변수명 설정 시 사용.
x
x["seo"]

x <- c("a", "b", "c", "d", "a")
x[1:4]
x[x > "a"]
u <- x > "a"
u
x[u]

y <- c(1, 4)
x[y]
x[-c(4, 6)]
x[-c(1:3)]

# 변환하고 싶은 형태에 대해 as. 함수를 이용

x <- 0:6
x

as.numeric(x)
as.logical(x)
as.character(x)

x <- c("m", "f")
as.factor(x)
as.numeric(as.factor(x))

## 강제변환이 불가능한 경우 NA(Not Available)를 반환함

x <- c("a", "b", "c")

as.numeric(x)
as.logical(x)


## 함수 생성***
## 덧셈 함수 생성
add <- function(x, y){
  return(x+y)
}

add(1, 5)

## 부호를 고려한 세 제곱근 함수 생성
sqrt3 <- function(x){
  dir <- ifelse(x > 0, 1, ifelse(x < 0, -1, 0))
  x <- abs(x)^(1/3)
  x <- x*dir
  return(x)
}

sqrt3(-8)

## Data frame 생성
my.dataset <- data.frame(site = c("A", "B", "A", "A", "B"),
                         season = c("Winter", "Summer", "Summer", "Spring", "Fall"),
                         pH = c(7.4, 6.3, 8.6, 7.2, 8.9))

my.dataset[3,2]
my.dataset$pH
my.dataset[my.dataset$pH > 7, ]
my.dataset[my.dataset$season == "Summer", c("site", "pH")]

## Data frame을 다루는 유용한 기능
attach(my.dataset)
my.dataset[site == "B", ]
detach(my.dataset)

subset(my.dataset, pH > 8)
subset(my.dataset, season == "Summer", season:pH)
my.dataset[my.dataset$season == "Summer", "pH"] <- my.dataset[my.dataset$season == "Summer", "pH"] + 1

## add new variable to data frame
my.dataset$NO3 <- c(234.5, 256.6, 654.1, 356.7, 776.4)
my.dataset

## Data frame 생성 : rbind, cbind
x <- 1:3
y <- 10:12
cbind(x, y)
rbind(x, y)

m1 <- matrix(c(45, 23, 66, 77, 33, 44, 56, 12, 78, 23), 2, 5)
cbind(c(4, 76, m1[, 4]))
m2 <- matrix(rep(10, 20), 4, 5)     # 10을 20번 반복
m3 <- rbind(m1[1, ], m2[3, ])



####################
# 2. R 주요 패키지 #
####################

# < tidyverse >
# 1. dplyr
# 2. tidyr
# 3. ggplot2

library(tidyverse)

data <- read.csv('https://bit.ly/2TLXbmw')
View(data)
str(data)

## 1) dplyr : 데이터 전처리 기능 제공

### @ select : 입력한 변수의 순서대로 변수를 포함하는 데이터가 생성됨.
### 변수를 재배열할 때도 간혹 사용되곤 함.

## (1) 변수 선택
newdata <- data %>% 
  # data라는 data.frame 사용
  select(balance, age, y)
  # data에서 balance, age, y라는 변수를 선택

## (2) 변수 제거
newdata <- data %>% 
  select(-balance, -age, -y)

## (3) 특정 문자열로 시작하는 변수 선택
newdata <- data %>% 
  select(starts_with('p'))

# < 유사한 다른 기능 >
# 특정 문자로 끝나는 변수 선택
# select(ends_with("특정 문자열"))
newdata <- data %>% 
  select(ends_with('e'))

# 특정 문자를 포함하는 변수 선택
# select(contains("특정 문자열"))
newdata <- data %>% 
  select(contains('out'))

str(newdata)


## (4) 모든 변수 선택
## 이를 응용하여 관심 있는 변수 y를 맨 앞으로 가져오기
## 이런 경우(순서 변경)를 제외하고는 딱히..
newdata <- data %>% 
  select(y, everything())

## (5) 변수 선택하고 이름 바꾸기
## rename()은 전체 변수에 대해 바꿀때
## select(바꾸고 싶은 변수 명 = 원래 변수 명)
newdata <- data %>% 
  select(BALANCE=balance, AGE=age, CLASS=y)


### @ mutate : 새로운 변수를 생성하거나, 기존 변수를 변형하는 기능

## (1) 새로운 변수 생성하기
## balance를 age로 나눈 balanceperage변수 생성하기
newdata <- data %>% 
  mutate(balanceperage = balance/age)

## (2) 기존 변수 변형하기
## balance 변수 제곱하기
newdata <- data %>% 
  mutate(balance = balance^2)


### @ filter : 주어진 조건에 따라 데이터를 필터링하는 기능

## (1) 데이터 필터링 하기
## balance가 0보다 큰 변수만 추출하기
newdata <- data %>% 
  filter(balance > 0)

## filter에서 자주 쓰는 기능**

# 데이터 전체 기준(data)
# * 결측되지 않은 행(결측치가 있는 행 제거) : complete.cases(data) 
# * 중복된 행 : duplicated(data)

# 변수 기준(variable)
# * 결측되지 않은 값 : complete.cases(variable)
# * 중복된 값 : duplicated(variable)
# * 특정 문자열(들)을 포함 : variable %in% c('특정 문자열1', '특정 문자열2', ...)

# 중복된 데이터 제거하기
# 조건 앞에 !를 붙이면 원래 조건의 반대 데이터들이 추출된다.
newdata <- data %>% 
  filter(!duplicated(data))


### @ group_by() & summarise()
### 특정 변수에 포함된 값을 기준으로 데이터를 그룹화하며, 그룹별 요약정보를 생성할 수 있다.

## (1) 그룹 설정하기
## month 변수로 그룹 설정하고 balance 합계 구하기
newdata <- data %>% 
  group_by(month) %>% 
  summarise(sum_balance=sum(balance))

## month, job 변수로 그룹 설정하고 balance 합계 구하기
newdata <- data %>% 
  group_by(month, job) %>% 
  summarise(sum_balance = sum(balance))

## (2) 여러 변수 한번에 요약하기
newdata <- data %>% 
  select(month, balance, duration) %>% 
  group_by(month) %>% 
  summarise_all(funs(sum, mean))


## 2) tidyr : 데이터의 형태를 변형시키는 여러 함수를 제공.

## (1) gather : 여러 개의 변수를 하나의 변수로 쌓는다.
##     gather(데이터, key='키 컬럼 이름', value='값 컬럼', 쌓을 변수1, ...)
## month, age, balance, duration 변수를 선택하고 맨 앞 두 줄만 선택한 후, month 변수는 그대로 두고 age, balance, duration 변수 쌓기
newdata <- data[1:2, ] %>% 
  select(month, age, balance, duration) %>% 
  gather(., key = 'variable', value = 'value', age, balance, duration)


## 3) ggplot2 : 강력한 시각화 도구


# geom_point        산포도
# geom_line         선 그래프
# geom_bar          막대 그래프
# geom_histogram    히스토그램
# geom_boxplot      상자 그림


### (1) 기본 그래프 그리기(산포도) : geom_point()
p <- ggplot(data=data, aes(x=balance, y=duration)) +     # 데이터, 축 정의
  geom_point()                                           # 그래프 종류 정의

### (2) 기본 그래프 그리기(히스토그램) : geom_histogram(bins=막대 개수)
p <- ggplot(data=data, aes(x=balance)) + 
  geom_histogram(bins=10)

p <- ggplot(data=data, aes(x=balance)) + 
  geom_histogram(bins=1000)

### (3) 기본 그래프 그리기(막대 그래프) : geom_bar()
p <- ggplot(data=data, aes(x=job)) +
  geom_bar()

### (4) 기본 그래프 그리기(상자 그림) : geom_boxplot()
p <- ggplot(data=data, aes(x=job, y=age)) +
  geom_boxplot()


# @ 히스토그램은 숫자형 변수를 일정 구간으로 나누어 각 구간의 개수를 보여줌
# @ 막대 그래프는 범주형 변수의 각 범주의 개수를 보여준다.
# @ 상자 그림을 하나만 보고 싶다면 x=1 or x=이름 등 아무거나 작성


### (5) 그래프 색 추가하기
### ex. balance와 duration의 산포도를 y(정기예금가입여부)로 색을 달리 보기
p <- ggplot(data=data, aes(x=balance, y=duration, color=y)) + 
  geom_point()

### (6) 그래프 색 추가하기2
### ex. job의 바 차트를 y(정기예금가입여부)에 따라 색을 달리해서 보기
###     position='fill' 일 때와 아닐 때를 비교
p <- ggplot(data=data, aes(x=job, fill=y)) + 
  geom_bar()

p <- ggplot(data=data, aes(x=job, fill=y)) + 
  geom_bar(position='fill')

### (7) 그래프 창 나누기 
### : facet_wrap(나누는 기준으로 사용할 변수 ~ 나누는 기준으로 사용할 변수)

### ex. y(정기예금가입여부)를 기준으로 나누어 두 개의 marital 바 그래프 한번에 그리기
p <- ggplot(data=data, aes(x=marital))
p + geom_bar() + facet_wrap(~y)

### (8) 그래프 다듬기
p <- ggplot(data=data, aes(x=job, fill=y))
p + geom_bar(position='fill') +
  labs(title="직업별 정기예금 가입자 비율 바 그래프",
       x="직업",
       y="가입자 비율") +
  scale_fill_manual(values=c('#B2DF8A', '#33A02C')) +
  theme(title=element_text(size=24),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20),
        axis.text.x=element_text(size=14, angle=45, hjust=1),
        axis.text.y=element_text(size=14))
# labs() : 그래프에 들어갈 텍스트를 입력
# scale_fill_manual() : fill로 정의된 값의 색을 조정
# theme() : 제목, 각 축의 제목의 크기, 각도 등의 값을 조정

### (9) 응용 그래프 그리기(여러 변수의 그래프를 한번에)
data %>% 
  select(marital, education, y) %>% 
  gather(., key='variable', value='value', marital, education) %>% 
  ggplot(., aes(x=y, fill=value)) +
  geom_bar(position='fill') + 
  facet_wrap(~variable) +
  theme_bw()    # 흰색 바탕에 검은색, 추천하는 theme

### (10) 실습
View(data)
str(data)  

data %>% 
  select(default, education, housing, job, loan, marital, y) %>% 
  gather(., key='variable', value='value', default, education, housing, job, loan, marital) %>% 
  ggplot(., aes(x=y, fill=value)) + 
  geom_bar(position='fill') +
  facet_wrap(~variable) +
  theme_bw()

