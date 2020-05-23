#####################
#Chapter 9 퀀트 전략을 이용한 종목선정 (기본)
######################

###########
#9.1.1 베타 계산하기
###########


library(quantmod)
library(PerformanceAnalytics)
library(magrittr)

symbols = c('102110.KS', '039490.KS')  # kospi, 키움증권 티커 입력
getSymbols(symbols) #해당 티거의 데이터 다운


prices = do.call(cbind,
                 lapply(symbols, function(x)Cl(get(x)))) ####cbind:열결합, do.call: 단일 데이터프레임으로, 질문

ret = Return.calculate(prices)
ret = ret['2016-01::2018-12']

rm = ret[, 1]  #독립변수는 첫 번째 열인 KOSPI 200 ETF의 수익률
ri = ret[, 2]  #종속변수는 두번째 열인 증권주의 수익률을 선택

reg = lm(ri ~ rm)  
summary(reg)



9.1.2 베타 시각화

plot(as.numeric(rm), as.numeric(ri), pch = 4, cex = 0.3, 
     xlab = "KOSPI 200", ylab = "Individual Stock",
     xlim = c(-0.02, 0.02), ylim = c(-0.02, 0.02))  #pch: 점들의 모양, cex: 점들이ㅡ 크기
abline(a = 0, b = 1, lty = 2)  #a는 상수, b는 직선의 기울기, lty는 선의 유형
abline(reg, col = 'red')



###########
#9.2 저변동성 전략
###########
example = c(85, 76, 73, 80, 72)
sd(example)



######
#9.2.1 저변동성 포트폴리오 구하기: 일간 기준
#######
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(ggplot2)
library(dplyr)

KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' = 
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price) #수익률 계산
std_12m_daily = xts::last(ret, 252) %>% apply(., 2, sd) %>% ##질문#xts::last() 형식을 통해 xts 패키지의 함수임을 정의, apply() 함수를 통해 sd 즉 변동성을 계산
  multiply_by(sqrt(252))  #연율화를 해주기 위해 multiply_by() 함수 이용. 일변동성을 연율화할때는 보통 영업일수 252


std_12m_daily %>% 
  data.frame() %>%
  ggplot(aes(x = (`.`))) +
  geom_histogram(binwidth = 0.01) +
  annotate("rect", xmin = -0.02, xmax = 0.02,
           ymin = 0,
           ymax = sum(std_12m_daily == 0, na.rm = TRUE) * 1.1,
           alpha=0.3, fill="red") +
  xlab(NULL)

std_12m_daily[std_12m_daily == 0] = NA

### 변동성 낮은 종목 30
std_12m_daily[rank(std_12m_daily) <= 30]

#그래프
std_12m_daily[rank(std_12m_daily) <= 30] %>%
  data.frame() %>%
  ggplot(aes(x = rep(1:30), y = `.`)) +
  geom_col() +
  xlab(NULL)


# 변동성 낮은 종목 확인하기

invest_lowvol = rank(std_12m_daily) <= 30
KOR_ticker[invest_lowvol, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`변동성` = round(std_12m_daily[invest_lowvol], 4))



##########
#9.2.2 저변동성 포트폴리오 구하기: 주간 기준
###########

std_12m_weekly = xts::last(ret, 252) %>%
  apply.weekly(Return.cumulative) %>%
  apply(., 2, sd) %>% multiply_by(sqrt(52)) #Return.cumulative를 입력해 주간 수익률을 계산하며, 연율화를 위해 연간 주수에 해당하는 sqrt(52)를 곱해

std_12m_weekly[std_12m_weekly == 0] = NA


std_12m_weekly[rank(std_12m_weekly) <= 30]


invest_lowvol_weekly = rank(std_12m_weekly) <= 30
KOR_ticker[invest_lowvol_weekly, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`변동성` =
           round(std_12m_weekly[invest_lowvol_weekly], 4))



# 일간 저변동성, 주간 저변동성 확인
intersect(KOR_ticker[invest_lowvol, '종목명'],
          KOR_ticker[invest_lowvol_weekly, '종목명'])





###################
##9.3.1 모멘텀 포트폴리오 구하기: 12개월 모멘텀
#################


library(stringr)
library(xts)
library(PerformanceAnalytics)
library(magrittr)
library(dplyr)

KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

ret = Return.calculate(KOR_price) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) {  prod(1+x) - 1}) ####질문


ret_12m[rank(-ret_12m) <= 30]



invest_mom = rank(-ret_12m) <= 30
KOR_ticker[invest_mom, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom], 4))



#########
#9.3.2 모멘텀 포트폴리오 구하기: 위험조정 수익률
###########


ret = Return.calculate(KOR_price) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) {prod(1+x) - 1})   ##분자에 해당하는 누적수익률
std_12m = ret %>% apply(., 2, sd) %>% multiply_by(sqrt(252))  ##분모에 해당하는 연율화 변동성
sharpe_12m = ret_12m / std_12m  #위험조정 수익률



#수익율 높으면서 변동성 낮은 종목
invest_mom_sharpe = rank(-sharpe_12m) <= 30
KOR_ticker[invest_mom_sharpe, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom_sharpe], 2),
         `변동성` = round(std_12m[invest_mom_sharpe], 2),
         `위험조정 수익률` =
           round(sharpe_12m[invest_mom_sharpe], 2)) %>%
  as_tibble() %>% 
  print(n = Inf) ##질문


intersect(KOR_ticker[invest_mom, '종목명'],
          KOR_ticker[invest_mom_sharpe, '종목명'])   #단순 수익률 및 위험조정 수익률 기준 모두에 포함되는 종목



library(xts)
library(tidyr)
library(ggplot2)

KOR_price[, invest_mom_sharpe] %>%
  fortify.zoo() %>%
  gather(ticker, price, -Index) %>%
  ggplot(aes(x = Index, y = price)) +
  geom_line() +
  facet_wrap(. ~ ticker, scales = 'free') + #집단 간 비교 위한 면 분할, fixed와 free 차이?
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())



##########
#9.4.1 밸류 포트폴리오 구하기: 저PBR
 #############
library(stringr)
library(ggplot2)
library(dplyr)

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

invest_pbr = rank(KOR_value$PBR) <= 30
KOR_ticker[invest_pbr, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`PBR` = round(KOR_value[invest_pbr, 'PBR'], 4))


######
#9.4.2 각 지표 결합하기
#####


library(corrplot)

rank_value = KOR_value %>% 
  mutate_all(list(~min_rank(.))) #모든 열에 함수 적용, min_rank()를 통해 순위 구함

cor(rank_value, use = 'complete.obs') %>%  #NA 종목은 삭제해주기 위해 use = 'complete.obs'를 입력
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt=45, tl.col = 'black',
           col = colorRampPalette(
             c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))


  ######
##9.5.1
###############
library(stringr)
library(ggplot2)
library(dplyr)

KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)


# 수익성
ROA = KOR_fs$'지배주주순이익' / KOR_fs$'자산'
CFO = KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산'
ACCURUAL = CFO - ROA

# 재무성과
LEV = KOR_fs$'장기차입금' / KOR_fs$'자산'
LIQ = KOR_fs$'유동자산' / KOR_fs$'유동부채'
OFFER = KOR_fs$'유상증자'

# 운영 효율성
MARGIN = KOR_fs$'매출총이익' / KOR_fs$'매출액'
TURN = KOR_fs$'매출액' / KOR_fs$'자산'



if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = ncol(KOR_fs[[1]]) - 1
} else {
  num_col = ncol(KOR_fs[[1]]) 
}

F_1 = as.integer(ROA[, num_col] > 0)
F_2 = as.integer(CFO[, num_col] > 0)
F_3 = as.integer(ROA[, num_col] - ROA[, (num_col-1)] > 0)
F_4 = as.integer(ACCURUAL[, num_col] > 0) 
F_5 = as.integer(LEV[, num_col] - LEV[, (num_col-1)] <= 0) 
F_6 = as.integer(LIQ[, num_col] - LIQ[, (num_col-1)] > 0)
F_7 = as.integer(is.na(OFFER[,num_col]) |
                   OFFER[,num_col] <= 0)
F_8 = as.integer(MARGIN[, num_col] -
                   MARGIN[, (num_col-1)] > 0)
F_9 = as.integer(TURN[,num_col] - TURN[,(num_col-1)] > 0)



F_Table = cbind(F_1, F_2, F_3, F_4, F_5, F_6, F_7, F_8, F_9) 
F_Score = F_Table %>%
  apply(., 1, sum, na.rm = TRUE) %>%
  setNames(KOR_ticker$`종목명`)

(F_dist = prop.table(table(F_Score)) %>% round(3))

invest_F_Score = F_Score %in% c(9)
KOR_ticker[invest_F_Score, ] %>% 
  select(`종목코드`, `종목명`) %>%
  mutate(`F-Score` = F_Score[invest_F_Score])



library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)

KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)

if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = ncol(KOR_fs[[1]]) - 1
} else {
  num_col = ncol(KOR_fs[[1]]) 
}

quality_roe = (KOR_fs$'지배주주순이익' / KOR_fs$'자본')[num_col]
quality_gpa = (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col]
quality_cfo =
  (KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산')[num_col]

quality_profit =
  cbind(quality_roe, quality_gpa, quality_cfo) %>%
  setNames(., c('ROE', 'GPA', 'CFO'))


rank_quality = quality_profit %>% 
  mutate_all(list(~min_rank(desc(.))))

cor(rank_quality, use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))



rank_sum = rank_quality %>%
  rowSums()

invest_quality = rank(rank_sum) <= 30

KOR_ticker[invest_quality, ] %>%
  select(`종목코드`, `종목명`) %>%
  cbind(round(quality_profit[invest_quality, ], 4))