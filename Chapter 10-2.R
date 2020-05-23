####################################################
###Chapter 10 퀀트 전략을 이용한 종목선정 (심화)####
####################################################


##############################
# 10.1 섹터 중립 포트폴리오  #
##############################


library(stringr)
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(magrittr)


### 모멘텀 포트폴리오: 최근 12개월 수익률 구해 상위 30종목 선택

KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)



ret = Return.calculate(KOR_price) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) {prod(1+x) - 1})

invest_mom = rank(-ret_12m) <= 30



KOR_ticker[invest_mom, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`수익률` = round(ret_12m[invest_mom], 4)) %>% View()




### 섹터 정보 추가
KOR_sector = read.csv('data/KOR_sector.csv', row.names = 1,
                      stringsAsFactors = FALSE)

View(KOR_sector)
View(KOR_ticker)

KOR_sector$'CMP_CD' =
  str_pad(KOR_sector$'CMP_CD', 6, 'left', 0)
data_market = left_join(KOR_ticker, KOR_sector,
                        by = c('종목코드' = 'CMP_CD',
                               '종목명' = 'CMP_KOR'))

View(data_market)



##섹터 쏠림 확인
data_market[invest_mom, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(`SEC_NM_KOR`, `n`),
             y = `n`, label = n)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) + 
  theme_classic()



## 모멘텀(위험조정수익률): 
ret = Return.calculate(KOR_price) %>% xts::last(252) 
ret_12m = ret %>% sapply(., function(x) { prod(1+x) - 1})

std_12m = ret %>% apply(., 2, sd) %>% multiply_by(sqrt(252))  #연율화 변동성을 계산
sharpe_12m = ret_12m / std_12m  
invest_mom_sharpe = rank(-sharpe_12m) <= 30

data_market[invest_mom_sharpe, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(`SEC_NM_KOR`, `n`),
             y = `n`, label = n)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) + 
  theme_classic()




##섹터 중립 포트폴리오
sector_neutral = data_market %>%
  select(`종목코드`, `SEC_NM_KOR`) %>% #종목코드, 섹터 정보 선택
  mutate(`ret` = ret_12m) %>%   #계산한 12개월 수익률 정보 새로운 열에 추가
  group_by(`SEC_NM_KOR`) %>%   #섹터별 그룹 만들어줌
  mutate(scale_per_sector = scale(`ret`), #scale함수 이용해 그룹별 정규화
         scale_per_sector = ifelse(is.na(`SEC_NM_KOR`),
                                   NA, scale_per_sector)) #섹터 정보가 없을 경우 NA로 변경

#전체 종목에서 12개월 수익률을 비교하는 것이 아닌 각 섹터별로 수익률의 강도를 비교
#특정 종목의 과거 수익률이 전체 종목과 비교해서 높았더라도 해당 섹터 내에서의 순위가 낮다면, 정규화된 값은 낮아짐.

View(sector_neutral)



#정규화된 값 기준으로 상위 30종목 살펴보기
invest_mom_neutral =
  rank(-sector_neutral$scale_per_sector) <= 30

View(sector_neutral[invest_mom_neutral,])


data_market[invest_mom_neutral, ] %>%
  select(`SEC_NM_KOR`) %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(`SEC_NM_KOR`, `n`),
             y = `n`, label = n)) +
  geom_col() +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) +
  ylab(NULL) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) + 
  theme_classic()



#이처럼 group_by() 함수를 통해 손쉽게 그룹별 중립화를 할 수 있으며, 
#글로벌 투자를 하는 경우에는 지역, 국가, 섹터별로도 중립화된 포트폴리오를 구성 가능


##############################
# 10.2 마법공식              #
##############################
library(stringr)
library(xts)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(magrittr)

# 밸류와 퀄리티의 조합은 전통적으로 많이 사용된 방법이며, 대표적인 예가 조엘 그린블라트의 마법공식(Greenblatt 2010)

#####################################
#### 10.2.1 퀄리티와 밸류 간의 관계
####################################

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

data_pbr = KOR_value['PBR']

#################
if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = ncol(KOR_fs[[1]]) - 1} else {num_col = ncol(KOR_fs[[1]])}
#1월~4월의 경우 전년도 재무제표가 일부만 들어오는 경향이 있음
#전전년도 데이터를 사용해야 합니다. 따라서 Sys.Date() 함수를 통해 현재 날짜를 추출한 후, 
#lubridate 패키지의 month() 함수를 이용해 해당 월을 계산합니다. 
#만일 현재 날짜가 1~4월 일 경우 ncol(KOR_fs[[1]]) - 1을 이용해 전전년도 데이터를 선택하며, 
#그렇지 않을 경우(5~12월) 전년도 데이터를 선택합니다.
##################

data_gpa =
  (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col] %>%
  setNames('GPA')     
#매출총이익은 매출에서 매출원가만을 차감해 산출하기 때문에 영업이익이나 세전이익, 당기순이익에 비해 회계적으로 가공이 덜 되는 항목
#GPA 비율이 높은 기업은 그렇지 않은 기업보다 자산을 효율적으로 잘 활용하고 있다고 판단

View(data_gpa)
View(data_pbr)

cbind(data_pbr, -data_gpa) %>%
  cor(method = 'spearman', use = 'complete.obs') %>% round(4) %>%View() #스피어만 상관분석, 결측값이 있는 case는 모두 제거된 상태에서 상관계수 계산


cbind(data_pbr, data_gpa) %>%
  mutate(quantile_pbr = ntile(data_pbr, 5)) %>%   #PBR을 5분위수로 나눔
  filter(!is.na(quantile_pbr)) %>%  #PBR이 없는 종목은 제외
  group_by(quantile_pbr) %>%   #PBR의 분위수별 그룹을 묶어줍니다.
  summarise(mean_gpa = mean(GPA, na.rm = TRUE)) %>%   #각 PBR 그룹별 GPA의 평균값을 구해줌
  ggplot(aes(x = quantile_pbr, y = mean_gpa)) +
  geom_col() +
  xlab('PBR') + ylab('GPA')

##PBR이 낮을수록 GPA도 낮으며, 즉 가치주일수록 우량성은 떨어집니다. 
##반면에 PBR이 높을수록 GPA도 높으며, 이는 주식의 가격이 비쌀수록 우량성도 높다는 뜻입니다.





#####################################
#### 10.2.3 마법공식 구성하기
####################################
KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)
KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE)

if (lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {num_col = ncol(KOR_fs[[1]]) - 1} else {num_col = ncol(KOR_fs[[1]])}
#1월~4월의 경우 전년도 재무제표가 일부만 들어오는 경향이 있음
#전전년도 데이터를 사용해야 합니다. 따라서 Sys.Date() 함수를 통해 현재 날짜를 추출한 후, 
#lubridate 패키지의 month() 함수를 이용해 해당 월을 계산합니다. 
#만일 현재 날짜가 1~4월 일 경우 ncol(KOR_fs[[1]]) - 1을 이용해 전전년도 데이터를 선택하며, 
#그렇지 않을 경우(5~12월) 전년도 데이터를 선택합니다.
##################


# 분자:  분자 부분인 이자및 법인세 차감전이익은 지배주주 순이익에 법인세비용과 이자비용을 더해준 후 최근년도 데이터를 선택
magic_ebit = (KOR_fs$'지배주주순이익' + KOR_fs$'법인세비용' +  KOR_fs$'이자비용')[num_col]



# 분모 : 시가총액+총부채-여유자금
magic_cap = KOR_value$PER * KOR_fs$'지배주주순이익'[num_col]     #per*earning을 통해 '시가총액' 역산

magic_debt = KOR_fs$'부채'[num_col]        #'총부채'는 부채 항목

magic_excess_cash_1 = KOR_fs$'유동부채' - KOR_fs$'유동자산' +  KOR_fs$'현금및현금성자산'   # 여유자금 구하기위해 순 운전자본 구하기
magic_excess_cash_1[magic_excess_cash_1 < 0] = 0    #0보다 작을 경우 0으로
magic_excess_cash_2 =  (KOR_fs$'현금및현금성자산' - magic_excess_cash_1)[num_col]

magic_ev = magic_cap + magic_debt - magic_excess_cash_2


# 이익수익률
magic_ey = magic_ebit / magic_ev

View(magic_ey)




#투하자본 수익률
magic_ic = ((KOR_fs$'유동자산' - KOR_fs$'유동부채') +
              (KOR_fs$'비유동자산' - KOR_fs$'감가상각비'))[num_col]
magic_roc = magic_ebit / magic_ic
View(magic_roc)




#마법 공식 포트폴리오
invest_magic = rank(rank(-magic_ey) + rank(-magic_roc)) <= 30

magic_rank= KOR_ticker[invest_magic, ] %>%
  select(`종목코드`, `종목명`) %>%
  mutate(`이익수익률` = round(magic_ey[invest_magic, ], 4),
         `투하자본수익률` = round(magic_roc[invest_magic, ], 4))


View(magic_rank)





###########################################
#10.3 이상치 데이터 제거 및 팩터의 결합   #  
###########################################

# PBR 이상치 데이터 확인

KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)

max(KOR_value$PBR, na.rm = TRUE)


KOR_value %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)




##########################################
#### 10.3.1 트림(Trim): 이상치 데이터 삭제
#########################################



value_trim = KOR_value %>%
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99, NA, PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01, NA, PBR))

#percent_rank() 함수를 통해 백분위를 구한 후 상하위 1%에 해당하는 데이터들은 NA로 변경

value_trim %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)   #지나치게 PBR이 낮은 종목과 높은 종목은 제거되어 x축의 스케일이 많이 줄어든 모습



##########################################
#### 10.3.2 윈저라이징(Winsorizing)
#########################################

# 이상치 데이터 대체(포트폴리오 구성에서는 일반적으로 이상치 데이터를 다른 데이터로 대체하는 윈저라이징 방법이 사용)

value_winsor = KOR_value %>%
  select(PBR) %>%
  mutate(PBR = ifelse(percent_rank(PBR) > 0.99,
                      quantile(., 0.99, na.rm = TRUE), PBR),
         PBR = ifelse(percent_rank(PBR) < 0.01,
                      quantile(., 0.01, na.rm = TRUE), PBR))
   #상위 99%를 초과하는 데이터는 99% 값으로 대체하며, 하위 1% 미만의 데이터는 1% 데이터로 대체


value_winsor %>%
  ggplot(aes(x = PBR)) +
  geom_histogram(binwidth = 0.1)



##########################################
#### 10.3.3 팩터의 결합 방법
#########################################

#밸류 지표의 결합, 퀄리티 지표의 결합, 마법공식 포트폴리오를 구성할 때 단순히 랭킹을 더하는 방법을 사용
#그러나 전문투자자의 입장이거나 팩터를 분석하는 업무를 할 경우 단순 랭킹 더하는 방법은 여러 가지 문제를 안고 있음.

#밸류 지표 분포 확인
library(tidyr)

KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%  #순위(ranking) index 반환, 동일값에 대해서는 '1, 1, 1, 4, 4,...' 처리
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key) 


#랭킹을 구하는 것의 가장 큰 장점은 극단치로 인한 효과가 사라진다는 점과 균등한 분포를 가진다는 것
#그러나 각 지표의 x축을 보면 최댓값이 서로 다름. 
#이는 지표별 결측치로 인해 유효 데이터의 개수가 달라 나타나는 현상이며, 
#서로 다른 범위의 분포를 단순히 합치는 것은 좋은 방법이 아님


KOR_value %>%
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>% #데이터 정규화
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key)  




##########################################
#### 10.4 멀티팩터 포트폴리오
#########################################


# 데이터 불러오기
library(xts)
library(stringr)

KOR_fs = readRDS('data/KOR_fs.Rds')
KOR_value = read.csv('data/KOR_value.csv', row.names = 1,
                     stringsAsFactors = FALSE)
KOR_price = read.csv('data/KOR_price.csv', row.names = 1,
                     stringsAsFactors = FALSE) %>% as.xts()

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE) 
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, 'left', 0)



#퀄리티 지표 :ROE, GPA, CFO
#ROE = 당기순이익 / 평균 자기자본 (경영효율성)
#GPA = 매출총이익/ 자산총계로 
#CFO = 영업활동으로인한현금흐름 (/자산) : 보유하고 있는 자산을 얼마나 효율적으로 영업활동에 사용?
if ( lubridate::month(Sys.Date()) %in% c(1,2,3,4) ) {
  num_col = ncol(KOR_fs[[1]]) - 1
} else {
  num_col = ncol(KOR_fs[[1]]) 
}


quality_roe = (KOR_fs$'지배주주순이익' / KOR_fs$'자본')[num_col]    #ROE 자기자본이익률 계산
quality_gpa = (KOR_fs$'매출총이익' / KOR_fs$'자산')[num_col]        #GPA 매출총이익 계산
quality_cfo =
  (KOR_fs$'영업활동으로인한현금흐름' / KOR_fs$'자산')[num_col]   #영업활동현금흐름 계산

quality_profit =
  cbind(quality_roe, quality_gpa, quality_cfo) %>%
  setNames(., c('ROE', 'GPA', 'CFO')) 


factor_quality = quality_profit %>%
  mutate_all(list(~min_rank(desc(.)))) %>% #랭킹 구하기, 내림차순으로
  mutate_all(list(~scale(.))) %>%  #정규화
  rowSums() #계산된 Z-Score를 종목별로 합c쳐줌
View(factor_quality)


#분포 확인
factor_quality %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()




#밸류지표 : 밸류 지표는 이미 테이블 형태로 들어와 있으며, 랭킹과 표준화를 거쳐 합을 구해줌
View(KOR_value)
factor_value = KOR_value %>%   
  mutate_all(list(~min_rank(.))) %>%
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_value %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()




#모멘텀 지표
ret_3m = Return.calculate(KOR_price) %>% xts::last(60) %>% #최근 60일 : 3개월 수익률
  sapply(., function(x) {prod(1+x) - 1})
ret_6m = Return.calculate(KOR_price) %>% xts::last(120) %>% #최근 120일 : 6개월 수익률
  sapply(., function(x) {prod(1+x) - 1})
ret_12m = Return.calculate(KOR_price) %>% xts::last(252) %>% #최근252일 : 12개월 수익률
  sapply(., function(x) {prod(1+x) - 1})
ret_bind = cbind(ret_3m, ret_6m, ret_12m) %>% data.frame() #열로 묶어줌

factor_mom = ret_bind %>%
  mutate_all(list(~min_rank(desc(.)))) %>% #내림차순 기준 랭킹과 표준화를 거쳐 합을 구함
  mutate_all(list(~scale(.))) %>%
  rowSums()

factor_mom %>% 
  data.frame() %>%
  ggplot(aes(x = `.`)) +
  geom_histogram()




# 팩터 간 상관관계 분석
library(corrplot)

cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  setNames(c('Quality', 'Value', 'Momentum')) %>%
  cor(use = 'complete.obs') %>% #결측값이 있는 case는 모두 제거된 상태에서 상관계수 계산
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))

library(tidyr)

library(corrplot)

cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  setNames(c('Quality', 'Value', 'Momentum')) %>%
  cor(use = 'complete.obs') %>%
  round(., 2) %>%
  corrplot(method = 'color', type = 'upper',
           addCoef.col = 'black', number.cex = 1,
           tl.cex = 0.6, tl.srt = 45, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar=c(0,0,0.5,0))



##랭킹 상위종목 선택
factor_qvm =
  cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  mutate_all(list(~scale(.))) %>%
  mutate(factor_quality = factor_quality * 0.33,
         factor_value = factor_value * 0.33,
         factor_mom = factor_mom * 0.33) %>%  #모두 0.33의 동일한 가중치를 둠. 어떤 팩터 중요시 하냐에 따라 달라질 수 있음
  rowSums()


invest_qvm = rank(factor_qvm) <= 30
KOR_ticker[invest_qvm, ] %>%
  select('종목코드', '종목명') %>%
  cbind(round(quality_roe[invest_qvm, ], 2)) %>%
  cbind(round(KOR_value$PBR[invest_qvm], 2)) %>%
  cbind(round(ret_12m[invest_qvm], 2)) %>%
  setNames(c('종목코드', '종목명', 'ROE', 'PBR', '12M')) %>% View()



# 다른 가중치
factor_qvm2 =
  cbind(factor_quality, factor_value, factor_mom) %>%
  data.frame() %>%
  mutate_all(list(~scale(.))) %>%
  mutate(factor_quality = factor_quality * 0.33,
         factor_value = factor_value * 0.53,
         factor_mom = factor_mom * 0.13) %>%  #모두 0.33의 동일한 가중치를 둠. 어떤 팩터 중요시 하냐에 따라 달라질 수 있음
  rowSums()

invest_qvm2 = rank(factor_qvm2) <= 30
D= KOR_ticker[invest_qvm2, ] %>%
  select('종목코드', '종목명') %>%
  cbind(round(quality_roe[invest_qvm, ], 2)) %>%
  cbind(round(KOR_value$PBR[invest_qvm], 2)) %>%
  cbind(round(ret_12m[invest_qvm], 2)) %>%
  setNames(c('종목코드', '종목명', 'ROE', 'PBR', '12M')) 

View(D)



#랭킹 하위 종목

invest_qvm_d= rank(-factor_qvm)<=30
KOR_ticker[invest_qvm_d, ] %>%
  select('종목코드', '종목명') %>%
  cbind(round(quality_roe[invest_qvm_d, ], 2)) %>%
  cbind(round(KOR_value$PBR[invest_qvm_d], 2)) %>%
  cbind(round(ret_12m[invest_qvm_d], 2)) %>%
  setNames(c('종목코드', '종목명', 'ROE', 'PBR', '12M')) %>% View()



#지표별 평균
#랭킹 상위종목
cbind(quality_profit, KOR_value, ret_bind)[invest_qvm, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()



#랭킹 하위종목

cbind(quality_profit, KOR_value, ret_bind)[invest_qvm_d, ] %>% 
  apply(., 2, mean) %>% round(3) %>% t()




#퀄리티 지표별 분포
library(tidyr)

quality_profit[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)


#가치 지표별 분포
KOR_value[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)



#기간별 수익률 분포
ret_bind[invest_qvm, ] %>%
  gather() %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(. ~ key, scale = 'free', ncol = 1) +
  xlab(NULL)