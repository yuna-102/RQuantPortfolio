################
#8.1 종목정보 데이터 분석
################

library(stringr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1,
                      stringsAsFactors = FALSE)
KOR_sector = read.csv('data/KOR_sector.csv', row.names = 1,
                      stringsAsFactors = FALSE)

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6,'left', 0)
KOR_sector$'CMP_CD' =
  str_pad(KOR_sector$'CMP_CD', 6, 'left', 0)



#############
#8.1.1 *_join: 데이터 합치기
##############

library(dplyr)

data_market = left_join(KOR_ticker, KOR_sector,   #거래소 ticker 기준으로 합치기
                        by = c('종목코드' = 'CMP_CD',
                               '종목명' = 'CMP_KOR'))    

head(data_market)




#####################
#8.1.2 glimpse(): 데이터 구조 확인하기
####################
glimpse(data_market)



##################
#8.1.3 rename(): 열 이름 바꾸기
####################
head(names(data_market), 10)

data_market = data_market %>%
  rename(`시가총액` = `시가총액.원.`)

head(names(data_market), 10)


###############
#8.1.4 distinct(): 고유한 값 확인
#################
data_market %>%
  distinct(SEC_NM_KOR) %>% c()   #고유한 값 반환



#################
#8.1.5 select(): 원하는 열만 선택
##############

data_market %>%
  select(`종목명`) %>% head()

data_market %>%
  select(`종목명`, `PBR`, `SEC_NM_KOR`) %>% head()

data_market %>%
  select(starts_with('시')) %>% head()    #특정 문자로 시작하는 열들을 선택


data_market %>%
  select(ends_with('R')) %>% head()   #특정 문자로 끝나는 열들을 선택

data_market %>%
  select(contains('가')) %>% head()  #특정 문자가 포함되는 열들을 선택




################
#8.1.6 mutate(): 열 생성 및 데이터 변형
#############

data_market = data_market %>%
  mutate(`PBR` = as.numeric(PBR),
         `PER` = as.numeric(PER),
         `ROE` = PBR / PER,
         `ROE` = round(ROE, 4),
         `size` = ifelse(`시가총액` >=
                           median(`시가총액`, na.rm = TRUE),
                         'big', 'small')
  )

data_market %>%
  select(`종목명`, `ROE`, `size`) %>% head()




##############3
#8.1.7 filter(): 조건을 충족하는 행 선택
#########

data_market %>%
  select(`종목명`, `PBR`) %>%
  filter(`PBR` < 1) %>% head()


data_market %>%
  select(`종목명`, `PBR`, `PER`, `ROE`) %>%
  filter(PBR < 1 & PER < 20 & ROE > 0.1 ) %>% head()


###################
#8.1.8 summarize(): 요약 통곗값 계산
#################
data_market %>%
  summarize(PBR_max = max(PBR, na.rm = TRUE),
            PBR_min = min(PBR, na.rm = TRUE))


################3
#8.1.9 arrange(): 데이터 정렬 (디폴트 오름차순)
##################
data_market %>%
  select(PBR) %>%
  arrange(PBR) %>%
  head(5)

data_market %>%
  select(ROE) %>%
  arrange(desc(ROE)) %>%  #내림차순
  
  
  
##################
#8.1.10 row_number(): 순위 계산   (디폴트 오름차순)
#####################
data_market %>%
  mutate(PBR_rank = row_number(PBR)) %>%
  select(`종목명`, PBR, PBR_rank) %>%
  arrange(PBR) %>%
  head(5)


data_market %>%
  mutate(PBR_rank = row_number(desc(ROE))) %>%
  select(`종목명`, ROE, PBR_rank) %>%
  arrange(desc(ROE)) %>%
  head(5)


#####################3
#8.1.11 ntile(): 분위수 계산
######################3
data_market %>%
  mutate(PBR_tile = ntile(PBR, n = 5)) %>%
  select(PBR, PBR_tile) %>%
  head()





################
#8.1.12 group_by(): 그룹별로 데이터를 묶기
##################

data_market %>%
  group_by(`SEC_NM_KOR`) %>%
  summarize(n())   # n(): 해당 그룹 내 데이터의 개수

#섹터를 기준으로 데이터를 묶은 후 summarize()를 통해 각 섹터에 속하는 종목의 PBR 중앙값을 구한 후 정렬
data_market %>%
  group_by(`SEC_NM_KOR`) %>% 
  summarize(PBR_median = median(PBR, na.rm = TRUE)) %>%
  arrange(PBR_median)

#시장과 섹터를 기준으로 데이터를 그룹화한 후 각 그룹별 PBR 중앙값
data_market %>%
  group_by(`시장구분`, `SEC_NM_KOR`) %>%
  summarize(PBR_median = median(PBR, na.rm = TRUE)) %>%
  arrange(PBR_median)


