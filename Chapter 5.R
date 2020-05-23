pkg = c('magrittr', 'quantmod', 'rvest', 'httr', 'jsonlite',
        'readr', 'readxl', 'stringr', 'lubridate', 'dplyr',
        'tidyr', 'ggplot2', 'corrplot', 'dygraphs',
        'highcharter', 'plotly', 'PerformanceAnalytics',
        'nloptr', 'quadprog', 'RiskPortfolios', 'cccp',
        'timetk', 'broom', 'stargazer', 'timeSeries')

new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)}


# 필요한 패키지 라이브러리 불러오기
library(httr)
library(rvest)
library(readr)
library(stringr)
options(digits=20)
library(jsonlite)

# 1. 산업별 현황 크롤링
## 원하는 데이터 쿼리에 해당되는 OTP 받기
gen_otp_url = 'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'

gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = 'MKD/03/0303/03030103/mkd03030103',
  tp_cd = 'ALL',
  date = '20200417',
  lang = 'ko',
  pagePath = '/contents/MKD/03/0303/03030103/MKD03030103.jsp')

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()


## 생성된 OTP 제출하여 데이터 다운로드 하기

down_url = 'http://file.krx.co.kr/download.jspx'
down_sector = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()




## 데이터가 잘 불러졌는지 확인
View(down_sector)


##data 파일
ifelse(dir.exists('data'), FALSE, dir.create('data'))

## csv 파일로 저장하기
write_csv(down_sector,'data/krx_ind.csv')





# 2. 개별 종목 크롤링

gen_otp_url = 'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name= 'fileDown',
  filetype=  'csv',
  url= 'MKD/13/1302/13020401/mkd13020401',
  market_gubun = 'ALL',
  gubun = '1',
  schdate = '20200417',
  pagePath = '/contents/MKD/13/1302/13020401/MKD13020401.jsp')

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://file.krx.co.kr/download.jspx'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()


## 데이터가 잘 불러졌는지 확인
View(down_ind)



## csv 파일로 저장하기
write_csv(down_ind,'data/krx_ind.csv')







# 3. 최근 영업일 기준 데이터 받기
## 네이버 금융 사이트 이용하여 GET 방식으로 날짜 데이터 크롤링하기

url = 'https://finance.naver.com/sise/sise_deposit.nhn'


biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_1"]/div/ul[2]/li/span') %>%
  html_text() %>%
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>%
  str_replace_all('\\.', '')



print(biz_day)





## 산업별 현황 적용
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'

gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = 'MKD/03/0303/03030103/mkd03030103',
  tp_cd = 'ALL',
  date = biz_day, # 최근영업일로 변경
  lang = 'ko',
  pagePath = '/contents/MKD/03/0303/03030103/MKD03030103.jsp')

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://file.krx.co.kr/download.jspx'
down_sector = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()

write.csv(down_sector, 'data/krx_sector.csv')


View(down_sector)


## 개별 종목 지표 적용
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = "MKD/13/1302/13020401/mkd13020401",
  market_gubun = 'ALL',
  gubun = '1',
  schdate = biz_day, # 최근영업일로 변경
  pagePath = "/contents/MKD/13/1302/13020401/MKD13020401.jsp")

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://file.krx.co.kr/download.jspx'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()

write.csv(down_ind, 'data/krx_ind.csv')


View(down_ind)





# 4. 거래소 데이터 정리하기
## csv 파일 불러오기
down_sector = read.csv('data/krx_sector.csv', row.names = 1,
                       stringsAsFactors = FALSE)
down_ind = read.csv('data/krx_ind.csv',  row.names = 1,
                    stringsAsFactors = FALSE)



## 중복되는 열 이름 찾기
intersect(names(down_sector), names(down_ind))




## 하나의 데이터에만 존재하는 종목 찾기
setdiff(down_sector[, '종목명'], down_ind[ ,'종목명'])




## 데이터 하나로 합치기
KOR_ticker = merge(down_sector, down_ind,
                   by = intersect(names(down_sector),
                                  names(down_ind)),
                   all = FALSE)

View(KOR_ticker)



## 시가총액 기준 내림차순 정렬하기
KOR_ticker = KOR_ticker[order(-KOR_ticker['시가총액.원.']), ]
print(head(KOR_ticker))



View(KOR_ticker)


## 스팩, 우선주 제거하기 
KOR_ticker[grepl('스팩', KOR_ticker[, '종목명']), '종목명']  
KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) != 0, '종목명']


KOR_ticker = KOR_ticker[!grepl('스팩', KOR_ticker[, '종목명']), ]  
KOR_ticker = KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) == 0, ]

View(KOR_ticker)




## 행이름 초기화 후 저장
rownames(KOR_ticker) = NULL
write.csv(KOR_ticker, 'data/KOR_ticker.csv')




#섹터 정보 크롤링
##json 크롤링
library(jsonlite)

url = 'http://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=20190607&sec_cd=G10'
data = fromJSON(url)

lapply(data, head)

##sec_code 
sector_code = c('G25', 'G35', 'G50', 'G40', 'G10',
                'G20', 'G55', 'G30', 'G15', 'G45')
data_sector = list()

for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  
  Sys.sleep(1)
}

data_sector = do.call(rbind, data_sector)

write.csv(data_sector, 'data/KOR_sector.csv')

