######################
#개별 주가 크롤링 하기
###################

# 종목코드 6자리로 가공

###종목코드 확인
library(stringr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])


### 종목코드 6자리로 변경
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')


### 삼성전자 크롤링
library(xts)

ifelse(dir.exists('data/KOR_price'), FALSE,
       dir.create('data/KOR_price'))



i = 1
name = KOR_ticker$'종목코드'[i]


# 현재 날짜 시계열 데이터 생성
price = xts(NA, order.by = Sys.Date())
print(price)



# 개별 주가 크롤링
library(httr)
library(rvest)

url = paste0(
  'https://fchart.stock.naver.com/sise.nhn?symbol=',
  name,'&timeframe=day&count=500&requestType=0')
data = GET(url)
data_html = read_html(data, encoding = 'EUC-KR') %>%
  html_nodes('item') %>%
  html_attr('data') 
  
  

print(head(data_html))



#테이블 형태로 바꾸기
library(readr)

price = read_delim(data_html, delim = '|')  #read_delim
print(head(price))



#데이터 클렌징
library(lubridate)
library(timetk)

price = price[c(1, 5)] 
price = data.frame(price) #데이터 프레임 형태로 바꿈
colnames(price) = c('Date', 'Price')
price[, 1] = ymd(price[, 1])
price = tk_xts(price, date_var = Date)

print(head(price))

write.csv(price, paste0('data/KOR_price/', name,
                        '_price.csv'))



#전체 종목 크롤링
library(httr)
library(rvest)
library(stringr)
library(xts)
library(lubridate)
library(readr)


#위와 동일
#KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
#print(KOR_ticker$'종목코드'[1])
#KOR_ticker$'종목코드' =
#  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

#ifelse(dir.exists('data/KOR_price'), FALSE,
#       dir.create('data/KOR_price'))
####





for(i in 1 : nrow(KOR_ticker) ) {
  
  price = xts(NA, order.by = Sys.Date())
  name = KOR_ticker$'종목코드'[i]
  tryCatch({
    url = paste0(
      'https://fchart.stock.naver.com/sise.nhn?symbol='
      ,name,'&timeframe=day&count=500&requestType=0')
    data = GET(url)
    data_html = read_html(data, encoding = 'EUC-KR') %>%
      html_nodes("item") %>%
      html_attr("data") 
    price = read_delim(data_html, delim = '|')
    price = price[c(1, 5)] 
    price = data.frame(price)
    colnames(price) = c('Date', 'Price')
    price[, 1] = ymd(price[, 1])
    
    rownames(price) = price[, 1]
    price[, 1] = NULL}, error = function(e) {warning(paste0("Error in Ticker: ", name))})
  write.csv(price, paste0('data/KOR_price/', name,
                          '_price.csv'))
  Sys.sleep(2)
}





##############
#재무제표 크롤링
################

#삼성전자 재무제표
library(httr)
library(rvest)

ifelse(dir.exists('data/KOR_fs'), FALSE,
       dir.create('data/KOR_fs'))

Sys.setlocale("LC_ALL", "English")

url = paste0('http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A005930')

data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
data = data %>%
  read_html() %>%
  html_table()

Sys.setlocale("LC_ALL", "Korean")
  
lapply(data, function(x) {
  head(x, 3)})
 


#연간 기준 재무제표 선택
data_IS = data[[1]]
data_BS = data[[3]]
data_CF = data[[5]]

print(names(data_IS))

#전년 동기 삭제
data_IS = data_IS[, 1:(ncol(data_IS)-2)]

#데이터 클렌징
data_fs = rbind(data_IS, data_BS, data_CF)
data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                    '', data_fs[, 1])
data_fs = data_fs[!duplicated(data_fs[, 1]), ]

rownames(data_fs) = NULL
rownames(data_fs) = data_fs[, 1]
data_fs[, 1] = NULL

data_fs = data_fs[, substr(colnames(data_fs), 6,7) == '12']

print(head(data_fs))

sapply(data_fs, typeof)


#데이터 숫자형으로 변경
library(stringr)

data_fs = sapply(data_fs, function(x) {
  str_replace_all(x, ',', '') %>%
    as.numeric()
}) %>%
  data.frame(., row.names = rownames(data_fs))

print(head(data_fs))


write.csv(data_fs, 'data/KOR_fs/005930_fs.csv')




####
#가치지표
######
ifelse(dir.exists('data/KOR_value'), FALSE,
       dir.create('data/KOR_value'))



value_type = c('지배주주순이익',
               '자본',
               '영업활동으로인한현금흐름',
               '매출액')

value_index = data_fs[match(value_type, rownames(data_fs)),
                      ncol(data_fs)]
print(value_index)


#현재 주가 가져오기
library(readr)

url = 'http://comp.fnguide.com/SVO2/ASP/SVD_main.asp?pGB=1&gicode=A005930'
data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))

price = read_html(data) %>%
  html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
  html_text() %>%
  parse_number()

print(price)


# 보통주 발행주식수 가져오기
share = read_html(data) %>%
  html_node(
    xpath =
      '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
  html_text()

print(share)

share = share %>%
  strsplit('/') %>%
  unlist() %>%
  .[1] %>%
  parse_number()

print(share)



#가치 지표 계산하기
data_value = price / (value_index * 100000000 / share)
names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
data_value[data_value < 0] = NA

print(data_value)


write.csv(data_value, 'data/KOR_value/005930_value.csv')





#전체 재무제표 크롤링
library(stringr)
library(httr)
library(rvest)
library(stringr)
library(readr)
  
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
str_pad(KOR_ticker$'종목코드', 6,side = c('left'), pad = '0')
  
ifelse(dir.exists('data/KOR_fs'), FALSE,
         dir.create('data/KOR_fs'))
ifelse(dir.exists('data/KOR_value'), FALSE,
         dir.create('data/KOR_value'))
  
for(i in 1 : nrow(KOR_ticker) ) {
  
  data_fs = c()
  data_value = c()
  name = KOR_ticker$'종목코드'[i]
tryCatch({
    
    Sys.setlocale('LC_ALL', 'English')
    url = paste0(
      'http://comp.fnguide.com/SVO2/ASP/'
      ,'SVD_Finance.asp?pGB=1&gicode=A',
      name)
    data = GET(url,user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36')) %>%
      read_html() %>%
      html_table()
    Sys.setlocale('LC_ALL', 'Korean')
    data_IS = data[[1]]
    data_BS = data[[3]]
    data_CF = data[[5]]
    data_IS = data_IS[, 1:(ncol(data_IS)-2)]
    data_fs = rbind(data_IS, data_BS, data_CF)
    data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                        '', data_fs[, 1])
    data_fs = data_fs[!duplicated(data_fs[, 1]), ]
    rownames(data_fs) = NULL
    rownames(data_fs) = data_fs[, 1]
    data_fs[, 1] = NULL
    data_fs =
      data_fs[, substr(colnames(data_fs), 6,7) == "12"]
    data_fs = sapply(data_fs, function(x) {
      str_replace_all(x, ',', '') %>%
        as.numeric()
    }) %>%
      data.frame(., row.names = rownames(data_fs))
    value_type = c('지배주주순이익', 
                   '자본', 
                   '영업활동으로인한현금흐름', 
                   '매출액') 
    value_index = data_fs[match(value_type, rownames(data_fs)),
                          ncol(data_fs)]
    url =
      paste0(
        'http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp',
        '?pGB=1&gicode=A',name)
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
    price = read_html(data) %>%
      html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
      html_text() %>%
      parse_number()
    share = read_html(data) %>%
      html_node(
        xpath =
        '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
      html_text() %>%
      strsplit('/') %>%
      unlist() %>%
      .[1] %>%
      parse_number()
    data_value = price / (value_index * 100000000/ share)
    names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
    data_value[data_value < 0] = NA
    
  }, error = function(e) {
    data_fs <<- NA
    data_value <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  write.csv(data_fs, paste0('data/KOR_fs/', name, '_fs.csv'))
  write.csv(data_value, paste0('data/KOR_value/', name,
                               '_value.csv'))
  Sys.sleep(1)
}
