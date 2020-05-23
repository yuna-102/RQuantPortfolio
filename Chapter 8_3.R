###8.3 종목정보 시각화
####################

# 8.3.1 geom_point(): 산점도 나타내기

library(ggplot2)

ggplot(data_market, aes(x = ROE, y = PBR)) +
  geom_point() #산점도 그래프 그리기, 극단치 데이터 존재


ggplot(data_market, aes(x = ROE, y = PBR,
                        color = `시장구분`,
                        shape = `시장구분`)) + #그룹별로 컬러, 모양 다르게
  geom_point() +
  geom_smooth(method = 'lm') + #선형회귀선
  coord_cartesian(xlim = c(0, 0.30), ylim = c(0, 3)) # 극단치 제거

########################################
#8.3.2 geom_histogram(): 히스토그램 나타내기 : pbr
###########################################

ggplot(data_market, aes(x = PBR)) +
  geom_histogram(binwidth = 0.1) + 
  coord_cartesian(xlim = c(0, 10))



ggplot(data_market, aes(x = PBR)) +
  geom_histogram(aes(y = ..density..), #밀도함수로
                 binwidth = 0.1,
                 color = 'sky blue', fill = 'sky blue') + 
  coord_cartesian(xlim = c(0, 10)) +
  geom_density(color = 'red') + #밀도곡선
  geom_vline(aes(xintercept = median(PBR, na.rm = TRUE)), color = 'blue') + #중앙값으로 세로선
  geom_text(aes(label = median(PBR, na.rm = TRUE),
                x = median(PBR, na.rm = TRUE), y = 0.05),
            col = 'black', size = 6, hjust = -0.05) #hjust 



########################
#8.3.3 geom_boxplot(): 박스 플롯 나타내기
#########################
ggplot(data_market, aes(x = SEC_NM_KOR, y = PBR)) +
  geom_boxplot() +
  coord_flip() #x축, y축 뒤집어서



#######################
#8.3.4 dplyr과 ggplot을 연결하여 사용하기
#####################

data_market %>%
  filter(!is.na(SEC_NM_KOR)) %>% #섹터 na아닌 종목 선택
  group_by(SEC_NM_KOR) %>%
  summarize(ROE_sector = median(ROE, na.rm = TRUE),
            PBR_sector = median(PBR, na.rm = TRUE)) %>%
  ggplot(aes(x = ROE_sector, y = PBR_sector,
             color = SEC_NM_KOR, label = SEC_NM_KOR)) +
  geom_point() +
  geom_text(color = 'black', size = 3, vjust = 1.3) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank()) #범례 타이틀 지움




##########################
#8.3.5 geom_bar(): 막대 그래프 나타내기
#######################

data_market %>%
  group_by(SEC_NM_KOR) %>%
  summarize(n = n()) %>% #각 그룹별 데이터 개수
  ggplot(aes(x = SEC_NM_KOR, y = n)) +
  geom_bar(stat = 'identity') + #y축 n 데이터 그대로 이용
  theme_classic()


#보기 좋게 바꾸기
data_market %>%
  filter(!is.na(SEC_NM_KOR)) %>% #na 종목 삭제, 섹터별 종목 개수
  group_by(SEC_NM_KOR) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = reorder(SEC_NM_KOR, n), y = n, label = n)) +
  geom_bar(stat = 'identity') +
  geom_text(color = 'black', size = 4, hjust = -0.3) +
  xlab(NULL) +
  ylab(NULL) + #라벨 삭제
  coord_flip() +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) +  #그림 간격 넓혀줌
  theme_classic()





#8.4 주가 및 수익률 시각화
#######################


################
#8.4.1 주가 그래프 나타내기
################

library(quantmod)

getSymbols('SPY') #미국 s&p 500 지수, 데이터 xts 형식으로 다운
prices = Cl(SPY)

#더 깔끔하게 정리
library(ggplot2)

SPY %>%
  ggplot(aes(x = Index, y = SPY.Close)) +
  geom_line()




##############
#8.4.2 인터랙티브 그래프 나타내기
###############

library(dygraphs) #사용자의 움직임에 따라 반응

dygraph(prices) %>%
  dyRangeSelector()

library(highcharter)

highchart(type = 'stock') %>%
  hc_add_series(prices) %>%
  hc_scrollbar(enabled = FALSE)


library(plotly)

p = SPY %>%
  ggplot(aes(x = Index, y = SPY.Close)) +
  geom_line()

ggplotly(p)

prices %>%
  fortify.zoo %>%
  plot_ly(x= ~Index, y = ~SPY.Close ) %>%
  add_lines() #ggplot과 달리 plot_ly()함수는 파이프 오퍼레이터로 연결 가능


#############
#8.4.3 연도별 수익률 나타내기
############
#########질문
library(PerformanceAnalytics)

ret_yearly = prices %>%
  Return.calculate() %>%
  apply.yearly(., Return.cumulative) %>% #연도별 수익률 계산 , 반올림 
  round(4) %>%
  fortify.zoo() %>% #인뎃스에 있는 시간 데이터 index  열로
  mutate(Index = as.numeric(substring(Index, 1, 4))) #연도 부분 뽑아내서 숫자형태로 저장

ggplot(ret_yearly, aes(x = Index, y = SPY.Close)) +
  geom_bar(stat = 'identity') +
  scale_x_continuous(breaks = ret_yearly$Index,
                     expand = c(0.01, 0.01)) + #몯ㄴ 연도 출력
  geom_text(aes(label = paste(round(SPY.Close * 100, 2), "%"),
                vjust = ifelse(SPY.Close >= 0, -0.5, 1.5)), # 수익률 0볻 크면 위쪽, 작으면 아래ㅈ쪽에 표시
            position = position_dodge(width = 1),
            size = 3) +
  xlab(NULL) + ylab(NULL)