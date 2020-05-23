#########################
########ggplot() 기초
####################



##########
#8.2.1 diamonds 데이터셋 & Data, Aesthetics, Geometrics
#############
library(ggplot2)

data(diamonds)
head(diamonds)

ggplot(data = diamonds, aes(x = carat, y = price)) +
  geom_point()   #산점도

#컬러 맵핑
library(magrittr)

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut))



########
#8.2.3 Facets
######

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point() +
  facet_grid(. ~ cut)


###############
#8.2.4 Statistics
###############

diamonds %>%
  ggplot(aes(x = cut, y = carat)) +
  stat_summary_bin(fun= 'mean', geom = 'bar')  #cut에 따른 carat의 평균값 
#######<<<<<stat_summary_bin>>>>>>


##################
#8.2.5 Coordinates
################

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut)) +
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 20000))



diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_boxplot(aes(group = cut)) +
  coord_flip() # x축과 y축을 뒤집어 표현


####################3
#8.2.6 Theme
#################

diamonds %>%
  ggplot(aes(x = carat, y = price)) +
  geom_point(aes(color = cut)) +
  theme_bw() +    #배경흰색
  labs(title = 'Relation between Carat & Price',
       x = 'Carat', y = 'Price') +
  theme(legend.position = 'bottom',   #범례 하단
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()    #격자 제거
  ) +
  scale_y_continuous(
    labels = function(x) {
      paste0('$', 
             format(x, big.mark = ','))  #천원 단위로 콤마(,)를 붙여주며, 이를 달러($) 표시와 합침
    })