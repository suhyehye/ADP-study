data <- read.csv("C:\\Users\\suhye\\Desktop\\ADP\\모의고사 R코드 및 데이터\\모의고사 4회\\bike_marketing.csv")

#1. pop_density 변수를 factor형으로 변환하고, pop_density별 revenues의 평균차이가 있는 지 통계분석을 시행,해석
## 대립가설이 채택되면 사후분석을 실시하고 해석

str(data)
head(data)
sum(is.na(data))

#factor변환
data$pop_density <- as.factor(data$pop_density)
str(data)

#일원배치 분산분석
result <- aov(revenues~pop_density,data=data)
summary(result)
#p-value값이 0.05보다 큼
# 인구밀집정도에 따른 매출은 통계적으로 유의한 차이가 없다고 판단

#2. google_adwords, facebook, twitter, marketing_total, employees가 revenues에 영향을 미치는지 알아보는 회귀분석
##회귀모형
bike_lm <- lm(revenues~google_adwords+facebook+twitter+marketing_total+employees, data = data)
summary(bike_lm)

##전진선택법
formula_low <- lm(revenues~1, data = data)
formula_up <- lm(revenues~google_adwords+facebook+twitter+marketing_total+employees,data=data)
step(formula_low,scope=list(upper=formula_up),direction = 'forward')


#3.