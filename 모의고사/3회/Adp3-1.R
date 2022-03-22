install.packages("ISLR")
library(ISLR)
car <- Carseats
car
head(car)
sum(is.na(car))


#1 Urban 변수에 따른 Sales의 차이가 있는지 통계적으로 검정
#등분산성검정
var.test(Sales~Urban,data=car)

#독립표본 t-test
t.test(Sales~Urban,data=car,alternative="two.sided",var_equal=TRUE)
# p-value가 0.05보다 크기 때문에 귀무가설을 기각하지 않는다. 
# 따라서 도시지역과 도시가 아닌 지역간 차 판매량에는 통계적으로 유의한 차이가 존재하지 않는다고 판단

#2. Sales 변수와 CompPrice, Income, Advertising, Population, Price, Age, Education 변수들간에 피어슨 상관계수를 이용한 상관관계 분석

#Sales
cor(car$Sales,car$CompPrice)
cor.test(car$Sales,car$CompPrice)
#거의 상관성 존재하지 않음, 상관관계는 통계적으로 유의하지 않음

#Income
cor(car$Sales,car$Income)
cor.test(car$Sales,car$Income)
#상관관계는 0.15, p-value값이 0.05보다 작아 통계적으로 유의

#Advertising
cor(car$Sales,car$Advertising)
cor.test(car$Sales,car$Advertising)
#p-value가 매우작아 통계적으로 유의, 상관관계는 0.27

#Population
cor(car$Sales,car$Population)
cor.test(car$Sales,car$Population)
#p-value값이 커 통계적으로 유의미하지 않음, 상관관계도 작음

#Price
cor(car$Sales,car$Price)
cor.test(car$Sales,car$Price)
#p-value값이 작아 통계적으로 유의미, 상관관계는 -0.44

#Age
cor(car$Sales,car$Age)
cor.test(car$Sales,car$Age)
#p-value값이 작아 통계적으로 유의미, 상관관계는 -0.23

#Education
cor(car$Sales,car$Education)
cor.test(car$Sales,car$Education)
#p-value값이 0.05보다 커 통계적으로 유의미하지 않음

#상관계수 행렬을 생성하고, 이를 시각화
cor(car[,-c(7,10,11)])
plot(car[,-c(7,10,11)])



#3. 종속변수를 Sales, 독립변수를 CompPrice, Income, Advertising, Population, Price, Age, Education으로 설정하고 후진 제거법을 활용하여 회귀분석 수행
result <- lm(Sales~CompPrice+Income+Advertising+Population+Price+Age+Education,car)
step(result,direction = "backward")


#4.회귀모델에 대해 해석
car_lm <- lm(Sales~CompPrice+Income+Advertising+Price+Age,data=car)
summary(car_lm)
