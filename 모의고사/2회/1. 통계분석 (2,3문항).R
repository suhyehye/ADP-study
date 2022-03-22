#회귀분석
data <- read.csv("C:\\Users\\suhye\\Desktop\\ADP\\모의고사 R코드 및 데이터\\모의고사 2회\\Admission.csv")
data
lm_result <- lm(Chance_of_Admit~GRE+TOEFL+Univ_Rating+SOP+LOR+CGPA+Research,data)
step(lm_result,direction = 'both')

#모형에 대한 정보 확인
lm2 <- lm(Chance_of_Admit~GRE+TOEFL+LOR+CGPA+Research,data = data)
summary(lm2)

#독립성을 만족하는지 확인
install.packages('lmtest')
library(lmtest)

dwtest(lm2)


#정규성을 만족하는지 확인
shapiro.test(resid(lm2))


#등분산성과 정규성 가정을 만족하는지 확인하기 위해 그래프를 통해 데이터 시각화
par(mfrow=c(2,2))
plot(lm2)
