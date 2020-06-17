## 1. (a). ANOVA 검정
rm(list = ls())
N = 25
a = 5
n = 5
batch = as.factor(rep((1:5), each = 5))
calcium = c(23.46, 23.48, 23.56, 23.39, 23.40,
            23.59, 23.46, 23.42, 23.49, 23.50,
            23.51, 23.64, 23.46, 23.52, 23.49,
            23.28, 23.40, 23.37, 23.46, 23.39,
            23.29, 23.46, 23.37, 23.32, 23.38)
dat = data.frame(calcium, batch)
fit = aov(calcium ~ batch, data = dat)
summary(fit)
## p-value가 0.00363으로 유의수준인 0.05보다 작으므로,
## H0: 모든 batch에서 평균이 같다. <- 귀무가설 기각
## H1: 적어도 하나 이상의 batch에서 평균이 다르다. <- 대립가설 채택
## 따라서, 유의수준 0.05에서 batch에 따른 calcium content의 변화가 있다.

## 1. (b). variance components
all_mean = mean(calcium)
b1_mean = mean(calcium[1:5])
b2_mean = mean(calcium[6:10])
b3_mean = mean(calcium[11:15])
b4_mean = mean(calcium[16:20])
b5_mean = mean(calcium[21:25])
SSTr = n * sum((c(b1_mean, b2_mean, b3_mean, b4_mean, b5_mean)-all_mean)^2)
MSTr = SSTr / (a-1)
SSE = sum((calcium - rep(c(b1_mean, b2_mean, b3_mean, b4_mean, b5_mean), each = 5))^2)
MSE = SSE / (N-a)
c(MSTr, MSE)
## MSTr = 0.024244, MSE = 0.004380
e_s2 = MSE
tr_s2 = (MSTr-MSE) / n
all_s2 = e_s2 + tr_s2
c(all_s2, tr_s2, e_s2)
## 전체 분산 추정치 = 0.0083528
## 트리트먼트 분산 추정치 = 0.0039728
## 에러 분산의 추정치 = 0.0043800

## 1. (c). confidence interval
L = (1/n) * ((MSTr/MSE)*(1/qf(0.975, 4, 20)) - 1)
U = (1/n) * ((MSTr/MSE)*(1/qf(0.025, 4, 20)) - 1)
c(L/(1+L), U/(1+U))
## 95% confidence interval is (0.1031168 0.9026871)

## 1. (d). check assumptions
par(mfrow = c(2, 2))
plot(fit)
## 잔차를 통한 분석결과,
## 잔차가 정규분포와 비슷한 형태를 가지므로 정규성 가정 만족
## 독립변인에 관계없이 잔차의 분산이 일정한 수준을 가지므로 등분산성 가정 만족
## 잔차가 자기상관을 갖는 것처럼 보이지 않으므로 독립성 가정 만족

###################################################################################

## 2. (a). ANOVA 검정
rm(list = ls())
N = 12
a = 4
n = 3
chemist = as.factor(rep(1:4, each = 3))
alcohol_per = c(84.99, 84.04, 84.38, 85.15, 85.13, 84.88,
                84.72, 84.48, 85.16, 84.20, 84.10, 84.55)
dat = data.frame(chemist, alcohol_per)
fit = aov(alcohol_per ~ chemist, data = dat)
summary(fit)
## p-value가 0.0813으로 유의수준인 0.05보다 크므로,
## H0: 모든 chemist에서 평균이 같다. <- 귀무가설 채택
## H1: 적어도 하나 이상의 chemist에서 평균이 다르다. <- 대립가설 기각
## 따라서, 유의수준 0.05에서 chemist에 따른 percentage of methly alcohol의 변화가 없다.


## 2. (b). check assumptions
par(mfrow = c(2, 2))
plot(fit)
## 잔차를 통한 분석결과,
## 잔차가 정규분포와 비슷한 형태를 가지므로 정규성 가정 만족
## 독립변인에 관계없이 잔차의 분산이 일정한 수준을 가지므로 등분산성 가정 만족
## 잔차가 자기상관을 갖는 것처럼 보이지 않으므로 독립성 가정 만족

## 2. (c). orthogonal contrasts
## 새로운 chemist 2가 주된 관심사이므로 M2와의 비교를 최대한 반영함
## H0: M1 = M2, C1 = +1 -1 0 0
## H0: M1 + M2 = M3 + M4 , C2 = +1 +1 -1 -1
## H0: M3 = M4, C3 = 0 0 +1 -1
## 위의 표처럼 contrasts를 설정하면 모두 orthogonal하다.

###################################################################################

## 3. pooled t-test == single factor ANOVA
rm(list = ls())
set.seed(100)
## 과제 평가를 위해 seed 설정
mean_pop = runif(2, min = 1, max = 10)
variance_pop = runif(2, min = 1, max = 10)
## 두 모집단 랜덤 설정
n = sample(1:30, size = 2, replace = T)
## 추출할 두 표본집단의 크기를 랜덤 설정
group_a = rnorm(n[1], mean = mean_pop[1], sd = sqrt(variance_pop[1]))
group_b = rnorm(n[2], mean = mean_pop[2], sd = sqrt(variance_pop[2]))
## 두 모집단으로부터 각각의 표본집단 추출
sample_mean = c(mean(group_a), mean(group_b))
s2 = c(var(group_a), var(group_b))
s2_pooled = ((n[1]-1)*s2[1] + (n[2]-1)*s2[2]) / (n[1] + n[2] - 2)
## 표본평균과 표본분산을 이용하여 pooled estimator 생성
t = (sample_mean[1] - sample_mean[2]) / sqrt(s2_pooled * (1/n[1] + 1/n[2]))
p_value = min(2 * (1 - pt(t, df = n[1] + n[2] -2)), 2 * pt(t, df = n[1] + n[2] -2))
p_value
t.test(group_a, group_b, var.equal = T)
## pooled estimator를 이용한 t-test 결과 도출
## p-value = 0.2545418

value = c(group_a, group_b)
group_part = as.factor(c(rep('a', n[1]), rep('b', n[2])))
dat = data.frame(value, group_part)
## 두 표본집단을 구별하는 single factor 생성
fit = aov(value ~ group_part, data = dat)
summary(fit)
## single factor ANOVA 결과 도출
## p-value = 0.255

## 두 테스트의 결과가 일치함을 볼 수 있다.

###################################################################################

## 4. (a). fixed model RCBD
rm(list = ls())
chemical = as.factor(rep(1:4, each = 5))
bolt_block = as.factor(rep(1:5, 4))
tensile_str = c(73, 68, 74, 71, 67, 73, 67, 75, 72, 70,
          75, 68, 78, 73, 68, 73, 71, 75, 75, 69)
dat = data.frame(chemical, bolt_block, tensile_str)
fit = aov(tensile_str ~ chemical + bolt_block, data = dat)
summary(fit)
## 트리트먼트인 chemical의 p-value가 0.121로 유의수준 0.05보다 크므로,
## H0: 모든 chemical agents에서 tensile strengths의 평균은 같다. <-귀무가설 채택
## H1: 적어도 하나의 chemical agents에서 tensile strengths의 평균은 나머지와 다르다. <- 대립가설 기각
## 따라서, 유의수준 0.05에서 chemical agents에 따른 tensile strengths의 차이가 없다고 결론내린다.

## 4. (b). plot
par(mfrow = c(1, 1))
boxplot(tensile_str[1:5], tensile_str[6:10], tensile_str[11:15], tensile_str[16:20])
## 박스플롯을 그린 결과, chemical = 1일 때와 chemical = 2일 때와 chemical = (3, 4)일 때
## 세 경우에서 평균 차이가 어느 정도 존재하는 것처럼 보인다.
## 더 자세한 비교를 위해 t분포를 사용하여 평균차 검정을 해보기로 한다.
library(agricolae)
LSD.result = LSD.test(fit, "chemical", group = F)
LSD.result
## Fisher's LSD는 t-test를 이용하여 각각의 페어들을 비교한다.
## LSD결과, chemical = (1, 4) 하나의 짝에서만 p-value 0.0370으로 유의수준 0.05보다 작다.
## 따라서, chemical = (1, 4) 하나의 짝만 tensile strengths의 평균이 다르다고 결론짓는다.
LSD.result2 = LSD.test(fit, "chemical", group = T)
LSD.result2
## 결과를 그룹 형태로 출력한 결과, tensile strengths의 평균은
## "그룹a(4) >= 그룹ab(3, 2) >= 그룹b(1)" & "그룹a(4) > 그룹b(1)"로 나타난다.

###################################################################################

## 5. (a). ANOVA
rm(list = ls())
treatment = as.factor(rep(c("MP", "MPHD", "MPLD"), each = 10))
responses = c(334.5, 31.6, 701, 41.2, 61.2, 69.6, 67.5, 66.6, 120.7, 881.9,
          919.4, 404.2, 1024.8, 54.1, 62.8, 671.6, 882.1, 354.2, 321.9, 91.1,
          108.4, 26.1, 240.8, 191.1, 69.7, 242.8, 62.7, 396.9, 23.6, 290.4)
dat = data.frame(responses, treatment)
fit = aov(responses ~ treatment, data = dat)
summary(fit)
## ANOVA 분석 결과, H0: 모든 트리트먼트에서 responses의 평균이 같다. <- 라는 귀무가설이 맞다고 가정할 때,
## F-vakue = 3.249로 나올 확률인 p-value가 0.0544로 흔히 쓰이는 유의수준인 0.05에 거의 근사하므로,
## 적어도 하나 이상의 트리트먼트에서 responses의 평균이 나머지와 어느 정도 다르다고 결론지을 수 있다.

## 5. (b). check the normality assumption
par(mfrow = c(1, 1))
qqnorm(fit$residuals)
qqline(fit$residuals)
## qqplot 결과, 그래프가 선형에서 많이 벗어나며 곡선의 형태를 띠므로,
## 모집단이 정규분포라고 가정하기 힘들다. -> 정규성 가정 위반

## 5. (c). logarithm
log_responses = log(responses)
dat2 = data.frame(log_responses, treatment)
fit2 = aov(log_responses ~ treatment, data = dat)
summary(fit2)
## ANOVA 분석 결과, p-value가 0.0951로 흔히 쓰이는 유의수준인 0.05보다 많이 크므로,
## H0: 모든 트리트먼트에서 log_responses의 평균이 같다. <- 귀무가설 채택
## H1: 적어도 하나 이상의 트리트먼트에서 log_responses의 평균이 나머지와 다르다. <- 대립가설 기각
## 따라서, 각각의 트리트먼트마다 log_responses의 평균이 다르다고 주장할 근거가 없다. 

## 5. (d). model adequacy
par(mfrow = c(2, 2))
plot(fit2)
## 잔차를 통한 분석결과,
## 잔차가 정규분포와 비슷한 형태를 가지므로 정규성 가정 만족
## 독립변인에 관계없이 잔차의 분산이 일정한 수준을 가지므로 등분산성 가정 만족
## 잔차가 자기상관을 갖는 것처럼 보이지 않으므로 독립성 가정 만족

###################################################################################

## 6. Latin square
rm(list = ls())
ingredients_trt = as.factor(c('A', 'B', 'D', 'C', 'E',
                          'C', 'E', 'A', 'D', 'B',
                          'B', 'A', 'C', 'E', 'D',
                          'D', 'C', 'E', 'B', 'A',
                          'E', 'D', 'B', 'A', 'C'))
value = c(8, 7, 1, 7, 3,
          11, 2, 7, 3, 8,
          4, 9, 10, 1, 5,
          6, 8, 6, 6, 10,
          4, 2, 3, 8, 8)
batch_block = as.factor(rep(1:5, each = 5))
day_block = as.factor(rep(1:5, 5))
dat = data.frame(batch_block, day_block, ingredients_trt, value)
fit = aov(value ~ ingredients_trt + batch_block + day_block, data = dat)
summary(fit)
## ANOVA 분석 결과, 트리트먼트 p-value가 0.000488로 유의수준 0.05보다 작으므로,
## H0: 모든 ingredients에서 value의 평균이 같다. -> 귀무가설 기각
## H1: 적어도 하나 이상의 ingredients에서 value의 평균은 나머지와 다르다. -> 대립가설 채택
## 따라서, 유의수준  0.05에서 ingredients에 따른 value의 평균 차이가 존재한다고 결론짓는다.



