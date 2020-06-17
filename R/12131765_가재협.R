## 0. 초기화
rm(list = ls())

## 1. 데이터에 대한 ANOVA table를 생성하시오.
Looms = as.factor(1:4)
Observation = 1:4
dat = expand.grid(Looms = Looms, Observation = Observation)
dat$values = c(98, 91, 96, 95, 97, 90, 95, 96, 99, 93, 97, 99, 96, 92, 95, 98)
dat
fit = aov(values ~ Looms, dat = dat)
summary(fit)
## 아노바분석 결과, 유의수준 0.05에서 p-value가 0.000188로 귀무가설을 기각하므로,
## H0: Looms = (1, 2, 3, 4)에서 values의 평균이 모두 같다. -> 기각
## H1: Looms = (1, 2, 3, 4)에서 values의 평균이 적어도 하나는 다르다. -> 채택

## 2. 위에서 적합한 ANOVA의 가정에 대하여 그래프를 통해 진단하시오.
par(mfrow = c(2, 2))
plot(fit)
par(mfrow = c(1, 1))
plot(dat$Observation, resid(fit))
## 잔차를 통한 분석결과,
## 잔차가 정규분포와 비슷한 형태를 가지므로 정규성 가정 만족
## 독립변인에 관계없이 잔차의 분산이 일정한 수준을 가지므로 등분산성 가정 만족
## 잔차가 자기상관을 갖는 것처럼 보이지 않으므로 독립성 가정 만족

## 3. agricolae 패키지를 활용하여 Scheffe test / Tukey test / LSD 방법을 적용하시오.
library(agricolae)
scheffe.result = scheffe.test(fit, trt='Looms', group = F)
scheffe.result
## 유의수준 0.05에서 평균차 기각역이 3.15048이므로,
## 다른 집단과의 평균차가 기각역을 넘지 않는 Looms = (1, 3, 4)는 집단 간의 평균차가 존재하지 않는다 결론짓고,
## 다른 집단과의 평균차가 기각역을 넘는 Looms = 2는 다른 집단과 평균차가 존재한다고 결론짓는다.
scheffe.result2 = scheffe.test(fit, trt='Looms', group = T)
scheffe.result2
## 결과를 그룹 형태로 출력한 결과, values의 평균은 그룹a(1, 4, 3) > 그룹b(2)로 나타난다.
tuk.result <- HSD.test(fit, "Looms", group = F)
tuk.result
## Looms = (1, 3, 4)는 유의수준 0.05에서 집단 간의 평균차가 존재하지 않는다고 나타난다.
## Looms = 2는 유의수준 0.05에서 다른 집단과의 평균차가 존재한다고 나타난다.
tuk.result2 <- HSD.test(fit, "Looms", group = T)
tuk.result2
## 결과를 그룹 형태로 출력한 결과, values의 평균은 그룹a(1, 4, 3) > 그룹b(2)로 나타난다.
LSD.result <- LSD.test(fit, "Looms", group = F)
LSD.result
## Looms = (1, 3, 4)는 유의수준 0.05에서 집단 간의 평균차가 존재하지 않는다고 나타난다.
## Looms = 2는 유의수준 0.05에서 다른 집단과의 평균차가 존재한다고 나타난다.
LSD.result2 <- LSD.test(fit, "Looms", group = T)
LSD.result2
## 결과를 그룹 형태로 출력한 결과, values의 평균은 그룹a(1, 4, 3) > 그룹b(2)로 나타난다.