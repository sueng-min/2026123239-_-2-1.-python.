# 통계학입문 과제 2 — 단측 Z-가설검정 (R 학생용)
# 이름: 신승민  학번: 2026123239
# 목표: Z-검정, 임계값, p-value, if/else 판단, 기각역 그래프를 작성함
# base R만 사용함

# [상황] 음료의 공칭 평균 용량은 500mL임.
# 36개 표본의 평균은 496mL이며, 모표준편차는 12mL로 알려져 있음.
# 실제 평균이 500mL보다 작은지 유의수준 5%에서 검정함.

# 문제 1. 가설을 작성할 것
# H0: μ = 500 (평균 용량이 500mL임)
# H1: μ < 500 (평균 용량이 500mL보다 작음)

mu_0 <- 500
x_bar <- 496
sigma <- 12
n <- 36
alpha <- 0.05

# 문제 2. 표준오차
standard_error <- sigma / sqrt(n)

# 문제 3. Z-검정통계량
z_stat <- (x_bar - mu_0) / standard_error

# 문제 4. 왼쪽 단측검정의 임계값
critical_z <- qnorm(alpha)

# 문제 5. p-value
p_value <- pnorm(z_stat)

cat('2. 표준오차:', standard_error, '\n')
cat('3. Z-검정통계량:', z_stat, '\n')
cat('4. 임계값:', critical_z, '\n')
cat('5. p-value:', p_value, '\n')

# 문제 6. 귀무가설 기각 여부
if (!is.na(p_value)) {
  if (p_value < alpha) {
    cat('귀무가설을 기각함\n')
  } else {
    cat('귀무가설을 기각하지 못함\n')
  }
}

# 문제 7. 표준정규분포 그래프에 왼쪽 기각역과 수직선 표시
if (!is.na(critical_z) && !is.na(z_stat)) {
  x <- seq(-4, 4, length.out=500)
  y <- dnorm(x)
  plot(x, y, type='l', lwd=2, main='Left-tailed Z test (alpha = 0.05)',
       xlab='Z', ylab='Density')
  reject_x <- x[x <= critical_z]
  reject_y <- dnorm(reject_x)
  polygon(c(reject_x[1], reject_x, tail(reject_x, 1)),
          c(0, reject_y, 0), col='gray80', border=NA)
  abline(v=critical_z, lty=2)   # 임계값
  abline(v=z_stat, lty=3)       # 검정통계량
  lines(x, y, lwd=2)            # 밀도곡선 다시 그리기
}

# 문제 8. 검정 결과를 일상적인 언어로 작성
# 답: 표본 36개의 평균이 496mL로 나왔는데, 실제 평균이 500mL라면 이렇게
# 적게 나올 확률은 약 2.3%밖에 안 돼. 이 확률이 기준인 5%보다 작으니까
# 우연이라고 보기 어렵고, 이 음료의 실제 평균 용량은 500mL보다 작다고 판단할 수 있어.