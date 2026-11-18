# 통계학입문 과제 1 — 정규분포 (R 학생용)
# 이름: 신승민  학번: 2026123239
# 목표: 정규분포의 표준화, 확률, 분위수, 확률밀도 그래프를 계산함
# 별도의 패키지 설치 없이 base R만 사용함

# [상황] 시험 점수 X는 평균 70점, 표준편차 10점의 정규분포를 따름
# X ~ N(70, 10^2)
# 문제 1
mu <- 70
sigma <- 10

# 문제 2
score <- 85
z_score <- (score - mu) / sigma

# 문제 3
p_above_80 <- pnorm(80, mean = mu, sd = sigma, lower.tail = FALSE)

# 문제 4
p_60_to_80 <- pnorm(80, mean = mu, sd = sigma) - pnorm(60, mean = mu, sd = sigma)

# 문제 5
cutoff_top_5 <- qnorm(0.95, mean = mu, sd = sigma)

cat('2. Z-score:', z_score, '\n')
cat('3. P(X >= 80):', p_above_80, '\n')
cat('4. P(60 <= X <= 80):', p_60_to_80, '\n')
cat('5. 상위 5% 기준점:', cutoff_top_5, '\n')

# 문제 6
if (!is.na(mu) && !is.na(sigma)) {
  x <- seq(40, 100, length.out=400)
  y <- dnorm(x, mean = mu, sd = sigma)
  if (!all(is.na(y))) {
    plot(x, y, type='l', lwd=2, main='Exam scores: Normal distribution',
         xlab='Score', ylab='Density')
    abline(v=mu, lty=2)
    legend('topright', legend=c('Normal density', 'Mean = 70'),
           lty=c(1, 2), bty='n')
  }
}


# [서술] 결과를 2~3문장으로 해석할 것
# 답: 평균이 70점, 표준편차가 10점인 정규분포에서 85점은 평균보다 표준편차의 1.5배만큼 높은 점수이다. 
# 전체의 약 68.27%가 60~80점 구간에 분포하고 80점 이상일 확률은 약 15.87%이며, 상위 5%에 들기 위한 기준 점수는 약 86.45점이다.