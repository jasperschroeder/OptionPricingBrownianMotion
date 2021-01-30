# Brownian Motion
sigma = 0.1
T = 1
n = 1000
B = 0
BB1 = rep(0, n)
BB2 = rep(0, n)
BB3 = rep(0, n)
BB4 = rep(0, n)

B1 = 0
B2 = 0
B3 = 0
B4 = 0

for (i in 1:n) {
  B1 = B1 + sigma * sqrt(T/n)*rnorm(1)
  B2 = B2 + sigma * sqrt(T/n)*rnorm(1)
  B3 = B3 + sigma * sqrt(T/n)*rnorm(1)
  B4 = B4 + sigma * sqrt(T/n)*rnorm(1)
  BB1[i] = B1
  BB2[i] = B2
  BB3[i] = B3
  BB4[i] = B4
}

plot(BB1, type='l', ylim=c(-0.25, 0.25), main="Four Random Walk/Brownian Motion Approximations", ylab='B')
lines(BB2, type='l', col='red')
lines(BB3, type='l', col='blue')
lines(BB4, type='l', col='green')

# Geometric Brownian Motion
sigma = 0.1
T = 1
n = 1000
B = 0
r = 0.03

GBB1 = rep(0, n)
GBB2 = rep(0, n)
GBB3 = rep(0, n)
GBB4 = rep(0, n)

GB1 = 0
GB2 = 0
GB3 = 0
GB4 = 0

for (i in 1:n) {
  GB1 = GB1 + r * GB1 * (T/n) + sigma * sqrt(T/n)*rnorm(1)
  GB2 = GB2 + r * GB2 * (T/n) + sigma * sqrt(T/n)*rnorm(1)
  GB3 = GB3 + r * GB3 * (T/n) + sigma * sqrt(T/n)*rnorm(1)
  GB4 = GB4 + r * GB4 * (T/n) + sigma * sqrt(T/n)*rnorm(1)
  GBB1[i] = GB1
  GBB2[i] = GB2
  GBB3[i] = GB3
  GBB4[i] = GB4
}

plot(GBB1, type='l', ylim = c(min(c(GBB1, GBB2, GBB3, GBB4)), max(c(GBB1, GBB2, GBB3, GBB4))),
  main="Four Random Walk/ \nGeometric Brownian Motion Approximations", ylab='B')
lines(GBB2, type='l', col='red')
lines(GBB3, type='l', col='blue')
lines(GBB4, type='l', col='green')

