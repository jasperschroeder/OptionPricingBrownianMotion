# Option Pricing by use of Monte Carlo Simulations

k = 10000   # 10000 sample paths
n = 25000   # 25000 steps
T = 2       # 2 Years Horizon
r = 0.04    # 4% risk-free rate of interest
sigma = 0.3 # 30% volatility
S = 25      # Initial stock price 
K = 20      # Strike Price

ST = rep(0, k)
Sts = rep(0, n)

for (i in 1:k) {
  S = 25
  for (j in 1:n) {
    S = S + r * S * (T/n) + sigma * S * sqrt(T/n)*rnorm(1)
  }
  ST[i] = S
}

payoffs <- rep(0, length(ST))
for (i in 1:length(ST)) {
  payoffs[i] = max(ST[i]-K,0)
}

exp(-r*T)*mean(payoffs)
BlackScholesModelEC(25, 2, 20, 0.04, 0.3)
