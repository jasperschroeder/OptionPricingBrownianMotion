# Derivative Pricing: Black-Scholes Model
# Non-dividend paying stocks!

BlackScholesModelEC <- function(S, T, K, r, sig) {
  d1 = (log(S/K) + (r + (sig^2/2))*T)/(sig*sqrt(T))
  d2 = d1 - sig*sqrt(T)
  EC = S * pnorm(d1) - K * exp(-r*T) * pnorm(d2)
  return (EC)
}

BlackScholesModelEP <- function(S, T, K, r, sig) {
  d1 = (log(S/K) + (r + (sig^2/2))*T)/(sig*sqrt(T))
  d2 = d1 - sig*sqrt(T)
  EP = -S * pnorm(-d1) + K * exp(-r*T)*pnorm(-d2)
  return (EP)
}

# Example I
EC = BlackScholesModelEC(20, 1, 15, 0.05, 0.2)
EP = BlackScholesModelEP(20, 1, 15, 0.05, 0.2)

# Does the Put-Call Parity hold? 
# EC(K,T) + PV(K) = EP(K,T) + S
all.equal(EC + exp(-0.05*1)* 15, EP + 20)
# Parity!

# Example II
EC = BlackScholesModelEC(275, 1, 150, 0.045, 0.1)
EP = BlackScholesModelEP(275, 1, 150, 0.045, 0.1)
# Does the Put-Call Parity hold here?
all.equal(EC + exp(-0.045*1)*150, EP + 275)
# Parity!

# How do paramter values affect the option Price? 
# Example: S= 100, T = 1, K = 95, r = 0.03, sig = 0.3
EC = BlackScholesModelEC(100, 1, 95, 0.03, 0.3)
EP = BlackScholesModelEP(100, 1, 95, 0.03, 0.3)
EC
EP

par(mfrow=c(2,3))

# 1: Stock Price
Sx <- seq(50, 150, 0.1)
plot(Sx, BlackScholesModelEC(Sx, 1, 95, 0.03, 0.3), 
     type='l', main='Option Price \nvs. Stock Price', xlab='Stock Price', ylab='Option Price')
lines(Sx, BlackScholesModelEP(Sx, 1, 95, 0.03, 0.3), col='red')
legend(55, 60, legend=c("EC", "EP"), col=c("black", "red"), lty=c(1,1))

# 2: Time to Maturity
Tx <- seq(0, 5, 0.01)
plot(Tx, BlackScholesModelEC(100, Tx, 95, 0.03, 0.3), 
     type='l', main='Option Price \nvs. Time to Maturity', xlab='Time to Maturity', ylab='Option Price')
lines(Tx, BlackScholesModelEP(100, Tx, 95, 0.03, 0.3), col='red')
legend(0.05, 35, legend=c("EC", "EP"), col=c("black", "red"), lty=c(1,1))

# 3: Strike Price
Kx <- seq(75, 125, 0.01)
plot(Kx, BlackScholesModelEC(100, 1, Kx, 0.03, 0.3), 
     type='l', main='Option Price \nvs. Strike Price', xlab='Strike Price', ylab='Option Price')
lines(Kx, BlackScholesModelEP(100, 1, Kx, 0.03, 0.3), col='red')
legend(80, 29.5, legend=c("EC", "EP"), col=c("black", "red"), lty=c(1,1))

# 4: Risk-Free rate of interest
rx <- seq(-0.1, 0.25, 0.01)
plot(rx, BlackScholesModelEC(100, 1, 95, rx, 0.3), 
     type='l', main='Option Price \nvs. Risk-free rate of interest', xlab='Risk-free rate of interest', 
     ylab='Option Price', xlim=c(-0.1, 0.25), ylim=c(0, 30))
lines(rx, BlackScholesModelEP(100, 1, 95, rx, 0.3), col='red')
legend(-0.085, 29.5, legend=c("EC", "EP"), col=c("black", "red"), lty=c(1,1))

# 5: Volatility
sigx <- seq(0, 0.5, 0.01)
plot(sigx, BlackScholesModelEC(100, 1, 95, 0.03, sigx), 
     type='l', main='Option Price \nvs. Volatility', xlab='Volatility', 
     ylab='Option Price', ylim = c(0, 25))
lines(sigx, BlackScholesModelEP(100, 1, 95, 0.03, sigx), col='red')
legend(0.015, 24.5, legend=c("EC", "EP"), col=c("black", "red"), lty=c(1,1))











