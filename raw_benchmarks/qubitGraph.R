# Fuction for overall model p-value
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Initialize
options(scipen=4, digits=3)
eqTable <- data.frame(MinQubit=integer(), Base=numeric(), Intercept=numeric(), RSqr=numeric(), BaseP=numeric(), InterceptP=numeric(), ModelP=numeric())

# Load data
testData <- t(read.csv(file="/home/iamu/qrack_benchmarks/qenginecpu/cland.csv", header=TRUE, sep=","))
firstTrendPoint <- 10

# Freely varied log base scaling:
modelY <- log(testData[2,firstTrendPoint:22])
modelX <- testData[1,firstTrendPoint:22]
exponential.model <- lm(modelY~modelX)
modelSummary<-summary(exponential.model)
modelSummary
coefs <- coef(exponential.model)

# Add to equations table:
eqRow <- data.frame(
  MinQubit=(firstTrendPoint + testData[1,1] - 1),
  Base=coefs[[2]],
  Intercept=(coefs[[1]] / coefs[[2]]),
  RSqr=modelSummary$r.squared,
  BaseP=modelSummary$coefficients[2,4],
  InterceptP=modelSummary$coefficients[1,4],
  ModelP=lmp(exponential.model)
)
row.names(eqRow)<-c('CLAND')
eqTable <- rbind(eqTable, eqRow)

qExpTrend <- exp(predict(exponential.model))
boxplot(testData[c(4,5,6,7,8),], log="y", names=testData[1,], main="X Time vs. No. of Qubits", xlab="No. of Qubits", ylab="Time (ms)")
lines(testData[1,firstTrendPoint:22] - testData[1,1] + 1, qExpTrend,lwd=2, col = "red", xlab = "No. of Qubits", ylab = "Time (ms)")
# coords <- locator()
text(x=coords$x, y=coords$y, labels=paste('t ≈ exp[', round(coefs[[2]], 2), '( n - ', -round(coefs[[1]] / coefs[[2]], 2), ')]'))

# Exact log(2) scaling fixed:
modelY <- (log(testData[2,firstTrendPoint:22]) - (log(2) * testData[1,firstTrendPoint:22]))
modelX <- rep(1, times=length(testData[1,firstTrendPoint:22]))
exponential.model <- lm(modelY~modelX)
summary(exponential.model)
coefs <- coef(exponential.model)

qExpTrend <- exp(predict(exponential.model) + (log(2) * testData[1,firstTrendPoint:22]))
boxplot(testData[c(4,5,6,7,8),], log="y", names=testData[1,], main="X Time vs. No. of Qubits", xlab="No. of Qubits", ylab="Time (ms)")
lines(testData[1,firstTrendPoint:22] - testData[1,1] + 1, qExpTrend,lwd=2, col = "red", xlab = "No. of Qubits", ylab = "Time (ms)")
# coords <- locator()
text(x=coords$x, y=coords$y, labels=paste('t ≈ 2^( n - ', -round(coefs[[1]] / log(2), 2), ')'))

orderedEqTable <- eqTable[order(Intercept, Base, MinQubit),]
