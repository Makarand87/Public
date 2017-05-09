setwd(
  "/\\shares\\shares\\Business Analytics\\Internal Analytics\\Medical Coding\\CD03 - Hypothesis Testing\\Functional Document"
)

inddata <- read.csv("WORK_QUERY_FOR_PARALLONIPR1.csv")
str(inddata)
inddata2 <- subset(inddata, Q2 < 100)

inddata2$hours <- as.factor(inddata2$hour)
inddata2$hours <- relevel(inddata2$hours, "8")

library(ggplot2)
p <- ggplot(inddata2, aes(Freq, Q2, color=as.character(hours)))
p + geom_point(size=1)

p <- ggplot(inddata2, aes(Freq, Q2, color=as.character(hours)))
p <- p + geom_line(size=1)
p + facet_grid(hours ~ .)

set.seed(1)
fit1 <- lm(Q2 ~ Freq, data = inddata2)
summary(fit1)

set.seed(1)
fit2 <- lm(Q2 ~ hours, data = inddata2)
summary(fit2)

set.seed(1)
fit3 <- lm(Q2 ~ Freq + hours, data = inddata2)
summary(fit3)

X <- model.matrix(fit3)
X[,2] <- mean(inddata2$Freq)
adj <- X %*%coef(fit3)
data.frame(obs = tapply(inddata2$Q2,inddata2$hour, mean),
           adj = tapply(adj,   inddata2$hour, mean))



inddata2$Freq.c <- inddata2$Freq - mean(inddata2$Freq)
fit4 <- lm(Q2 ~ Freq.c * hours, data = inddata2)
summary(fit4)
anova(fit4)

X <- model.matrix(fit4)
X[,2] <- mean(inddata2$Freq)
adj <- X %*%coef(fit4)
data.frame(obs = tapply(inddata2$Q2,inddata2$hour, mean),
           Quality_hour = tapply(adj,   inddata2$hour, mean))


xy <- cbind(inddata2$Freq, fitted(fit4))
plot(inddata2$Freq, inddata2$Q2, xlim=c(0,80),type="n", 
     xlab="Freq of Coding", ylab="Quality", main="Effects of workhour on Quality")
text(jitter(inddata2$Freq,18), jitter(inddata2$Q2,18), labels=inddata2$hours, 
     col = inddata2$hour, cex=0.75)
lines(xy[inddata2$hour=="7",], col=2, lty = 3, lwd=2)
lines(xy[inddata2$hour=="18",], col=13, lty = 3, lwd=2)
lines(xy[inddata2$hour=="19",], col=14, lty = 3, lwd=2)
lines(xy[inddata2$hour=="8",], col=3)
lines(xy[inddata2$hour=="9",], col=4)
lines(xy[inddata2$hour=="10",], col=5)
lines(xy[inddata2$hour=="11",], col=6)
lines(xy[inddata2$hour=="12",], col=7)
lines(xy[inddata2$hour=="13",], col=8)
lines(xy[inddata2$hour=="14",], col=9)
lines(xy[inddata2$hour=="15",], col=10)
lines(xy[inddata2$hour=="16",], col=11)
lines(xy[inddata2$hour=="17",], col=12)

legend("bottomright", legend = c("7", "18", "19"), fill = c(2,13,14), bty = "n", cex = 0.75)

dev.print(png,"fig25.png",width=1024,height=800)
