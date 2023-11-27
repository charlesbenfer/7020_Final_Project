plot(FPD...All.Hitting$SBS, col = as.factor(FPD...All.Hitting$Year), main = 'Stolen Bases', ylab = 'Number of Stolen Bases', pch = 18)
legend(x = 60, y=180, legend = c('2023', '2022', '2021'), pch = 18  , col = c(3,2,1))


boxplot(FPD...All.Hitting$SBS~as.factor(FPD...All.Hitting$Year), xlab = 'Season'
        , ylab = 'Stolen Bases')
abline(sbs)
sbs <- lm(SBS ~ Year, data = FPD...All.Hitting)
summary(sbs)
plot(sbs)
anova(sbs)

boxplot(FPD...All.Pitching$W9 ~ as.factor(FPD...All.Pitching$Year))

