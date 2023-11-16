#preliminary data analysis
summary(Stat.7020.Final.Project.Data.Set...2023.Pitching)

#are all predictors linear with response?
plot(Win_Percent~ERA, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~CG, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~SHO, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~SV, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~H, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~R, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~ER, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~HR, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~BB, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~SO, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~ERA_PLUS, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~FIP, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~WHIP, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~H9, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~W9, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
plot(Win_Percent~SO9, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)

plot(Stat.7020.Final.Project.Data.Set...2023.Pitching)

#boxplots
boxplot(Win_Percent~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(ERA~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(CG~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(SHO~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(SV~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(H~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(R~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(ER~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(HR~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(BB~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(SO~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(ERA_PLUS~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(FIP~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(WHIP~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(H9~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(W9~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
boxplot(SO9~as.factor(League), data = Stat.7020.Final.Project.Data.Set...2023.Pitching)

#Interesting single variable models
era_plus <- lm(Win_Percent ~ ERA_PLUS, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
summary(era_plus)

era <- lm(Win_Percent ~ ERA, data = Stat.7020.Final.Project.Data.Set...2023.Pitching)
summary(era)
