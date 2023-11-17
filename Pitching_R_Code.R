#preliminary data analysis
summary(Stat.7020.Final.Project.Data.Set...All.Pitching)

#Histograms
hist(Stat.7020.Final.Project.Data.Set...All.Pitching$ERA)
hist(Stat.7020.Final.Project.Data.Set...All.Pitching$Win_Percent)
hist(Stat.7020.Final.Project.Data.Set...All.Pitching$SV)
hist(Stat.7020.Final.Project.Data.Set...All.Pitching$H)
       
       
#are all predictors linear with response?
plot(Win_Percent~ERA, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~CG, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~SHO, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~SV, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~H, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~R, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~ER, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~HR, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~BB, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~SO, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~ERA_PLUS, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~FIP, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~WHIP, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~H9, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~W9, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(Win_Percent~SO9, data = Stat.7020.Final.Project.Data.Set...All.Pitching)

plot(Stat.7020.Final.Project.Data.Set...All.Pitching)

#boxplots
boxplot(Win_Percent~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(ERA~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(CG~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(SHO~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(SV~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(H~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(R~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(ER~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(HR~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(BB~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(SO~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(ERA_PLUS~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(FIP~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(WHIP~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(H9~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(W9~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)
boxplot(SO9~as.factor(League), data = Stat.7020.Final.Project.Data.Set...All.Pitching)

#Interesting single variable models
era_plus <- lm(Win_Percent ~ ERA_PLUS, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
summary(era_plus)

era <- lm(Win_Percent ~ ERA, data = Stat.7020.Final.Project.Data.Set...All.Pitching)
summary(era)


###########
#Model Selection

#Forwards

pitching_forwards <- lm(Win_Percent ~ ERA + SV + H + R + ER + HR + BB + SO + 
               ERA_PLUS + FIP + WHIP + H9 + W9 + SO9
             , data = Stat.7020.Final.Project.Data.Set...All.Pitching)
pf <- ols_step_forward_p(pitching_forwards, details = TRUE, penter = .05)
plot(pf)

p_chosen_forward <- lm(Win_Percent ~ ERA_PLUS + SV + R + ER + W9, 
                     data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(p_chosen_forward)

#Backwards

pitching_backwards <- lm(Win_Percent ~ ERA + SV + H + R + ER + HR + BB + SO + 
                           ERA_PLUS + FIP + WHIP + H9 + W9 + SO9
                         , data = Stat.7020.Final.Project.Data.Set...All.Pitching)
pb <- ols_step_backward_p(pitching_backwards, details = TRUE, prem = .051)
plot(pb)

p_chosen_backward <- lm(Win_Percent ~ ERA + SV +R + ER + 
                        ERA_PLUS + WHIP + H9 + SO9
                      , data = Stat.7020.Final.Project.Data.Set...All.Pitching)

plot(p_chosen_backward)

#Stepwise

pitching_stepwise <- lm(Win_Percent ~ ERA + SV + H + R + ER + HR + BB + SO + 
                           ERA_PLUS + FIP + WHIP + H9 + W9 + SO9
                         , data = Stat.7020.Final.Project.Data.Set...All.Pitching)
ps <- ols_step_both_p(pitching_stepwise, details = TRUE, penter = .05, prem = .05)
plot(ps)


p_chosen_stepwise <- lm(Win_Percent ~  ERA_PLUS + SV + R + ER + W9 + SO
                      , data = Stat.7020.Final.Project.Data.Set...All.Pitching)
plot(p_chosen_stepwise)

#################################

#Analyzing the Selected Models

#multicollinearity

vif(p_chosen_forward)
#Runs and Earned Runs are MASSIVE, ERA_PLUS is high

vif(p_chosen_backward)
#ERA, Runs, Earned Runs, ERA_PLUS, WHIP, and H9 all too high

vif(p_chosen_stepwise)
#ERA_PLUS, R, ER too high

####################################

#addressing Multicollinearity

#In Forward Model:

#build model with no Runs
p_forward_no_runs <- lm(Win_Percent ~ ERA_PLUS + SV  + ER + W9, 
                        data = Stat.7020.Final.Project.Data.Set...All.Pitching)

summary(p_forward_no_runs)
#everything significant but ER, R^2 adj .81 great

plot(p_forward_no_runs)
#residual plots still look okay

vif(p_forward_no_runs)
#earned runs too high still

#build model with no earned runs
p_forward_no_er <- lm(Win_Percent ~ ERA_PLUS + SV  + R + W9, 
                      data = Stat.7020.Final.Project.Data.Set...All.Pitching)
summary(p_forward_no_er)
#everything significant (W9 at .1 significance) and R^2 adj of .82

plot(p_forward_no_er)
#residuals still look okay

vif(p_forward_no_er)
#these look close enough, this will be the picked forward model

#In Backward Model:

#build model with no ER

p_backward_no_er <- lm(Win_Percent ~ ERA + SV + R + 
                         ERA_PLUS + WHIP + H9 + SO9
                       , data = Stat.7020.Final.Project.Data.Set...All.Pitching)

summary(p_backward_no_er)
#everything significant (SO9 at .1), R^@ adj at .85!
plot(p_backward_no_er)
#residuals look great
vif(p_backward_no_er)
#ERA still way too high

p_backward_no_er_era <- lm(Win_Percent ~ SV + R + 
                             ERA_PLUS + WHIP + H9 + SO9
                           , data = Stat.7020.Final.Project.Data.Set...All.Pitching)
summary(p_backward_no_er_era)
#whip no longer significant , SO9 still .1
plot(p_backw)