#preliminary data analysis
summary(Stat.7020.Final.Project.Data.Set...All.Hitting)

#are all predictors linear with response?
plot(Win_Percent~Runs, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~Hits, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~Doubles, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~Triples, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~Homeruns, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~RBIS, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~SBS, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~BBS, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~SOS, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~BA, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~OBP, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~SLG, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~OPS, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~OPS_PLUS, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(Win_Percent~TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)

#boxplots
boxplot(Win_Percent~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(Runs~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(Hits~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(Doubles~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(Triples~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(Homeruns~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(RBIS~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(SBS~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(BBS~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(SOS~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(BA~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(OBP~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(SLG~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(OPS~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(OPS_PLUS~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
boxplot(TB~League, data = Stat.7020.Final.Project.Data.Set...All.Hitting)

#interesting single variable models

tb <- lm(Win_Percent ~ TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
summary(tb)
plot(tb)

#########################################

#Model Selection

#Forwards (OBP and SLG)

hitting_forwards_nops <- lm(Win_Percent ~ Runs + Hits + Doubles + Triples + Homeruns + 
              RBIS + SBS + BBS + SOS+ BA + OBP + SLG + OPS_PLUS +
              TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
hfnops <- ols_step_forward_p(hitting_forwards_nops, details = TRUE, penter = .05)
plot(hfnops)

#Forwards (OPS)

hitting_forwards_ops <- lm(Win_Percent ~ Runs + Hits + Doubles + Triples + Homeruns + 
                             RBIS + SBS + BBS + SOS+ BA + OPS + OPS_PLUS +
                             TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
hfops <- ols_step_forward_p(hitting_forwards_ops, details = TRUE, penter = .05)
plot(hfops)

#model selection here is tricky, must revisit


#Backwards (OBP and SLG)

hitting_backwards_nops <- lm(Win_Percent ~ Runs + Hits + Doubles + Triples + Homeruns + 
                          RBIS + SBS + BBS + SOS+ BA + OBP + SLG + OPS_PLUS +
                          TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
hbnops <- ols_step_backward_p(hitting_backwards_nops, details = TRUE, prem = .051)
plot(hbnops)

#Backwards (OPS)

hitting_backwards_ops <- lm(Win_Percent ~ Runs + Hits + Doubles + Triples + Homeruns + 
                               RBIS + SBS + BBS + SOS+ BA + OPS + OPS_PLUS +
                               TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
hbops <- ols_step_backward_p(hitting_backwards_ops, details = TRUE, prem = .051)
plot(hbops)

h_chosen_backward <- lm(Win_Percent ~ Runs + Hits + Doubles + 
                        Triples + Homeruns + BA + OPS_PLUS +
                        TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(h_chosen_backward)

#Stepwise (OBP and SLG)
hitting_stepwise_nops <- lm(Win_Percent ~ Runs + Hits + Doubles + Triples + Homeruns + 
                               RBIS + SBS + BBS + SOS+ BA + OBP + SLG + OPS_PLUS +
                               TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
hsnops <- ols_step_both_p(hitting_stepwise_nops, details = TRUE, penter = .05, prem = .051)
plot(hsnops)

hitting_stepwise_ops <- lm(Win_Percent ~ Runs + Hits + Doubles + Triples + Homeruns + 
                              RBIS + SBS + BBS + SOS+ BA + OPS + OPS_PLUS +
                              TB, data = Stat.7020.Final.Project.Data.Set...All.Hitting)
hsops <- ols_step_both_p(hitting_stepwise_ops, details = TRUE, penter = .05, prem = .051)
plot(hsops)

h_chosen_stepwise <- lm(Win_Percent ~ OPS_PLUS + Runs + TB, 
                        data = Stat.7020.Final.Project.Data.Set...All.Hitting)
plot(h_chosen_stepwise)




