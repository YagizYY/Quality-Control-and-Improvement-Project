library(data.table)
library(statsr)
library(ggplot2)
c <- seq(5,500,5)
ns <- seq(100,10000,10)
c_rep <- rep(c, length(ns))
c_sort <- sort(c_rep)

ns_rep <- rep(ns,length(c))
# ns_rep_sort <- sort(ns_rep)

my_table <- data.table(c = c_sort, n=ns_rep)
my_table <- my_table[n>=c]

my_table[,P_accept_good := pbinom(c,n,0.03)]
my_table[,P_accept_fair := pbinom(c,n,0.07)]
my_table[,P_accept_bad := pbinom(c,n,0.1)]

my_table[, alpha := 1-P_accept_good]
my_table[, beta := P_accept_bad]

my_table[,sampling_cost := n/10]

my_table[,E_profit_good := P_accept_good*1000]
my_table[, E_profit_fair := P_accept_fair*500]
my_table[, E_cost_bad := P_accept_bad*2500]

my_table[, ProfitPerBatch := E_profit_good*0.3+ E_profit_fair*0.4-
           E_cost_bad*0.3]

my_table[, TotalProfit := ProfitPerBatch - sampling_cost]

c_and_n <- my_table[,.(TotalProfit=max(TotalProfit)),.(c)]

setkey(c_and_n,c,TotalProfit)
setkey(my_table,c,TotalProfit)

merged_table <- my_table[c_and_n]
merged_table <- merged_table[,.(c,n,TotalProfit)]

my_table <- my_table[order(c,n)]
# fwrite(my_table,"sampling.csv")

merged_table <- merged_table[order(c,n)]
# fwrite(merged_table, "c_n_TotalProfit_sampling.csv")

# c vs TotalProfit
ggplot(merged_table)+geom_line(aes(x=c,y=TotalProfit)) + 
  geom_line(aes(x=c,y=max(TotalProfit)),linetype=1,colour="red") +
  geom_text(aes(0,y=max(TotalProfit)),
                label=paste("Maximum Profit:",
                ceiling(max(merged_table[,TotalProfit]))),hjust=-1.5,vjust=1)

# n vs TotalProfit
ggplot(merged_table)+geom_line(aes(x=n,y=TotalProfit)) + 
  geom_line(aes(x=n,y=max(TotalProfit)),linetype=1,colour="red") +
  geom_text(aes(0,y=max(TotalProfit)),
            label=paste("Maximum Profit:",
            ceiling(max(merged_table[,TotalProfit]))),hjust=-1.5,vjust=1)



ggplot(my_table[c==30])+geom_line(aes(x=n,y=TotalProfit))

## TESTING & SAMPLING ##

my_testsamp <- data.table(c = c_sort, n=ns_rep)
my_testsamp <- my_testsamp[n>=c]

my_testsamp[,P_accept_good := pbinom(c,n,0.03)]
my_testsamp[,P_accept_fair := pbinom(c,n,0.07)]
my_testsamp[,P_accept_bad := pbinom(c,n,0.1)]

my_testsamp[, alpha := 1-P_accept_good]
my_testsamp[, beta := P_accept_bad]

my_testsamp[,sampling_cost := n/10]


my_testsamp[,E_profit_good := P_accept_good*1000]
my_testsamp[, E_profit_fair := P_accept_fair*500]
my_testsamp[, E_cost_bad := P_accept_bad*500]

my_testsamp[, ProfitPerBatch := E_profit_good*0.3+E_profit_fair*0.4-
           E_cost_bad*0.3]

my_testsamp[, TotalProfit := ProfitPerBatch - sampling_cost - 200]

c_and_n <- my_testsamp[,.(TotalProfit=max(TotalProfit)),.(c)]

setkey(c_and_n,c,TotalProfit)
setkey(my_testsamp,c,TotalProfit)

merged_testsamp <- my_testsamp[c_and_n]
merged_testsamp <- merged_testsamp[,.(c,n,TotalProfit)]
merged_testsamp <- merged_testsamp[order(c)]

my_testsamp <- my_testsamp[order(c,n)]
# fwrite(my_testsamp,"sampling_and_testing.csv")

merged_testsamp <- merged_testsamp[order(c,n)]
# fwrite(merged_testsamp, "c_n_TotalProfit_sampling_and_testing.csv")


# c vs TotalProfit
ggplot(merged_testsamp)+geom_line(aes(x=c, y=TotalProfit)) + 
  geom_line(aes(x=c,y=max(TotalProfit)),linetype=1, colour="red") +
  geom_text(aes(0,y=max(TotalProfit)),
            label=paste("Maximum Profit:",
            ceiling(max(merged_testsamp[,TotalProfit]))),hjust=-1.5,vjust=1)

# n vs TotalProfit
ggplot(merged_testsamp)+geom_line(aes(x=n, y=TotalProfit)) + 
  geom_line(aes(x=n,y=max(TotalProfit)),linetype=1, colour="red") +
  geom_text(aes(0,y=max(TotalProfit)),
            label=paste("Maximum Profit:",
            ceiling(max(merged_testsamp[,TotalProfit]))),hjust=-1.5,vjust=1)

# x[, c1 := qnorm(p=alpha,mean = n*p1, sd = sqrt(p1*(1-p1)*n))]
# 
# x[, c2 := qnorm(beta,mean=n*p2,sd=sqrt(n*p2*(1-p2)))]
# a <- data.table(alphe = rep(0.95,8), bete  = rep(0.01,8), p1  = rep(0.02,8),
#                 p2  = rep(0.05,8), nam  = c(50,100,200,300,400,500,550,570)
# )
# 
# a[, c1 := qnorm(p=alphe,mean = nam*p1, sd = sqrt(p1*(1-p1)*nam))]
# a[, c2 := qnorm(p=bete,mean = nam*p2, sd = sqrt(p2*(1-p2)*nam))]
# a[,difference := c1-c2]

#########################################################################
