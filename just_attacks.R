library(rugarch)
library(openxlsx)
library(PerformanceAnalytics)
library(ggplot2)
library(VIM)
library(longtable)
library(xts)
library(xtable)

df = read.xlsx('C:/Users/Florentina/Desktop/attacks/computation/btc_volat1.xlsx' , sheet='btc')
attacks = read.xlsx('C:/Users/Florentina/Desktop/attacks/computation/btc_volat1.xlsx' , sheet='events')

#df = read.xlsx('C:/Users/User/Desktop/attacks/hacks/attacks_file.xlsx' , sheet='BTC')
#attacks = read.xlsx('C:/Users/User/Desktop/attacks/hacks/attacks_file.xlsx' , sheet='attacks')


df$Date = as.Date(df$Date, origin= "1899-12-30") 
df = df[c('Date', 'close')]
attacks$Date = as.Date(attacks$Date, origin= "1899-12-30") 

df = xts(df$close, order.by=df$Date)

rt = Return.calculate(df, method="log")
rt = rt[2:length(rt)]



# checking monthly volatility relationship with attacks nb 

library(xts)

calcVol <- function(x){
  ndays <- nrow(x)
  first_part_of_formula <- sum(x^2)
  second_part_of_formula <- 2*sum(x[-1]*x[-nrow(x)])
  res <- sqrt(first_part_of_formula + second_part_of_formula)
  return(res)
}

month_vol = apply.monthly(rt, calcVol)
barplot(month_vol)

new_attacks = xts(attacks['Amount'], order.by = attacks$Date )


month_number = apply.monthly(new_attacks, length)
barplot(month_number)


all_df = merge(month_vol, month_number)

to_go_through = split(all_df)

my_dates = list() 
my_vol = list()
my_attack = list()
my_number = list()
c = 1
for(i in to_go_through){
  if(length(i)!=2){
    my_dates[[c]] = index(i)[1]
    my_vol[[c]] = as.numeric(na.omit(i$month_vol))
    my_number[[c]] = as.numeric(na.omit(i$Amount))
    c = c + 1
  }
}



month_df = data.frame(dates= do.call("c", my_dates), vol = unlist(my_vol), number = unlist(my_number))

reg = lm(vol~ number, data= month_df)
summary(reg)

barplot(rbind(month_df$vol*10, month_df$number), beside = T, names.arg = as.Date(month_df$dates, origin= "1899-12-30"))

cor(month_df[,2:3])


# checking monthly volatility relationship with attacks amount

new_attacks = xts(attacks['Amount'], order.by = attacks$Date )
month_attacks = apply.monthly(new_attacks, sum)
barplot(month_attacks)


all_df = merge(month_vol, month_attacks)


to_go_through = split(all_df)

my_dates = list() 
my_vol = list()
my_attack = list()
my_number = list()
c = 1
for(i in to_go_through){
  if(length(i)!=2){
    my_dates[[c]] = index(i)[1]
    my_vol[[c]] = as.numeric(na.omit(i$month_vol))
    my_attack[[c]] = as.numeric(na.omit(i$Amount))
    my_attack[[c]] = as.numeric(na.omit(i$Amount))
    c = c + 1
  }
}


month_df = data.frame(dates= do.call("c", my_dates), vol = unlist(my_vol), amount = unlist(my_attack))

reg = lm(vol~ amount, data= month_df)
summary(reg)

barplot(rbind(month_df$vol*10, month_df$amount), beside = T, names.arg = as.Date(month_df$dates, origin= "1899-12-30"))

cor(month_df[,2:3])




ggplot(month_df, aes(x=vol, y=number)) +
  geom_point(aes(size=vol)) + geom_smooth(method=lm)

write.xlsx(month_df, file = "results_Tina.xlsx", sheetName = "Vol/Amount", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


res1=read.xlsx('C:/Users/Florentina/Desktop/attacks/computation/results_Tina_Amount.xlsx' , sheet='amount')
res2=read.xlsx('C:/Users/Florentina/Desktop/attacks/computation/results_Tina_Number.xlsx' , sheet='nb')

barplot(rbind(res1$vol*10, log(res1$attack)), beside = T, names.arg = res1$merge)

barplot(rbind(res2$vol*10, res2$number), beside = T, names.arg = res1$merge)

#correlation significance level

cor1<-cor.test(res1$vol,res1$attack, method="pearson",  use = "complete.obs")
cor1

cor2<-cor.test(res2$vol,res2$number, method="pearson",  use = "complete.obs")
cor2

tau1<-cor.test(res1$vol,res1$attack, method="kendall",  use = "complete.obs")
tau1

tau2<-cor.test(res2$vol,res2$number, method="kendall",  use = "complete.obs")
tau2

rho1<-cor.test(res1$vol,res1$attack, method="spearman")
rho1

rho2<-cor.test(res2$vol,res2$number, method="spearman")
rho2

 

sample=read.xlsx('C:/Users/Florentina/Desktop/latex.xlsx' , sheet='aattacks')
sample$Date = as.Date(sample$Date, origin= "1899-12-30") 

print(xtable(sample, type = "latex", tabular.environment="longtable"), file = "sample.tex")

