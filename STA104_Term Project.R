# Appendix Code

set.seed(777)
library(ggplot2)
library("readxl")
hollywoodmovies = read_excel("C:/Users/pumad/Downloads/hollywoodmovies.xlsx")
# Problem 1


#(a)
##center of distribution and 95% C.I.
##For budget, domestic gross product, opening weekend
movies = hollywoodmovies

## Budget

## Histogram
par(mfrow=c(1,3))
hist(movies$Budget, breaks = 40, freq = FALSE, main = "Histogram of Budget", xlab = "Budget(in millions)", col = "lightblue")
lines(density(movies$Budget), col = "red", lwd = 2)
hist(movies$DomesticGross, breaks = 40, freq = FALSE, main = "Histogram of Domestic Gross", xlab = "DomesticGross(in millions)", col = "lightblue")
lines(density(movies$DomesticGross), col = "red", lwd = 2)
hist(movies$OpeningWeekend, breaks = 40, freq = FALSE, main = "Histogram of Opening Weekend", xlab = "OpeningWeekend(in millions)", col = "lightblue")
lines(density(movies$OpeningWeekend), col = "red", lwd = 2)



mean(movies$Budget)
t.test(movies$Budget)

## Domestic gross product

mean(movies$DomesticGross)
t.test(movies$DomesticGross)

## Opening weekend

mean(movies$OpeningWeekend)
t.test(movies$OpeningWeekend)


#(b)
## Median and Median confidence intervals

## Budget

median(movies$Budget)


x = movies$Budget
n = length(x)
q = 0.5
j = n*q - 1.96*sqrt(n*q*(1-q)) 
k = n*q + 1.96*sqrt(n*q*(1-q))

sort(x)[round(j)]
sort(x)[round(k)]
# Thus, the 95% median CI for budget is (32.5, 42)

## Domestic gross product

median(movies$DomesticGross)


x = movies$DomesticGross
n = length(x)
q = 0.5
j = n*q - 1.96*sqrt(n*q*(1-q)) 
k = n*q + 1.96*sqrt(n*q*(1-q))

sort(x)[round(j)]
sort(x)[round(k)]
# Thus, the 95% median CI for DomesticGross is (36.392, 55.1)

## Opening weekend

median(movies$OpeningWeekend)


x = movies$OpeningWeekend
n = length(x)
q = 0.5
j = n*q - 1.96*sqrt(n*q*(1-q)) 
k = n*q + 1.96*sqrt(n*q*(1-q))

sort(x)[round(j)]
sort(x)[round(k)]
# Thus, the 95% median CI for Opening weekend is (12.05, 18.445)


# Problem 2

## Budget
Bud_comedy = movies$Budget[movies$Genre=='Comedy']
Bud_dramas = movies$Budget[movies$Genre=='Drama']

## two sample t-test
t.test(Bud_comedy, Bud_dramas)

## Non-parametric(two sample permutation test)

outcome = c(Bud_comedy, Bud_dramas)
b = 10000
treat = c(rep(1,length(Bud_comedy)), rep(2,length(Bud_dramas)))
diffobs = mean(Bud_comedy)-mean(Bud_dramas)
d=c()
p=c()
for( i in 1:b){
  permut=sample(outcome)
  d[i]=mean(permut[treat==1])-mean(permut[treat==2])
  p[i]=(d[i]>=diffobs)+0
}
pvalue = sum(p)/b
pvalue


## ggpplot
gg_Bud = ggplot() + 
  stat_qq(aes(sample = Bud_comedy), colour = "darkgreen") + 
  stat_qq(aes(sample = Bud_dramas), colour = "red") +
  geom_abline(aes(intercept = mean(Bud_comedy), slope = sd(Bud_comedy)), color = "darkgreen") +
  geom_abline(aes(intercept = mean(Bud_dramas), slope = sd(Bud_dramas)), color = "red")




## Domestic gross

Dom_comedy = movies$DomesticGross[movies$Genre=='Comedy']
Dom_drama = movies$DomesticGross[movies$Genre=='Drama']

## two sample t-test
t.test(Dom_comedy, Dom_drama)

## Non-parametric(two sample permutation test)

outcome = c(Dom_comedy, Dom_drama)
b = 10000
treat = c(rep(1,length(Dom_comedy)), rep(2,length(Dom_drama)))
diffobs = mean(Dom_comedy)-mean(Dom_drama)
d=c()
p=c()
for( i in 1:b){
  permut=sample(outcome)
  d[i]=mean(permut[treat==1])-mean(permut[treat==2])
  p[i]=(d[i]>=diffobs)+0
}
pvalue = sum(p)/b
pvalue


gg_Dom = ggplot() + 
  stat_qq(aes(sample = Dom_comedy), colour = "darkgreen") + 
  stat_qq(aes(sample = Dom_drama), colour = "red") +
  geom_abline(aes(intercept = mean(Dom_comedy), slope = sd(Dom_comedy)), color = "darkgreen") +
  geom_abline(aes(intercept = mean(Dom_drama), slope = sd(Dom_drama)), color = "red")


## Opening weekend

Open_comedy = movies$OpeningWeekend[movies$Genre=='Comedy']
Open_drama = movies$OpeningWeekend[movies$Genre=='Drama']

## two sample t-test
t.test(Open_comedy, Open_drama)

## Non-parametric(two sample permutation test)

outcome = c(Open_comedy, Open_drama)
b = 10000
treat = c(rep(1,length(Open_comedy)), rep(2,length(Open_drama)))
diffobs = mean(Open_comedy)-mean(Open_drama)
d=c()
p=c()
for( i in 1:b){
  permut=sample(outcome)
  d[i]=mean(permut[treat==1])-mean(permut[treat==2])
  p[i]=(d[i]>=diffobs)+0
}
pvalue = sum(p)/b
pvalue


gg_Op = ggplot() + 
  stat_qq(aes(sample = Open_comedy), colour = "darkgreen") + 
  stat_qq(aes(sample = Open_drama), colour = "red") +
  geom_abline(aes(intercept = mean(Open_comedy), slope = sd(Open_comedy)), color = "darkgreen") +
  geom_abline(aes(intercept = mean(Open_drama), slope = sd(Open_drama)), color = "red")



ggarrange(gg_Bud, gg_Dom, gg_Op + rremove("x.text"), 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


# Problem 3

## Group A => horror/thriller
## Group B => animation/fantasy/romance
## Group C => action/adventure

GroupA = c(movies$Budget[movies$Genre=="Horror"], movies$Budget[movies$Genre=="Thriller"],
           movies$DomesticGross[movies$Genre=="Horror"], movies$DomesticGross[movies$Genre=="Thriller"],
           movies$OpeningWeekend[movies$Genre=="Horror"], movies$OpeningWeekend[movies$Genre=="Thriller"])

GroupB = c(movies$Budget[movies$Genre=="Animation"], movies$Budget[movies$Genre=="Fantasy"], movies$Budget[movies$Genre=="Romance"],
           movies$DomesticGross[movies$Genre=="Animation"], movies$DomesticGross[movies$Genre=="Fantasy"], movies$DomesticGross[movies$Genre=="Romance"],
           movies$OpeningWeekend[movies$Genre=="Animation"], movies$OpeningWeekend[movies$Genre=="Fantasy"], movies$OpeningWeekend[movies$Genre=="Romance"])

GroupC = c(movies$Budget[movies$Genre=="Action"], movies$Budget[movies$Genre=="Adventure"],
           movies$DomesticGross[movies$Genre=="Action"], movies$DomesticGross[movies$Genre=="Adventure"],
           movies$OpeningWeekend[movies$Genre=="Action"], movies$OpeningWeekend[movies$Genre=="Adventure"])

count = c(GroupA, GroupB, GroupC)
treat = c(rep("GroupA",length(GroupA)), rep("GroupB",length(GroupB)), rep("GroupC",length(GroupC)))

## The one way Anova

aov1 = aov(count~treat)
summary(aov1)

## P-value:
summary(aov1)[[1]]["Pr(>F)"][1]

df = data.frame(treat,count)
df
## Permutation F test
F_obs1 = summary(aov1)[[1]]["F value"][[1]][1]

n1 = sum(df$treat=="GroupA")
n2 = sum(df$treat=="GroupB")
n3 = sum(df$treat=="GroupC")
k=3
n=length(df$count)
cnt = 0
b= 10000
for(i in 1:b){
  permut=sample(df$count)
  mu1 = mean(permut[df$treat=="GroupA"])
  mu2 = mean(permut[df$treat=="GroupB"])
  mu3 = mean(permut[df$treat=="GroupC"])
  mu = mean(permut)
  sst = n1*(mu1-mu)^2+n2*(mu2-mu)^2+n3*(mu3-mu)^2
  sse = (n1-1)*var(permut[df$treat=="GroupA"]) + (n2-1)*var(permut[df$treat=="GroupB"]) + (n3-1)*var(permut[df$treat=="GroupC"])
  mst=sst/(k-1)
  mse=sse/(n-k)
  f=mst/mse
  if(f>=F_obs1){
    cnt=cnt+1
  }
}
pvalue = cnt/b
pvalue

# Problem 4

Dom_action = movies$DomesticGross[movies$Genre=="Action"]
Op_action = movies$OpeningWeekend[movies$Genre=="Action"]

## Pearson's corr
cor(Dom_action, Op_action)
## Spearman Rank Correlation
cor.test(rank(Dom_action), rank(Op_action), method = 's')

## regression of domestic gross on budget
fit = lm(movies$DomesticGross ~ movies$Budget)
fit


plot(movies$DomesticGross, movies$Budget, cex = 1,
   main = "Domestic Gross AGAINST Budget", xlab = "Domestic Gross(in million)", ylab = "Budget(in million)")
abline(fit, col = 'red', lwd='2')
lines(locavg, lwd=2, col='darkgreen')

## Non-parametric

locavg = supsmu(movies$DomesticGross, movies$Budget)

## Permutation Spearman Rank 
rx = rank(Dom_action)
ry = rank(Op_action)
spearobs = cor(rank(Dom_action), rank(Op_action))
rs = c()
for(i in 1:10000){
  rs[i]=cor(rx,sample(ry))
  p[i]=(rs[i]>=spearobs)+0
}
pvalue = sum(p)/10000
pvalue

## Permutation Regression
x = movies$DomesticGross
y = movies$Budget

b=10000
obs = cor(x, y)*sd(y)/sd(x)

obs

ans = c()
d=c()
for(i in 1:b){
  permuty = sample(y)
  ans[i]=cor(x, permuty)*sd(permuty)/sd(x)
  d[i]=(ans[i]>=obs) +0
}
pvalue = sum(d)/b

pvalue


cor.test(rank(x), rank(y))


# Problem 5

movies2 = na.omit(movies)

Dom = movies2$DomesticGross
For = movies2$ForeignGross

mean(Dom)
mean(For)

t.test(Dom, For)

outcome = c(Dom, For)
b = 10000
treat = c(rep(1,length(Bud_comedy)), rep(2,length(Bud_dramas)))
diffobs = mean(Bud_comedy)-mean(Bud_dramas)
d=c()
p=c()
for( i in 1:b){
  permut=sample(outcome)
  d[i]=mean(permut[treat==1])-mean(permut[treat==2])
  p[i]=(d[i]>=diffobs)+0
}
pvalue = sum(p)/b
pvalue


## ggpplot
gg_DF = ggplot() + 
  stat_qq(aes(sample = Dom), colour = "darkgreen") + 
  stat_qq(aes(sample = For), colour = "red") +
  geom_abline(aes(intercept = mean(Dom), slope = sd(Dom)), color = "darkgreen") +
  geom_abline(aes(intercept = mean(For), slope = sd(For)), color = "red")


gg_DF
