sparrow <- read.csv("sparrow.csv")

group.means = by(sparrow$Weight, sparrow$Treatment,mean)
overall.means = mean(sparrow$Weight)
group.sds = by(sparrow$Weight, sparrow$Treatment,sd)
overall.sds = sd(sparrow$Weight)
group.nis = by(sparrow$Weight, sparrow$Treatment,length)
overall.nis = length(sparrow$Weight)
overall_data = c(overall.means,overall.sds,overall.nis)
the.summary = rbind(group.means,group.sds,group.nis)
the.summary = cbind(the.summary,overall_data)
the.summary = round(the.summary,digits = 4)
#names(the.summary)
colnames(the.summary) = c("control","enlarged","reduced","overall")
rownames(the.summary) = c("Means","Std. Dev","Sample Size")
the.summary

summary(sparrow$Weight)
the.sparrow = lm(Weight ~ Treatment, data = sparrow)
anova.table = anova(the.sparrow)
anova.table
Weight = sparrow$Weight
Treatment = sparrow$Treatment
hist(Weight)

ggplot(sparrpw, aes(x = Weight)) + geom_histogram(binwidth = 0.5) + facet_grid(Treatment~.) +ggtitle("Histogram of nests for sparrows on Kent Island attracted different size sparrows")

ggplot(data = sparrow, aes(x = Treatment, y = Weight, color =Treatment)) + geom_boxplot() +
  labs(x = "Treatment",
       y = "Weight",
       title = "Boxplot of nests for sparrows on Kent Island attracted different size sparrows")
sparrow$ei = the.sparrow$residuals
nt = nrow(sparrow)
a = length(unique(sparrow$Treatment))
SSE = sum(sparrow$ei^2)
MSE = SSE/(nt-a)
eij.star = the.sparrow$residuals/sqrt(MSE)
eij.star
alpha = 0.01
t.cutoff = qt(1 - alpha/(2*nt), nt-a)
t.cutoff
CO.eij = which(abs(eij.star) > t.cutoff)
CO.eij
outliers = CO.eij
outliers

new.sparrow = sparrow[-outliers,]
diagnostic.model = lm(Weight ~ Treatment, data = new.sparrow) 

qqnorm(diagnostic.model$residuals, frame = FALSE, main = "Q-Q Plot of Model Residuals")
qqline(diagnostic.model$residuals, col = "#ffa7a1", lwd = 2)

ei = diagnostic.model$residuals 
the.SWtest = shapiro.test(ei) 
the.SWtest

plot(diagnostic.model$fitted.values, diagnostic.model$residuals, main = "Errors vs. Group Means",xlab = "Group Means", ylab = "Errors")
abline(h = 0,col = "purple")

qplot(Treatment, ei, data = diagnostic.model) +  ggtitle("Errors vs. Treatment") + xlab("Treatment") + ylab("Errors") + geom_hline(yintercept = 0)



boxplot(ei ~ Treatment, data = new.sparrow, main = "Treatment vs. Residuals", ylab = "Residuals", frame = FALSE)

the.BFtest = leveneTest(ei ~ Treatment, data = sparrow, center = median)
p.val = the.BFtest[[3]][1]
p.val


library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(knitr)
# Visualizing the data:
group.means = by(sparrow$Weight, sparrow$Treatment,mean)
plot(group.means, xaxt = "n",pch = 19,col = "purple",
     xlab = "Types of Treatment",ylab = "Weight in grams",
     main = "Average weight by group",type = "b")
axis(1,1:length(group.means),names(group.means))
# Box plot
boxplot(sparrow$Weight ~ sparrow$Treatment, main = "Weight by group",
        xlab = "Types of Treatment", ylab = "Weight")
# Histogram:
hist(sparrow$Weight, main = "Distribution of Weight",xlab = "Weight in grams")
# Histogram by groups
ggplot(sparrow, aes(x = Weight)) + geom_histogram(binwidth = 2, color = "black",fill = "white") + facet_grid(Treatment ~.) +ggtitle("Weight by Treatment")
ggplot(sparrow, aes(x = Weight, fill=Treatment)) + geom_histogram(binwidth=2, alpha=.5, position="identity") + ggtitle("Weight by Treatment")
# Groups
ggplot(sparrow, aes(x = Weight)) + geom_histogram(binwidth = 0.5) + facet_grid(Treatment~.) +ggtitle("Histogram of nests for sparrows on Kent Island attracted different size sparrows")
ggplot(data = sparrow, aes(x = Weight, y = Treatment, color = Weight)) + geom_boxplot() +
  labs(x = "Weight",
       y = " Treatment",
       title = "Boxplot of nests for sparrows on Kent Island attracted different size sparrows")
ggplot(data = sparrow, aes(x = Treatment, y = Weight, color = Weight)) + geom_boxplot() +
  labs(x = "Treatment ",
       y = "Weight",
       title = "Boxplot of nests for sparrows on Kent Island attracted different size sparrows")
# Summarizing the data:
treatment.weight.group.means =  by(sparrow$Weight, sparrow$Treatment,mean)
treatment.weight.group.sds = by(sparrow$Weight, sparrow$Treatment,sd)
treatment.weight.group.nis = by(sparrow$Weight, sparrow$Treatment,length)
treatment.weight.summary = rbind(treatment.weight.group.means, treatment.weight.group.sds, treatment.weight.group.nis)
treatment.weight.summary = round(treatment.weight.summary,digits = 4)
colnames(treatment.weight.summary) = names(treatment.weight.group.means)
rownames(treatment.weight.summary) = c("Means","Std. Dev","Sample Size")
treatment.weight.summary
knitr::kable(treatment.weight.summary)
treatment.weight.nt = sum(treatment.weight.group.nis)
a = length(treatment.weight.group.means)
overall.mean = (control.group.mean + enlarged.group.mean + reduced.group.mean)/g
overall.sds = sd(sparrow$Weight)
overall.nts = treatment.weight.nt
treatment.weight.overall.summary = rbind(overall.mean, overall.sds, overall.nts)
treatment.weight.overall.summary = round(treatment.weight.overall.summary,digits = 4)
treatment.weight.overall.summary
knitr::kable(treatment.weight.overall.summary)
# Analysis:
treatment.weight.model = lm(Weight ~ Treatment, data = sparrow)
treatment.weight.anova.table = anova(treatment.weight.model)
treatment.weight.anova.table
knitr::kable(treatment.weight.anova.table)
summary(treatment.weight.model)
# Confidence intervals:
treatment.weight.MSE = treatment.weight.anova.table[2,3]
alpha = 0.05
# Function:
give.me.CI = function(ybar, ni, ci, MSE, multiplier){
  if(sum(ci) != 0 & sum(ci !=0 ) != 1){
    return("Error - you did not input a valid contrast")
  } else if(length(ci) != length(ni)){
    return("Error - not enough contrasts given")
  }
  else{
    estimate = sum(ybar*ci)
    SE = sqrt(MSE*sum(ci^2/ni))
    CI = estimate + c(-1,1)*multiplier*SE
    result = c(estimate,CI)
    names(result) = c("Estimate","Lower Bound","Upper Bound")
    return(result)
  }
}
# Turkey:
Tuk = qtukey(1-alpha, a, treatment.weight.nt-a)/sqrt(2)
# Scheffe
S = sqrt((a-1)*qf(1-alpha, a-1, treatment.weight.nt-a))
# Bonferroni
g=3
B = qt(1-alpha/(2*g), treatment.weight.nt-a)
value = c(Tuk, S, B)
multiplier = c("Tukey", "Scheffe", "Bonferroni")
multi.df = data.frame(multiplier,value)
knitr::kable(multi.df)
# Largest sparrow
largest.ci = c(1,0,0)
give.me.CI(treatment.weight.group.means, treatment.weight.group.nis, largest.ci, treatment.weight.MSE, Tuk)
# Comparing the control nest to the enlarged nest
control.enlagred.ci = c(1,-1,0)
give.me.CI(treatment.weight.group.means, treatment.weight.group.nis, control.enlagred.ci, treatment.weight.MSE, Tuk)
# Comparing the control nest to the reduced nest
control.reduced.ci = c(1,0,-1)
give.me.CI(treatment.weight.group.means, treatment.weight.group.nis, control.reduced.ci, treatment.weight.MSE, Tuk)
# Factor Effect Model:
control.group.mean = treatment.weight.summary[1,1]
enlarged.group.mean = treatment.weight.summary[1,2]
reduced.group.mean = treatment.weight.summary[1,3]
control.diff = control.group.mean - overall.mean
enlarged.diff = enlarged.group.mean - overall.mean
reduced.diff = reduced.group.mean - overall.mean
gamma_i = c(control.diff, enlarged.diff, reduced.diff)
treatment.name = c("Control", "Enlagred", "Reduced")
factor.df = data.frame(treatment.name, gamma_i)
knitr::kable(factor.df)
# Power calculation:
treatment.weight.phi = (1/treatment.weight.MSE)*(sqrt((45*(control.diff^2)+45*(enlarged.diff^2)+26*(reduced.diff^2))/3))
