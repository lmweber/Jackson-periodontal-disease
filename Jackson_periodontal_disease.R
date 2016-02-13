## ===========================================================================
## Retrospective study of the effectiveness of different treatment methods for 
## equine cheek teeth periodontal disease (Kirsten Jackson)
##
## Statistical analysis
##
## Lukas M. Weber, March 2015
## ===========================================================================


## Prepare data ----------

# load data file
data <- read.csv("Jackson2015_periodontal_disease.csv")
str(data)

# convert columns to factors
cols <- c("repeated","extracted","hexarinse","truncated")
data[,cols] <- lapply(cols, function(x) as.factor(data[,x]))
str(data)
summary(data)

# remove rows with no treatment
data <- data[data$treatment != "None",]
data$treatment <- factor(data$treatment, levels=c("CL","M","PVS","DW"))
dim(data)
summary(data)

# remove rows where tooth was extracted
data[data$extracted == 1,]  # note each extraction is represented by 2 rows/cases
data <- data[data$extracted == 0,]
dim(data)
summary(data)



## Summary statistics ----------

# report mean, median, sd, min, max, N

table(data$treatment)

# CL: mean=1.75, med=3.00, sd=2.82, min=-5.0, max=6.0, N=64
summary(data[data$treatment=="CL","improvement"])
sd(data[data$treatment=="CL","improvement"])

# M: mean=2.81, med=2.00, sd=2.73, min=-3.0, max=12.0, N=145
summary(data[data$treatment=="M","improvement"])
sd(data[data$treatment=="M","improvement"])

# PVS: mean=3.64, med=3.00, sd=3.90, min=-4.0, max=26.0, N=73
summary(data[data$treatment=="PVS","improvement"])
sd(data[data$treatment=="PVS","improvement"])

# DW: mean=4.00, med=3.00, sd=4.90, min=0.0, max=16.0, N=9
summary(data[data$treatment=="DW","improvement"])
sd(data[data$treatment=="DW","improvement"])



## One-sample t-tests ----------

# alternative hypotheses: population mean = 0, two-sided, 5 percent sig. level

# are improvements sig. diff. from zero? (all treatments combined)
t.test(data$improvement)
  # yes, significant (tval=15.00, df=290, pval<0.001, 95% CI 2.45-3.20)

# are improvements from each treatment group sig. diff. from zero?
# note: apply Bonferroni correction for multiple testing by manually multiplying 
# p-values by no. of tests, and adjusting conf.level argument for CIs
t.test(data[data$treatment=="CL", "improvement"], conf.level=1-0.05/4)
t.test(data[data$treatment=="M",  "improvement"], conf.level=1-0.05/4)
t.test(data[data$treatment=="PVS","improvement"], conf.level=1-0.05/4)
t.test(data[data$treatment=="DW", "improvement"], conf.level=1-0.05/4)
  # CL:  significant (tval=4.97,  df=63,  adj. pval<0.001, adj. 95% CI [0.84,2.66])
  # M:   significant (tval=12.43, df=144, adj. pval<0.001, adj. 95% CI [2.24,3.39])
  # PVS: significant (tval=7.98,  df=72,  adj. pval<0.001, adj. 95% CI [2.47,4.81])
  # DW:  significant (tval=2.45,  df=8,   adj. pval=0.160,  adj. 95% CI [-1.24,9.24])

# note multiple testing: Bonferroni correction gives family-wise error rate of 
# 5 percent with 4 tests, resulting in DW not significant (likely due to small 
# sample size)



## Plots ----------

# box plots
pdf("plots/improvement_boxplot.pdf", width=5, height=5)
plot(improvement ~ treatment, data=data, 
     xlab="treatment method", ylab="improvement in pocket depth (mm)", 
     main="Improvement by treatment method")
dev.off()

# improvement vs. previous pocket depth  ## don't include in report
pdf("plots/improvement_vs_previous.pdf",width=6,height=6)
plot(data$previousdepth, data$improvement, type="n", 
     xlab="previous pocket depth (mm)", ylab="improvement in pocket depth (mm)", 
     main="Improvement vs. previous pocket depth")
points(data[data$treatment=="CL", c("previousdepth","improvement")],pch=1)
points(data[data$treatment=="M",  c("previousdepth","improvement")],pch=2)
points(data[data$treatment=="PVS",c("previousdepth","improvement")],pch=3)
points(data[data$treatment=="DW", c("previousdepth","improvement")],pch=4)
legend("bottomright",pch=1:4,legend=c("CL","M","PVS","DW"))
dev.off()

# improvement vs. previous pocket depth  ## don't include in report
# (with colors; vertical lines for average previous depth by group)
pdf("plots/improvement_vs_previous_colors.pdf",width=6,height=6)
plot(data$previousdepth, data$improvement, type="n", 
     xlab="previous pocket depth (mm)", ylab="improvement in pocket depth (mm)", 
     main="Improvement vs. previous pocket depth")
points(data[data$treatment=="CL", c("previousdepth","improvement")],col=1,pch=1)
points(data[data$treatment=="M",  c("previousdepth","improvement")],col=2,pch=2)
points(data[data$treatment=="PVS",c("previousdepth","improvement")],col=3,pch=3)
points(data[data$treatment=="DW", c("previousdepth","improvement")],col=4,pch=4)
abline(v=mean(data[data$treatment=="CL", "previousdepth"]),col=1)
abline(v=mean(data[data$treatment=="M",  "previousdepth"]),col=2)
abline(v=mean(data[data$treatment=="PVS","previousdepth"]),col=3)
abline(v=mean(data[data$treatment=="DW", "previousdepth"]),col=4)
legend("bottomright",pch=1:4,col=1:4,legend=c("CL","M","PVS","DW"))
dev.off()

# bar plot (number of cases)  ## don't include in report
pdf("plots/number_of_cases.pdf", width=5, height=5)
barplot(table(data$treatment), 
        xlab="treatment method", ylab="number of cases", 
        main="Number of cases by treatment method")
legend(x=3.4, y=140, bty="n", legend=paste0(names(table(data$treatment)),":"))
legend(x=4.0, y=140, bty="n", legend=table(data$treatment))
dev.off()



## Model fitting: Linear mixed model with random intercepts for horse/repeat ----------

library("nlme")

# linear mixed model: check possible confounding variables

fm1 <- lme(improvement ~ treatment*previousdepth + treatment*days + hexarinse, 
           random=~1|horse/repeated, data=data, method="ML")
summary(fm1)
fm2 <- lme(improvement ~ treatment*previousdepth + days + hexarinse, 
           random=~1|horse/repeated, data=data, method="ML")
summary(fm2)
fm3 <- lme(improvement ~ treatment*days + previousdepth + hexarinse, 
           random=~1|horse/repeated, data=data, method="ML")
summary(fm3)
anova(fm1,fm2)
anova(fm1,fm3)
  # interaction effects not significant: drop them

fm4 <- lme(improvement ~ treatment + previousdepth + days + hexarinse, 
           random=~1|horse/repeated, data=data, method="ML")
summary(fm4)
fm5 <- lme(improvement ~ treatment + previousdepth + hexarinse, 
           random=~1|horse/repeated, data=data, method="ML")
summary(fm5)
fm6 <- lme(improvement ~ treatment + previousdepth + days, 
           random=~1|horse/repeated, data=data, method="ML")
summary(fm6)
anova(fm4,fm5)
anova(fm4,fm6)
  # days, hexarinse not significant: drop them

fm7 <- lme(improvement ~ treatment + previousdepth, 
           random=~1|horse/repeated, data=data, method="ML")
summary(fm7)
fm8 <- lme(improvement ~ previousdepth, 
           random=~1|horse/repeated, data=data, method="ML")
summary(fm8)
anova(fm7,fm8)
  # treatment is significant: keep it
  # best model is fm7 above (includes terms for treatment, previousdepth)

# check random intercepts for horse/repeated are required
fm9 <- lme(improvement ~ treatment + previousdepth, 
           random=~1|horse, data=data, method="ML")
summary(fm9)
fm10 <- lm(improvement ~ treatment + previousdepth, data=data)
summary(fm10)
anova(fm7,fm9)
anova(fm7,fm10)
anova(fm9,fm10)
  # yes, random intercept terms for horse/repeated (i.e. horse, horse:repeated) 
  # are required

# re-calculate results for best model (fm7) using REML instead of ML
# (unbiased variances, more reliable signifiance tests)
summary(fm7)
summary(lme(improvement ~ treatment + previousdepth, 
            random=~1|horse/repeated, data=data))
  # results almost identical
  # note: report fm7 instead, since this allows AIC comparison with the ANCOVA models

# approximate confidence intervals
summary(fm7)
intervals(fm7)

# pairwise comparisons: which of the treatment groups differ? (given that some 
# differences exist, since entire treatment term is significant)
library("multcomp")
summary(glht(fm7, linfct=mcp(treatment="Tukey")))
  # Tukey's method for all pairwise comparisons (takes into account multiple testing)
  # results: DW is significantly lower than each of CL, M, and PVS



## Alternative model fitting: ANCOVA ----------
## note: don't use these models for the main results, since they ignore possible 
## correlations within horse/repeated - linear mixed model results above are more 
## reliable

# ANCOVA (or linear regression): check possible confounding variables
summary(aov(improvement ~ treatment*previousdepth + treatment*days + hexarinse, data=data))
summary(aov(improvement ~ treatment*previousdepth + days + hexarinse, data=data))
summary(aov(improvement ~ treatment*days + previousdepth + hexarinse, data=data))
  # interaction effects not significant: drop them
summary(aov(improvement ~ treatment + previousdepth + days + hexarinse, data=data))
  # previousdepth, days, hexarinse: all significant

# ANCOVA: are there significant differences between treatment groups, taking 
# into account confounding variables?
fm_aov <- aov(improvement ~ treatment + previousdepth + days + hexarinse, data=data)
summary(fm_aov)  # yes, sig. differences exist (Fval=5.84, df=3, pval<0.001)
AIC(fm_aov)

# linear regression: equivalent to above (but shows coefficient for each treatment)
fm_lm <- lm(improvement ~ treatment + previousdepth + days + hexarinse, data=data)
summary(fm_lm)
AIC(fm_lm)

# GLHT: which treatment groups differ? (given that some differences exist)
library("multcomp")
summary(glht(fm_aov, linfct=mcp(treatment="Tukey")))
  # Tukey's method for all pairwise comparisons (takes into account multiple testing)
  # results: with p-values adjusted for multiple testing, no individually 
  # significant pairwise differences are found

  ## note: ANCOVA ignores possible correlations within horse/repeated
  ## linear mixed model is more reliable



## Additional results ----------

# (1) check residuals for the best fitting linear mixed model (fm7)
plot(fm7)
qqnorm(fm7)
  # no obvious problems

# check residuals for ANCOVA model
plot(fm_aov)
  # also ok; note leverage of outlier


# (2) compare AIC for linear mixed model vs. ANCOVA model
AIC(fm7)
AIC(fm_aov)
  # linear mixed model fm7 is much better


# (3) truncated cases (fully healed pockets)
nrow(data[data$truncated==1,])  # 86 cases
summary(data[data$truncated==1,"improvement"])  # mean=5.47mm, median=5.00mm
sd(     data[data$truncated==1,"improvement"])  # sd=1.69mm
plot(improvement ~ treatment, data=data[data$truncated==1,], 
     xlab="treatment method", ylab="improvement in pocket depth (mm)", 
     main="Improvement by treatment type (truncated cases only)")
  # note: most truncated cases have high improvement, so truncation is not 
  # likely to be a major problem


# (4) possible outlier (horse id37, diastema 310/11, treatment PVS, 26mm improvement)
which.max(data$improvement)  # row 229
data[229,]
summary(lme(improvement ~ treatment + previousdepth, 
            random=~1|horse/repeated, data=data[-229,]))
  # note: DW term is no longer significant with outlier removed



## Session information ----------

sink("sessionInfo.txt")
sessionInfo()
sink()


