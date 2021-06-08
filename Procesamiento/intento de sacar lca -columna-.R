#Flexmix

#install.packages("flexmix")
library(flexmix)
data("NPreg")
m1 <- flexmix(yn ~ x + I(x^2), data = NPreg, k = 2)
m1

data("patent")
pat_mix <- flexmix(Patents ~ lgRD,
                   k = 3, data = patent,
                   model = FLXMRglm(family = "poisson"),
                   concomitant = FLXPmultinom(~RDS))
plot(pat_mix, mark = 3)



##############################

data("NregFix", package = "flexmix")
library(lattice)
xyplot(y ~ x1 | x2 * w, data = NregFix, groups = class)
Model <- FLXMRglmfix(~ 1, fixed = ~ x2, 
                     nested = list(k = c(2, 1),
                                   formula = c(~x1, ~0)))
fittedModel <- initFlexmix(y ~ 1, model = Model, data = NregFix, k = 3,
                           concomitant = FLXPmultinom(~ w), nrep = 5)
fittedModel


#######################################

load("../Input/Data_proc/2019/data_naomit2019.RData")

rm(list = ls(all = TRUE))   # Clean objects
#install.packages("gmnl")
library(gmnl)             # Load gmnl package
#install.packages("mlogit")
library(mlogit)           # Load mlogit package

install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")

# Load data and put it into the required format
data("Electricity", package = "mlogit")

data_naomit2019$Att_p1<-as_factor(data_naomit2019$Att_p1)
data_naomit2019$Att_p2<-as_factor(data_naomit2019$Att_p2)
data_naomit2019$Att_r1<-as_factor(data_naomit2019$Att_r1)
data_naomit2019$Att_r2<-as_factor(data_naomit2019$Att_r2)

Electr <- mlogit.data(data_naomit2019,
                      choice = c("Att_p1","Att_p2","Att_r1","Att_r2"), 
                      varying = 11:16, 
                      shape = "wide", 
                      sep = "")

# Estimate a LC-MNL model with 3 classes
lc <- gmnl(choice ~ pf + cl + loc + wk + tod + seas | 0 | 0 | 0 | 1, 
           data = Electr,
           model = 'lc', 
           Q = 3, 
           panel = TRUE,
           method = "bhhh")
# Result
summary(lc)

# Load script with additional functions
source('lc_helpers.R')

# Plot coefficients and 95% CI
plot_ci_lc(lc)

# Plot some coefficients
plot_ci_lc(lc, var = c("pf", "cl"))
