#install.packages("gscaLCA")
library(gscaLCA)
#AddHealth data with 3 clusters with 500 samples
AH.sample= AddHealth[1:500,]
R3 = gscaLCA (dat = AH.sample,
              varnames = names(AddHealth)[2:6],
              ID.var = "AID",
              num.class = 3,
              num.factor = "EACH",
              Boot.num = 0)
summary(R3)
R3$model.fit      # Model fit
R3$LCprevalence   # Latent Class Prevalence
R3$RespProb       # Item Response Probability
head(R3class$membership)     # Membership for all observations

# AddHealth data with 3 clusters with 500 samples with two covariates
R3_2C = gscaLCA (dat = AH.sample,
                 varnames = names(AddHealth)[2:6],
                 ID.var = "AID",
                 num.class = 3,
                 num.factor = "EACH",
                 Boot.num = 0,
                 multiple.Core = FALSE,
                 covnames = names(AddHealth)[7:8], # Gender and Edu
                 cov.model = c(1, 0),   # Only Gender varaible is added to the gscaLCR.
                 multinomial.ref = "FIRST")

# To print with the results of multinomial regression with hard partitioning of the gscaLCR,
# use the option of "multinomial.hard".
summary(R3_2C, "multinomial.hard")


# AddHealth data with 2 clusters with 20 bootstraps
R2 = gscaLCA(AddHealth,
             varnames = names(AddHealth)[2:6],
             num.class = 2,
             Boot.num = 20,
             multiple.Core = FALSE) # "multiple.Core = TRUE" is recommended.
# TALIS data with 3 clusters with 20 bootstraps and the "ALLin1" option
T3 = gscaLCA(TALIS,
             varnames = names(TALIS)[2:6],
             num.class = 3,
             num.factor = "ALLin1",
             Boot.num = 20,
             multiple.Core = FALSE) # "multiple.Core = TRUE" is recommended.






#--------------------------------------------------------------------------------------#
  # gscaLCA con atribuciones de pobreza y riqueza
#--------------------------------------------------------------------------------------#

load("../Input/Data_proc/2019/data_naomit2019.RData")

load("../Input/Data_proc/proc_data.RData")

data_naomit2019$Att_p1 = factor(data_naomit2019$Att_p1,
                                      levels=c("atribucion externa","atribucion interna"))
data_naomit2019$Att_p2 = factor(data_naomit2019$Att_p2,
                                levels=c("atribucion externa","atribucion interna"))
data_naomit2019$Att_r1 = factor(data_naomit2019$Att_r1,
                                levels=c("atribucion externa","atribucion interna"))
data_naomit2019$Att_r2 = factor(data_naomit2019$Att_r2,
                                levels=c("atribucion externa","atribucion interna"))

frq(data_naomit2019$Att_r2)
frq(proc_data$Att_p1)

data_naomit2019 <- tibble::rowid_to_column(data_naomit2019, "ID")
proc_data <- tibble::rowid_to_column(proc_data, "ID")


R3class = gscaLCA (dat = data_naomit2019,
              varnames = names(data_naomit2019)[19:22],
              ID.var = "ID",
              num.class = 2,
              num.factor = "EACH",
              Boot.num = 100)

summary(R3class)

LCR = gscaLCA (dat = data_naomit2019,
                 varnames = names(data_naomit2019)[19:22],
                 ID.var = "ID",
                 num.class = 2,
                 num.factor = "EACH",
                 Boot.num = 20,
                 multiple.Core = FALSE,
                 covnames = names(data_naomit2019)[12:13], # sociodemograficas
                 cov.model = c(1, 0),   # Only Gender varaible is added to the gscaLCR.
                 multinomial.ref = "FIRST")

summary(LCR, "multinomial.hard")
