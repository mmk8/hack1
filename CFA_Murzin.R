install.packages("psych") # factor analysis
install.packages("foreign") # open data from Stata, SPSS, SAS etc.
install.packages("corrplot") # visualize correlations
install.packages("GPArotation") # factor rotation
install.packages("nFactors") # define the number of factors to be extracted
install.packages("lavaan")
install.packages("semTools")
install.packages("semPlot")

library(psych)
library(foreign)
library(corrplot)
library(GPArotation)
library(nFactors)
library(lavaan)
library(semTools)
library(semPlot)


data <- read.spss("Japan.sav", use.value.labels = FALSE, to.data.frame = TRUE, use.missings = TRUE) 
View(data) 

dataselect <- subset (data, select= c(V131, V132, V133, V134, V135, V136, V137, V138, V139))
Japan <- na.omit(dataselect) 
scale(Japan) 

which(is.na(Japan)==T)
summary(Ukraine)

corr <- cor(Japan) 
corr 
corrplot(corr, type = "lower", method = "color", tl.cex = 0.9, tl.col="blue")

KMO(Japan)

VSS.scree(corr) 
eigen <- eigen(cor(Japan))
screeplot <- nScree(x=eigen$values) 
plotnScree(screeplot)

e1 <- fa(r = corr, nfactors = 2, fm = "pa", rotate = "varimax")
print(efa1$loadings, cutoff=0.3)
e1

e2 <- fa(r = corr, nfactors = 2, fm = "pa", rotate = "oblimin") 
print(efa2$loadings, cutoff=0.3) 
e2

cfa <- 'anime =~ V132 + V136 + V138
kawaii =~ V131 + V134'

model <- cfa(cfa, Japan)
summary(model, fit.measures = TRUE, standardized = TRUE)

# test configural invariance
configural1 <- cfa(model, Japan,  group="V131")
summary(configural1, fit.measures = TRUE)

cfa1 <- 'WhoRule =~ V132 + V135 +V138
LiveofCitizen =~ V131 + V134'
model1 <- cfa(cfa1, Japan)
summary(model1, fit.measures = TRUE, standardized = TRUE)



semPaths(model1, "std", rotation = 2,   
         nodeLabels = c("Religious Authorities \nInerpret the Law",
                        "The Army Takes Over \nWhen Government is Incompetent",
                        "People Obey \nTheir Rulers",
                        "Governments Tax \nthe Rich and Subsidise th Poor",
                        "People Receive State \nAid For Unemployment",
                        "WhoRule", "LiveofCitizens"), 
         colFactor=0, sizeMan = 20, sizeMan2 = 10, sizeLat = 15, 
         edge.color = "black", edge.label.cex = 1.1, esize = 1, mar = c(3,10,3,10))

# modification indices and expected value 
# choose the biggest mod.index and the highest standardized (!) epc
mepc <- modindices(model1, sort. = TRUE)
mepc

cfa2 <- 'WhoRule =~ V132 + V135 +V138
LiveofCitizen =~ V131 + V134
V132 ~~ V138'
model2 <- cfa(cfa2, Japan)
summary(model2, fit.measures = TRUE, standardized = TRUE)

cfa3 <- 'WhoRule =~ V132 + V135 +V138
LiveofCitizen =~ V131 + V134
V135 ~~ V134'
model3 <- cfa(cfa3, Japan)
summary(model3, fit.measures = TRUE, standardized = TRUE)

semPaths(model3, "std", rotation = 2,   
         nodeLabels = c("Religious Authorities \nInerpret the Law",
                        "The Army Takes Over \nWhen Government is Incompetent",
                        "People Obey \nTheir Rulers",
                        "Governments Tax \nthe Rich and Subsidise th Poor",
                        "People Receive State \nAid For Unemployment",
                        "WhoRule", "LiveofCitizens"), 
         colFactor=0, sizeMan = 20, sizeMan2 = 10, sizeLat = 15, 
         edge.color = "black", edge.label.cex = 1.1, esize = 1, mar = c(3,10,3,10))
