#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

#----------------------------------------------------------
# Reset graphic device
# As long as there is any dev open (exept "null device") 
# close the active one!
# Caution: closes all open plots!!!!
#----------------------------------------------------------
while(!is.null(dev.list()))
{
  dev.off()
}

# install.packages("ISLR") -- run this code to install ISLR package - first time only
#install.packages("ISLR")
require(ISLR) #loading the package
library(ISLR) # enabling package
attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]

#checking the summary of dataset
summary(assessment_dataframe)

#checking the structure of dataset
str(assessment_dataframe)

#checking in case null values is present in the data frame
assessment_dataframe[!complete.cases(assessment_dataframe),]

#checking in case NA is present in any columns
assessment_dataframe[is.na.data.frame(assessment_dataframe),]

#Extract the data of the columns year, age, education and wage and work on those only. 
wage_survey_data <- assessment_dataframe[, c("year", "age", "education","wage")]

str(wage_survey_data) 
summary(wage_survey_data)  

#checking the levels, whether levels are correct or not
levels(wage_survey_data$education)

#changing the row names to NULL to sync the index 
row.names(wage_survey_data) <- NULL 

# Histograms for showing the distribution of data(number of males) - Wage , Age and Year
##run all the 6 lines together for getting the desired histograms
#Below is the par function used to get all the graphs at once in the screen

parBackup <- par()
par(mfrow=c(1,3))
wagehist <- hist(wage_survey_data$wage, breaks=15)
agehist <-  hist(wage_survey_data$age, breaks=15)
yearhist <- hist(wage_survey_data$year)
par(parBackup)

## Seperate histograms in one plot to check the number of males corresponding to each level of education

parBackup <- par()
par(mfrow=c(3,2))
WageHISTHS <- hist(wage_survey_data[,4][wage_survey_data[,3]=="1. < HS Grad"], 
                   breaks=15, 
                   col="cornflowerblue",
                   xlab="WAGE - <HS Grad", 
                   ylim = c(0, 350),
                   main="Distribution of WAGES  - Below HS Grad")
WageHISTHsGrad <- hist(wage_survey_data[,4][wage_survey_data[,3]=="2. HS Grad"], 
                       breaks=15, 
                       col="cornflowerblue",
                       xlab="WAGE - HS Grad", 
                       ylim = c(0, 350),
                       main="Distribution of WAGES - HS Grad")
WageHISTSomeCollege <- hist(wage_survey_data[,4][wage_survey_data[,3]=="3. Some College"], 
                            breaks=15, 
                            col="cornflowerblue",
                            xlab="WAGE - some College", 
                            ylim = c(0, 350),
                            main="Distribution of WAGES - Some College")
WageHISTCollegeGrad <- hist(wage_survey_data[,4][wage_survey_data[,3]=="4. College Grad"], 
                            breaks=15, 
                            col="cornflowerblue",
                            xlab="WAGE - College Grad", 
                            ylim = c(0, 350),
                            main="Distribution of WAGES - College Grad")
WageHISTAdvancedDegree <- hist(wage_survey_data[,4][wage_survey_data[,3]=="5. Advanced Degree"], 
                               breaks=15, 
                               col="cornflowerblue",
                               xlab="WAGE - Advanced Degree", 
                               ylim = c(0, 350),
                               main="Distribution of WAGES - Advanced Degree")
par(parBackup)


### installing ggplot package 
install.packages("ggplot2")

#enabling ggplot library
library(ggplot2)


###Calculate the correlation coefficient for quantitatve (numeric) fields i.e wage and age, 
###wage and year with Pearson, Kendall and Spearman test respectively

##wage and year (quantitative ---> quantitative)
###Kendals correlation test
##here rk_wage_year is the kendal's tau correlation coefficient 

rk_wage_year <- cor(wage_survey_data[,"wage"],
                    wage_survey_data[,"year"], 
                    method = c("kendall") )

##rk_wage_year = 0.05432

yearmodel <- lm(wage ~ year, data = wage_survey_data)
plot(yearmodel)

## wage and age (quantitative --> quantitative)
###Pearson correlation test

rp_wage_age <- cor(wage_survey_data[,"wage"],
                   wage_survey_data[,"age"], 
                   method = c("pearson") )

agemodel <- lm(wage ~ age, data = wage_survey_data)
plot(agemodel)

## qplot for checking the slope between Wage and Age
scatter_01 <- qplot(age, wage, data=wage_survey_data, main="Wage vs Age Data")
wageagescatter <- scatter_01 + geom_smooth()
wageagescatter

######Anova test for finding relation between ordinal categorical variable education with continuous numeric variable wage

install.packages("xtable")
#used to create formated html tables
library(xtable)

#used to subset data set          
library(plyr)  

#used for dollar format         
library(scales)             

##subsetting the data frame for performing ANOVA with 2 relevant variables

subset <- data.frame(wage_survey_data$education, wage_survey_data$wage)
subset <- na.omit(subset)

#subset of discriptive statistics
summary <- ddply(subset, "wage_survey_data.education", summarise, N = length(wage_survey_data.wage),
                 MEAN = mean(wage_survey_data.wage), STD = sd(wage_survey_data.wage), 
                 MIN = min(wage_survey_data.wage), MAX = max(wage_survey_data.wage),
                 MEDIAN = median(wage_survey_data.wage), IQR = IQR(wage_survey_data.wage))

#renaming column of summay matrix
names(summary)[1] <- "EDUCATION"

#converting values to dollar format
summary <- cbind(summary[,1:2],
                 apply(summary[c("MEAN","STD","MIN","MAX","MEDIAN","IQR")],2,dollar))


#creating and printing table of summary statistics
stat.table <- xtable(summary, caption = "Table 1")
print(stat.table, type = "html")


g <- ggplot(subset, aes(x = wage_survey_data.education, y = wage_survey_data.wage, fill = wage_survey_data.education))
g <- g + geom_boxplot()
g <- g + xlab("Education") + ylab("Wage") + labs(title = "Wage by Education")
g <- g + guides(fill=guide_legend(title=NULL))
print(g)

#code for anova analysis
fit <- aov(wage ~ education, data = subset)

#code for anove table
anova.table <- xtable(fit, caption = "Table 2")
print(anova.table, type = "html")


#code for pairwise t-tests
ttests <- pairwise.t.test(subset$wage_survey_data.wage,subset$wage_survey_data.education,p.adj="bonferroni")

#table for pairwise t-test results
ttest.table <- xtable(ttests$p.value, caption="Table 3")
print(ttest.table, type = "html")

####Multiple Regression for determining the effect of 2 independent variables on dependent variable

#### Age + education ----> Wage

# Actually R does the dummy coding for us when using a character or 
# factor variable as a predictor in our lm model

model1 <- lm(wage ~ age + education, data = wage_survey_data)

str(model1)
summary(model1)
confint(model1, conf.level=0.90)
plot(model1)

####Fitted Regression Equation 
### E1, E2, E3, E4 and E5 are the levels of the education respectively
##Rt is the formula which is obtained from the model1

###   Rt = 60.33597 + 0.56869 Age + 11.43 E2 + 24.16 E3 + 39.77 E4 + 64.99 E5 

##fitted Regression equation for education ---> Below HS Grad
## in this case E2, E3, E4 and E5 will be zero

## Rt1 = 60.33597 + 0.56869 Age 

##fitted Regression equation for education ---> HS Grad
## in this case E3, E4 and E5 will be zero

## Rt2 = 60.33597 + 0.56869 Age + 11.43 (1)
## Rt2 = 71.76 + 0.56869 Age

##fitted Regression equation for education ---> Some College
## in this case E2, E4 and E5 will be zero

### Rt3 = 60.33597 + 0.56869 Age  + 24.16 (1)
### Rt3 = 84.49 + 0.56869 Age

##fitted Regression equation for education ---> college Grad
## in this case E2, E3 and E5 will be zero

###   Rt4 = 60.33597 + 0.56869 Age +  39.77 (1)
### Rt4 = 100.105 + 0.56869 Age

##fitted Regression equation for education ---> Advanced Degree
## in this case E2, E3 and E4 will be zero

###   Rt5 = 60.33597 + 0.56869 Age  + 64.99 E5 
#### Rt5 = 125.325 + 0.56869 Age

###Plotting the Regression Model (wage ~ age + education)

plot(wage_survey_data$age[wage_survey_data$education=="1. < HS Grad"], wage_survey_data$wage[wage_survey_data$education=="1. < HS Grad"], col=2, ylim =c(0,350),xlim=c(0,100), xlab = "Age",ylab = "Wage",
     main="Wage Vs Age, Education")

points(wage_survey_data$age[wage_survey_data$education=="2. HS Grad"], wage_survey_data$wage[wage_survey_data$education=="2. HS Grad"], col=3)

points(wage_survey_data$age[wage_survey_data$education=="3. Some College"], wage_survey_data$wage[wage_survey_data$education=="3. Some College"], col=4)

points(wage_survey_data$age[wage_survey_data$education=="4. College Grad"], wage_survey_data$wage[wage_survey_data$education=="4. College Grad"], col=5)

points(wage_survey_data$age[wage_survey_data$education=="5. Advanced Degree"], wage_survey_data$wage[wage_survey_data$education=="5. Advanced Degree"], col=6)

legend(5,350,legend=c("Below HS Grad","HS Grad","Some College","College Grad","Advanced Degree"), col=c(2,3,4,5,6), pch = c(1,100), bty ="n")

#adding regression line 

abline(a=60.33597 , b=0.56869, col=2,lwd=3)
abline(a=71.76  , b=0.56869, col=3,lwd=3)
abline(a=84.49  , b=0.56869, col=4,lwd=3)
abline(a=100.105  , b=0.56869, col=5,lwd=3)
abline(a=125.325  , b=0.56869, col=6,lwd=3)

####GG plot to demonstrate the relation between ordinal categorical and quantitative data fields

plot1 <- ggplot(data=wage_survey_data, aes(x=education, y=wage)) 

#adding layers
plot3 <- plot1 + geom_jitter(aes(colour=age)) +  geom_boxplot(alpha=0.7 , outlier.colour = NA) 

## adding labels legend details and Title to the graph
plot_4 <- plot3 +
  xlab("Education") +
  ylab("Wage") +
  ggtitle("Wage by Education, Age")

##adding layers
plot_final <- plot_4 + theme(axis.title.x = element_text(colour = "DarkGreen", size=25),
                             axis.title.y = element_text(colour = "Red", size = 25),
                             axis.text.x = element_text(size=15),
                             axis.text.y = element_text(size = 20),
                             legend.title = element_text(size=25),
                             legend.text = element_text(size = 15),
                             plot.title = element_text(colour = "DarkBlue", size = 35,
                                                       family = "Comic Sans MS"))
####plotting the final graph 
plot_final


#######Interation  between Age and Education

###firstly plot the graph between Age and Education
##non parallel lines 

model2 <- lm(wage ~ age + education + age:education, data = wage_survey_data)
summary(model2)

#Fitted Regression Equation

### Wage = 68.09017 + 0.38316 * age + 7.24752 E2 + 0.86720 E3 + 36.83562 E4 + 61.13781 E5 
###        + 0.10113 age:E2 + 0.56573 age:E3 + 0.07277 age:E4 + 0.09876 age:E5 

#####################################################################################################################################################