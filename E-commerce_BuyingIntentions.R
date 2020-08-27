
##pakages
install.packages("magrittr")
install.packages("dplyr")    # alternative installation of the %>%
install.packages("tidyr")
install.packages("corrplot")

library(plotly)
library(magrittr) # needs to be run every time the R session is started and should use %>%
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(ggpubr)
library(grid)
library(gridExtra)
## read the data
onlinedata <- read.csv("online_shoppers_intention.csv", header = TRUE)

## viewing the dataset
head(onlinedata)
View(onlinedata)
summary(onlinedata)
dim(onlinedata)
sapply(onlinedata, class)
colnames(onlinedata)
str(onlinedata)

##----------------------------------Data CLeaning--------------------------------------------------------##
##Looking for missing Values
(complete.cases(onlinedata))
which(!complete.cases(onlinedata))
install.packages("kableExtra")
library(kableExtra)
list_na <- colnames(onlinedata)[ apply(df, 2, anyNA) ]
list_na
numMissingVal <-sapply(onlinedata, function(x) sum(length(which(is.na(x)))))  
kable(as.data.frame(numMissingVal)) %>% kable_styling(bootstrap_options = c("striped", "hover", "responsive")) %>% scroll_box(width = "100%", height = "250px")
##no Missing Values
##-------------------------------------Exploratory Data Analysis--------------------------------------------------------------##
summary(onlinedata)
## finding relationship between each variable 

##--------------------Univariate Analysis-------------------------------------------------------------------------------
par(mfrow = c(4,4))
hist(onlinedata$Administrative, col = "darkseagreen", xlab = "Administrative" , ylab = "count", border = "white", main = "Administrative")
hist(onlinedata$Administrative_Duration, col = "darkseagreen", xlab = "Administrative_Duration" , ylab = "count", border = "white", main = "Administrative_Duration")
hist(onlinedata$Informational, col = "darkseagreen", xlab = "Informational" , ylab = "count", border = "white", main = "Informational")
hist(onlinedata$Informational_Duration, col = "darkseagreen", xlab = "Informational_Duration" , ylab = "count", border = "white", main = "Informational_duration")
hist(onlinedata$ProductRelated, col = "darkseagreen", xlab = "ProductRelated" , ylab = "count", border = "white", main = "ProductRelated")
hist(onlinedata$ProductRelated_Duration, col = "darkseagreen", xlab = "ProductRelated_Duration" , ylab = "count", border = "white", main = "ProductRelated_Duration")
hist(onlinedata$BounceRates, col = "darkseagreen", xlab = "BounceRates" , ylab = "count", border = "white", main = "BounceRates")
hist(onlinedata$ExitRates, col = "darkseagreen", xlab = "ExitRates" , ylab = "count", border = "white", main = "ExitRates")
hist(onlinedata$SpecialDay, col = "darkseagreen", xlab = "SpecialDay" , ylab = "count", border = "white", main = "SpecialDay")
hist(onlinedata$TrafficType, col = "darkseagreen", xlab = "TrafficType" , ylab = "count", border = "white", main = "TrafficType")
hist(onlinedata$OperatingSystems, col = "darkseagreen", xlab = "OperatingSystems" , ylab = "count", border = "white", main = "OperatingSystems")
hist(onlinedata$Browser, col = "darkseagreen", xlab = "Browser" , ylab = "count", border = "white", main = "Browser")
hist(onlinedata$Region, col = "darkseagreen", xlab = "Region" , ylab = "count", border = "white", main = "Region")
plot(onlinedata$Month, col = "darkseagreen", xlab = "Month" , ylab = "count", border = "white", main = "Month", ylim = c(0,5000))

##Checking for unique columns present in categorical variables
rapply(onlinedata,function(x)length(unique(x)))
unique(onlinedata$Weekend, incomparables = FALSE)
unique(onlinedata$Revenue, incomparables = FALSE)
unique(onlinedata$VisitorType, incomparables = FALSE)
unique(onlinedata$Month, incomparables = FALSE)

## Categorical variables -Revenue 
pie_chart1 <- data.frame(Category = c("True", "False"), "freq" = c(1908, 10422))

ggplot(pie_chart1, aes (x="", y = freq, fill = factor(Category))) + 
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste(round(freq / sum(freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Pie Chart of Revenue") + scale_fill_brewer(palette = "Pastel2") +
  coord_polar("y")

##Visitortype- ##Newvisitor - 1694, other - 85, Returning - 10551
pie_chart <- data.frame(Category = c("NewVisitor", "Other", "Returning_Visitor"), "freq" = c(1694, 85, 10551))

ggplot(pie_chart, aes (x="", y = freq, fill = factor(Category))) + 
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste(round(freq / sum(freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Pie Chart of VisitorType") + scale_fill_brewer(palette = "Set3") +
  coord_polar("y")

##Weekend
pie_chart2 <- data.frame(Category = c("True", "False"), "freq" = c(2868, 9462))

ggplot(pie_chart2, aes (x="", y = freq, fill = factor(Category))) + 
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste(round(freq / sum(freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_classic() +
  theme(plot.title = element_text(hjust=0.5),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Category",
       x = NULL,
       y = NULL,
       title = "Pie Chart of Weekend Sales") + scale_fill_brewer(palette = "Set3") +
  coord_polar("y")

library(forcats)
ggplot(mutate(onlinedata, Month = fct_infreq(Month))) + geom_bar(aes(x = Month), fill = "darkseagreen", main = "Month") + theme_classic()
ggplot(mutate(onlinedata, Weekend = fct_infreq(Weekend))) + geom_bar(aes(x = Weekend), fill = "darkseagreen") + theme_classic()
ggplot(mutate(onlinedata, VisitorType = fct_infreq(VisitorType))) + geom_bar(aes(x = VisitorType), fill = "darkseagreen") + theme_classic()


## checking normality with histogram
par(mfrow = c(4,4))
hist(onlinedata$Administrative)        # We do not see a very good graph here to draw meaningful conclusions,
hist(log(onlinedata$Administrative), main = "Administrative", col = "lightcyan3", border = "white", xlab = "Administrative") ## Better results - right skewed
hist(log(onlinedata$Administrative_Duration), main = "Administrative_Duration", col = "lightcyan3", border = "white", xlab = "Administrative_Duration")
hist(log(onlinedata$Informational), main = "Informational", col = "lightcyan3", border = "white", xlab = "Informational")
hist(log(onlinedata$Informational_Duration), main = "Informational_Duration", col = "lightcyan3", border = "white", xlab = "Informational_Duration")
hist(log(onlinedata$ProductRelated), main = "ProductRelated", col = "lightcyan3", border = "white", xlab = "ProductRelated")
hist(log(onlinedata$ProductRelated_Duration), main = "ProductRelated_Duration", col = "lightcyan3", border = "white", xlab = "ProductRelated_Duration")
hist(log(onlinedata$BounceRates), main = "BounceRates", col = "lightcyan3", border = "white", xlab = "BounceRates")
hist(log(onlinedata$ExitRates), main = "ExitRates", col = "lightcyan3", border = "white", xlab = "ExitRates")
hist(log(onlinedata$SpecialDay), main = "SpecialDay", col = "lightcyan3", border = "white", xlab = "SpecialDay")
hist(log(onlinedata$Browser), main = "Browser", col = "lightcyan3", border = "white", xlab = "Browser")
hist(log(onlinedata$Region), main = "Region", col = "lightcyan3", border = "white", xlab = "Region")
hist(log(onlinedata$OperatingSystems), main = "OperatingSystems", col = "lightcyan3", border = "white", xlab = "OperatingSystem")
hist(log(onlinedata$TrafficType), main = "TrafficType", col = "lightcyan3", border = "white", xlab = "TrafficType")


##--------------------------------Bi-Variate Analysis---------------------------------------------------------------------------------------------------------------------
##All variables vs Revenue
library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(ggtitle)
attach(onlinedata)
par(mfrow = c(4,3))
boxplot(onlinedata$Administrative ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "Adminstrative", main = "Administrative vs Revenue")        
boxplot(onlinedata$Informational ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "Informational", main = "Informational vs Revenue")        
boxplot(onlinedata$Informational_Duration ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "Informationduration", main = "Informationduration vs Revenue")        
boxplot(onlinedata$Administrative_Duration ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "AdministrativeDuration", main = "AdministrativeDuration vs Revenue")
boxplot(onlinedata$ProductRelated ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "ProductRelated", main = "Productrelated vs Revenue")        
boxplot(onlinedata$ProductRelated_Duration ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "ProductRelated_Duration", main = "Productrelated_Duration vs Revenue")        
boxplot(onlinedata$BounceRates ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "BounceRates", main = "BounceRates vs Revenue")  
boxplot(onlinedata$PageValues ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "Pagevalues", main = "Pagevalues vs Revenue")
boxplot(onlinedata$Region ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "Region", main = "Region vs Revenue")        
boxplot(onlinedata$TrafficType ~ onlinedata$Revenue, col = "lightblue", pch = 20, xlab = "Buy or Not", ylab = "Traffictype", main = "Traffictype vs Revenue")                

## Revenue vs pages visited and duration of each page
library(hrbrthemes)
library(viridis)
par(mfrow = c(1,2))
plota<- ggplot(onlinedata, aes(x = Revenue, y=Administrative)) + geom_violin() + geom_violin(trim=FALSE, fill= "pink", color='black') + 
geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("Revenue")

plotad<- ggplot(onlinedata, aes(x = Revenue, y=Administrative_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill= "pink", color='black') + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("Revenue")
ploti<- ggplot(onlinedata, aes(x = Revenue, y=Informational)) + geom_violin() + geom_violin(trim=FALSE, fill= "pink", color='black') + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("Revenue")

plotid<- ggplot(onlinedata, aes(x = Revenue, y=Informational_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill= "pink", color='black') + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("Revenue")

plotp<- ggplot(onlinedata, aes(x = Revenue, y=ProductRelated)) + geom_violin() + geom_violin(trim=FALSE, fill= "pink", color='black') + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("Revenue")

plotpd<- ggplot(onlinedata, aes(x = Revenue, y=ProductRelated_Duration)) + geom_violin() + geom_violin(trim=FALSE, fill= "pink", color='black') + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Violin wrapping a boxplot") +
  xlab("Revenue")
grid.arrange(plota, plotad, ploti, plotid, plotp, plotpd, nrow = 3, ncol = 2)


dev.off()
library(GGally)
##Bouncerate, exitrares, pagevalues vs Revenue
plotb <- ggplot(onlinedata, aes(x=BounceRates, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ") + 
scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))


plote <- ggplot(onlinedata, aes(x=ExitRates, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ") +
scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

plotpv <- ggplot(onlinedata, aes(x=PageValues, fill=Revenue)) + geom_density(alpha=0.4) + labs(y = " ") +
scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))

grid.arrange(plotb, plote, plotpv, nrow = 3)


table(Month)
##Monthly revenue 
onlinedata$Month <- as.character(onlinedata$Month)
onlinedata$Month[onlinedata$Month == "June"] <- "Jun"
onlinedata$Month <- as.factor(onlinedata$Month)
onlinedata$Month = factor(onlinedata$Month, levels = month.abb)
ggplot(data.frame(onlinedata), aes(Month, fill=Revenue)) + geom_boxplot() + labs(x = "Month") + labs(y = "ProductRelated") +
  scale_fill_manual(values=c("palegreen4", "palegreen3"))

##Specialday
library(ggthemes)
table(SpecialDay)
par(mfrow = c(2,2))
plots <- ggplot(onlinedata, aes(x = factor(1), y = SpecialDay)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(color = "darkred", width = 0.1, size = 1, alpha=0.4) + labs(x = "Special Day") + labs(y = "Closeness") + theme(axis.text.x = element_blank(), axis.ticks = element_blank())
plotsr <- ggplot(onlinedata, aes(x = Revenue, y = SpecialDay)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(color = "darkred", width = 0.2, size = 1, alpha=0.4) + labs(x = "Special Day") + labs(y = " ") + theme(axis.ticks = element_blank())
grid.arrange(plots, plotsr, ncol = 2)


##Operating system , browser, traffictype, region vs Revenue
par(mfrow = c(3,3))
pt1<-ggplot(data.frame(onlinedata), aes(OperatingSystems, fill=Revenue)) + geom_bar() + labs(x = "Operating Systems") + labs(y = " ") + 
  scale_x_continuous(breaks = 1:8) +  scale_fill_manual(values=c("lightblue2", "lightpink", "black")) + coord_cartesian(xlim = c(0, 8), ylim = c(0, 8000)) + theme_classic()
pt2<-ggplot(data.frame(onlinedata), aes(Browser, fill=Revenue)) + geom_bar() + labs(x = "Browser") + labs(y = " ") + 
  scale_x_continuous(breaks = 1:8) +  scale_fill_manual(values=c("lightblue2", "lightpink", "black")) + coord_cartesian(xlim = c(0, 8), ylim = c(0, 8000)) + theme_classic()                                                                                                                                                                                                       
pt3<-ggplot(data.frame(onlinedata), aes(Region, fill=Revenue)) + geom_bar() + labs(x = "Region") + labs(y = " ") + 
  scale_x_continuous(breaks = 1:8) +  scale_fill_manual(values=c("lightblue2", "lightpink", "black")) + coord_cartesian(xlim = c(0, 8), ylim = c(0, 8000)) + theme_classic()                                                                                                                                                                                                       
pt4<-ggplot(data.frame(onlinedata), aes(TrafficType, fill=Revenue)) + geom_bar() + labs(x = "TrafficType") + labs(y = " ") + 
  scale_x_continuous(breaks = 1:8) +  scale_fill_manual(values=c("lightblue2", "lightpink", "black")) + coord_cartesian(xlim = c(0, 8), ylim = c(0, 8000)) + theme_classic()                                                                                                                                                                                                       
grid.arrange(pt1, pt2, pt3, pt4, nrow = 4)


##Pagesvisited by visitor type
ggplot(onlinedata, aes(x = VisitorType, y=Administrative)) + geom_violin() + geom_violin(trim=FALSE, fill= "deepskyblue", color='black') + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Administrative vs VisitorType") +
  xlab("VisitorType")

ggplot(onlinedata, aes(x = VisitorType, y=Informational)) + geom_violin() + geom_violin(trim=FALSE, fill= "deepskyblue", color='black') + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Informational vs VisitorType") +
  xlab("VisitorType")

ggplot(onlinedata, aes(x = VisitorType, y=ProductRelated)) + geom_violin() + geom_violin(trim=FALSE, fill= "deepskyblue", color='black') + 
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  scale_fill_viridis(discrete = TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("ProductRelated vs VisitorType") +
  xlab("ProductRelated")

##Monthly ExitRates

##Monthly Revenue

plotm <- ggplot(data.frame(onlinedata), aes(Month, fill=Revenue)) + geom_bar() + 
  labs(x = "Month") + labs(y = " ") + scale_fill_manual(values =c("lightseagreen", "lightskyblue2"))
##Weekend Revenue
plotw <-ggplot(data.frame(onlinedata), aes(Weekend, fill=Revenue)) + geom_bar() + labs(x = "Weekend") + labs(y = " ") + scale_fill_manual(values =c("lightseagreen", "lightskyblue2"))
##Visitortype vs Revenue
plotVR <- ggplot(data.frame(onlinedata), aes(VisitorType, fill=Revenue)) + geom_bar() + labs(x = "Visitor Type") + labs(y = " ") +
scale_fill_manual(values = c("lightseagreen", "lightskyblue2"))
grid.arrange(plotm, plotw, plotVR, nrow = 3)


##----------------------Multivariate analysis-----------------------------------------------------------------
##revenue based on visitor type vs Prodrelated
ggplot(onlinedata, aes(x=VisitorType, y=ProductRelated, fill = Revenue)) + scale_y_continuous(labels = scales::comma) +
  geom_boxplot() + geom_smooth(method="loess", se=F) +
  labs( x="Visitor type", y="ProductRelated") + 
  scale_fill_manual(values =c("Salmon4", "Salmon3"))

##Month vs pagevalues wrt Revenue
 ggplot(onlinedata, aes(x=Month, y=PageValues, fill = Revenue)) + scale_y_continuous(labels = scales::comma) +
  geom_boxplot() + geom_smooth(method="loess", se=F) +
  labs( x="Month", y="PageValues") + 
  scale_fill_manual(values =c("indianred4", "indianred3"))

##Month vs ExitRates wrt revenue
ggplot(onlinedata, aes(x=Month, y=ExitRates, fill = Revenue)) + scale_y_continuous(labels = scales::comma) +
  geom_boxplot() + geom_smooth(method="loess", se=F) +
  labs( x="Month", y="ExitRates") + 
  scale_fill_manual(values =c("goldenrod4", "goldenrod3"))

##Month vs Bouncerate wrt Revenue
ggplot(onlinedata, aes(x=Month, y=BounceRates, fill = Revenue)) + scale_y_continuous(labels = scales::comma) +
  geom_boxplot() + geom_smooth(method="loess", se=F) +
  labs( x="Month", y="BounceRates") + 
  scale_fill_manual(values =c("tan2", "tan"))

##visitortype vs BounceRates wrt Revenue
ggplot(onlinedata, aes(x=VisitorType, y=BounceRates, fill = Revenue)) + scale_y_continuous(labels = scales::comma) +
  geom_boxplot() + geom_smooth(method="loess", se=F) +
  labs( x="VisitorType", y="BounceRates") + 
  scale_fill_manual(values =c("firebrick4", "firebrick3"))
grid.arrange(plotvp, plotMp, plotMe,plotMb, plotVb, nrow = 3, col = 3)


## VisitorTpye vs ExitRates wrt Revenue
ggplot(onlinedata, aes(x=VisitorType, y=ExitRates, fill = Revenue)) + scale_y_continuous(labels = scales::comma) +
  geom_boxplot() + geom_smooth(method="loess", se=F) +
  labs( x="VisitorType", y="ExitRates") + 
  scale_fill_manual(values =c("gold3", "gold2"))

##correlation
library(ggplot2)
library(ggcorrplot)
library(RColorBrewer)
ggcorr(onlinedata[, 1:10], method=c("everything", "pearson"), 
       geom = "tile",
       max_size = 10,
       min_size = 2,
       size = 3,
       hjust = 0.90,
       nbreaks = 6,
       angle = -45,
       layout.exp = 2,
       label = TRUE,
       palette = "GnBu")


##Z-score standardization of numeric variables
par(mfrow = c(1,1))
boxplot(onlinedata$Administrative, xlab="Administrative", col = "peachpuff2")
boxplot(onlinedata$Administrative_Duration, xlab="Administrative_Duartion", col = "peachpuff2")
boxplot(onlinedata$Informational, xlab="Informational", col = "peachpuff2")
boxplot(onlinedata$Informational_Duration, xlab="Informational_Duration", col = "peachpuff2")
boxplot(onlinedata$ProductRelated, xlab="ProductedRelated", col = "peachpuff2")
boxplot(onlinedata$ProductRelated_Duration, xlab="Administrative", col = "peachpuff2")
boxplot(onlinedata$BounceRates, xlab="BounceRates", col = "peachpuff2")
boxplot(onlinedata$ExitRates, xlab="ExitRates", col = "peachpuff2")
boxplot(onlinedata$SpecialDay, xlab="SpecialDay", col = "peachpuff2")
boxplot(onlinedata$Browser, xlab="Browser", col = "peachpuff2")
boxplot(onlinedata$Region, xlab="Region", col = "peachpuff2")
boxplot(onlinedata$OperatingSystems, xlab="OperatingSystems", col = "peachpuff2")
boxplot(onlinedata$TrafficType, xlab="TafficType", col = "peachpuff2")
##--------------------------------------ModelBuild---------------------------------------------##
##Data Transformation(converting categorical to numeric)
library(plyr)
library(purrr)
onlinedata$Revenue <- as.factor(onlinedata$Revenue)
onlinedata$Weekend <- as.factor(onlinedata$Weekend)
onlinedata$VisitorType <- as.factor(onlinedata$VisitorType)
str(onlinedata)
##Dividing the dataset
set.seed(100)
train <- sample(nrow(onlinedata), 0.7*nrow(onlinedata), replace = FALSE)
trainset <- onlinedata[train,]
testset <- onlinedata[-train,]
summary(trainset)
summary(testset)
dim(trainset)
dim(testset)
##------------------------Models------------------------------
attach(onlinedata)
##Logistic Regression
logisticmodel <- glm(Revenue ~ ., data = trainset, family = "binomial")
summary(logisticmodel)
sum(predicted ==  TRUE & actual == TRUE) / sum(actual == TRUE) 
sum(predicted == FALSE & actual == FALSE) / sum(actual == FALSE)
plot(logisticmodel)
predict.Probability <- predict(logisticmodel, type = "response")
threshold <- 0.5
predicted <- ifelse(predict.Probability > threshold, 1,0)
actual <- trainset$Revenue
sum(actual == predicted)/nrow(trainset) ## 88% model accuracy
##Testing accuracy
predict.Probability <- predict(logisticmodel, type = "response", newdata = testset)
predicted <- ifelse(predict.Probability > threshold, 1,0)
actual <- testset$Revenue
sum(actual == predicted)/nrow(testset)

##Classification
summary(onlinedata)
library(rpart)
library(rpart.plot)
treemodel <- rpart(Revenue ~ . , data = trainset, method = "class")
rpart.plot(treemodel)
library(rattle)
fancyRpartPlot(treemodel)
printcp(treemodel)
pred.train <- predict(treemodel,trainset,type = "class")
mean(pred.train == trainset$Revenue) ## 0.904 model accuracy
summary(test)
summary(train)
pred.test <- predict(treemodel, testset, type = "class")
mean(pred.test == testset$Revenue) ##0.906 testing set accuracy
tabular <- table(pred.train, trainset$Revenue)
tabular
table(train$Revenue)

##Ensemble Methods
##Randomforest
attach(onlinedata)
library(randomForest)
class(onlinedata$Revenue)
Model_Forest<- randomForest(Revenue ~., data = trainset, mtry = 4)
Model_Forest
print(Model_Forest)
  predTrain <- predict(Model_Forest, trainset, type = "class")
  table(predTrain, trainset$Revenue)
mean(predTrain == trainset$Revenue)
predtest <- predict(Model_Forest, testset, type = "class")
table(predtest, testset$Revenue)
mean(predtest == testset$Revenue)
matrix <- confusionMatrix(predTrain, factor(trainset$Revenue))
matrix

##Boosting Model
library(ada)
BoostingModel <- ada(Revenue ~ ., data = trainset)
print(BoostingModel)
predTrainB <- predict(BoostingModel, newdata = trainset)
mean(predTrainB == trainset$Revenue)
predTrainB <- predict(BoostingModel, newdata = testset)
mean(predTrainB == testset$Revenue)

##Bagging
library(ipred)
baggingmodel <- bagging(Revenue ~ ., data = trainset, coob = TRUE)
print(baggingmodel)
predicted<- predict(baggingmodel)
actual <- trainset$Revenue
sum(actual==predicted)/nrow(trainset)
predicted<- predict(baggingmodel, newdata = testset)
table(actual, predicted)
actual <- testset$Revenue
sum(actual==predicted)/nrow(testset)
##-------------------------------------End---------------------------------------------------------------













