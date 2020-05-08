#Import in the csv file into R (it may have a max size restriction)
rawTable <- read.csv("raws.csv")
#Extract table with property class and its value
classVsValue <- rawTable[,c("CLASSDSCRP","CNTASSDVAL")]
#Get the mean value of each property class
propClassMean <- aggregate(classVsValue[,2], list(classVsValue$CLASSDSCRP),
mean, na.rm = TRUE, na.action = NULL)
#Similar for other functions
propClassVar <- aggregate(classVsValue[,2], list(classVsValue$CLASSDSCRP),
var, na.rm = TRUE)
propClassSD <- aggregate(classVsValue[,2], list(classVsValue$CLASSDSCRP), sd,
na.rm = TRUE)
propClassMedi <- aggregate(classVsValue[,2], list(classVsValue$CLASSDSCRP),
median, na.rm = TRUE, na.action = NULL)
propClassMax <- aggregate(classVsValue[,2], list(classVsValue$CLASSDSCRP),
max, na.rm = TRUE, na.action = NULL)
propClassMin <- aggregate(classVsValue[,2], list(classVsValue$CLASSDSCRP),
min, na.rm = TRUE, na.action = NULL)
#define a mode function
Mode <- function(x) {
     ux <- unique(x)
     ux[which.max(tabulate(match(x, ux)))]
}
propClassMode <- aggregate(classVsValue[,2], list(classVsValue$CLASSDSCRP),
Mode)
#Create a visual model of the raw data
png("rawPlot.png")
plot(classVsValue$CLASSDSCRP, classVsValue$CNTASSDVAL, main="Property Classes
and their Values", xlab="Property Class", ylab="Value (USD)")
dev.close()
#Calculate mean of all records
sampleMean <- mean(rawTable$CNTASSDVAL)
