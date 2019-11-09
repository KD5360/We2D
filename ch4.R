getwd()
setwd("C:/work")
getwd()
boston.housing.df <- read.csv("BostonHousing.csv")
head(boston.housing.df,9)
summary(boston.housing.df)

mean(boston.housing.df$CRIM)
sd(boston.housing.df$CRIM)
min(boston.housing.df$CRIM)
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM)
length(boston.housing.df$CRIM)

sum(is.na(boston.housing.df$CRIM))

data.frame(mean=sapply(boston.housing.df, mean),
            sd=sapply(boston.housing.df, sd),
            min=sapply(boston.housing.df, min),
            max=sapply(boston.housing.df, max),
            median=sapply(boston.housing.df, median),
             length=sapply(boston.housing.df, length),
             miss.val=sapply(boston.housing.df, function(x)
  sum(length(which(is.na(x))))))

round(cor(boston.housing.df),2)

table(boston.housing.df$CHAS)

boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM, c(1:9))

aggregate(boston.housing.df$MEDV, by=list(RM=boston.housing.df$RM.bin,CHAS=boston.housing.df$CHAS), FUN=mean)

library(reshape)

mlt <- melt(boston.housing.df, id=c("RM.bin","CHAS"), measure=c("MEDV"))
head(mlt,5)

cast(mlt,RM.bin~CHAS, subset=variable=="MEDV", margins = c("grand_row","grand_col"),mean)

library(ggmap)
install.packages("ggmap")
library(ggmap)

tbl <- table(boston.housing.df$CAT..MEDV, boston.housing.df$ZN)
prop.tbl <- prop.table(tbl,margin = 2)
barplot(prop.tbl, xlab = "ZN",ylab = "",yaxt="n",main = "Distribution of CAT.MEDV by ZN")
axis(2, at=(seq(0,1,0.2)), paste(seq(0,100,20),"%"))

cereals.df <- read.csv("Cereals.csv")

pcs <- prcomp(data.frame(cereals.df$calories,cereals.df$rating))
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores,5)
pcs
