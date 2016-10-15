setwd("C:/Users/Lydia/Documents/Stat159/hw03")
Advertising <- read.csv("~/Stat159/hw02/data/Advertising.csv")
library("xtable")

overall_regression <- lm(Sales~ TV + Radio + Newspaper, data = Advertising)
regression_tv <-lm(Sales~TV, data = Advertising)
regression_radio <-lm(Sales~Radio, data = Advertising)
regression_news <-lm(Sales~Newspaper, data = Advertising)

reg_sum_TV <- summary(regression_tv)
reg_sum_radio <- summary(regression_radio)
reg_sum_news <- summary(regression_news)

jpeg('./images/scatterplot-radio-sales.png')
plot(Advertising$Sales ~ Advertising$Radio)
dev.off()
jpeg('./images/scatterplot-newspaper-sales.png')
plot(Advertising$Sales ~ Advertising$Newspaper)
dev.off()
jpeg('./images/scatterplot-tv-sales.png')
plot(Advertising$Sales ~ Advertising$TV)
dev.off()


jpeg("./images/residual-plot.png")
plot(regression_object, which = c(1))
dev.off()
jpeg("./images/scale-location-plot.png")
plot(regression_object, which = c(2))
dev.off()
jpeg("./images/normal-qq-plot.png")
plot(regression_object, which = c(3))
dev.off()

save(overall_regression,
     regression_tv,
     regression_news,
     regression_radio,
     reg_sum_TV,
     reg_sum_radio,
     reg_sum_news,
     file = "./data/regression.RData")
