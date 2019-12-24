library(tidyverse)

setwd("C:/Users/lucio/Documents/School/Classes/STAT/STAT 382/Final Proj")
temp = list.files(pattern = ".csv")
list2env(
  lapply(setNames(temp, 
                  make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

sum(is.na(CO))
sum(is.na(Commercial_buildings))
sum(is.na(Industry_values))
sum(is.na(NO2))
sum(is.na(O3))
sum(is.na(Pb))
sum(is.na(pm10))
sum(is.na(pm2.5))
sum(is.na(Residential_building))
sum(is.na(SO2))
sum(is.na(vehicle.population))

mergedGases <- Reduce(function(x,y) merge(x,y, by="year")
                      ,list(CO, NO2, O3, SO2))
mergedGases

summary(mergedGases)
sd(mergedGases$CO_mean)
sd(mergedGases$NO2_mean)
sd(mergedGases$O3_mean)
sd(mergedGases$SO2_mean)

#plot(mergedGases$year, mergedGases$CO_mean,
#     type = "o", col = "red")
#plot(mergedGases$year, mergedGases$NO2_mean,
#     type = "o", col = "blue")
#plot(mergedGases$year, mergedGases$O3_mean, 
#     type = "o", col = "green")
#plot(mergedGases$year, mergedGases$SO2_mean, 
#     type = "o", col = "Orange")

gaspm <- Reduce(function(x,y) merge(x,y, by ="year"),
                list(mergedGases, pm10, pm2.5))
gaspm

summary(gaspm$pm10_mean)
summary(gaspm$pm2.5_mean)
sd(gaspm$pm10_mean)
sd(gaspm$pm2.5_mean)

sepindustry <- split(Industry_values, Industry_values$product_type)
#sepindustry[1]

sepcombuild <- split(Commercial_buildings, Commercial_buildings$building_type)
#sepcombuild[1]

sepresbuild <- split(Residential_building, Residential_building$building_type)
#sepresbuild[1]

sepvehiclepop <- split(vehicle.population, vehicle.population$car_type)
#sepvehiclepop[1]


scaledpoll <- as.data.frame(apply(gaspm[,-c(1)], 2, function(x) (x - min(x))/(max(x)-min(x))))

#scaledpol <- scale(gaspm[,-c(1)])
#colMeans(scaledpol)
#apply(scaledpol, 1, sd)
#scaledpoll <- data.frame(scaledpol)
#x <- 2002:2014
#scaledpoll$year <- x

scaledpoll$year <- as.factor(gaspm$year)
scaledpoll$CO_mean <- as.factor(scaledpoll$CO_mean)
scaledpoll$NO2_mean <- as.factor(scaledpoll$NO2_mean)
scaledpoll$O3_mean <- as.factor(scaledpoll$O3_mean)
scaledpoll$SO2_mean <- as.factor(scaledpoll$SO2_mean)
scaledpoll$pm10_mean <- as.factor(scaledpoll$pm10_mean)
scaledpoll$pm2.5_mean <- as.factor(scaledpoll$pm2.5_mean)

ggplot(scaledpoll, aes(x = year, group = 0)) +
#  theme_bw() +
  geom_line(aes(y = CO_mean, colour = "CO"), size = 1) +
  geom_line(aes(y = NO2_mean, colour = "NO2"), size = 1) +
  geom_line(aes(y = O3_mean, colour = "O3"), size = 1) +
  geom_line(aes(y = SO2_mean, colour = "SO2"), size = 1) +
  geom_line(aes(y = pm10_mean, colour = "pm10"), size = 1) +
  geom_line(aes(y = pm2.5_mean, colour = "pm2.5"), size = 1) +
  scale_y_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ylab("Means") 

indusvalue <- data.frame(sepindustry$`Basic Metal`$year)
indusvalue$`Basic Metal` <- sepindustry$`Basic Metal`$values
indusvalue$`Chemicals & Chemical Products` <- sepindustry$`Chemicals & Chemical Products`$values
indusvalue$`Computer, Electronic & Optical Products` <- sepindustry$`Computer, Electronic & Optical Products`$values
indusvalue$`Electrical Equipment` <- sepindustry$`Electrical Equipment`$values
indusvalue$`Fabricated Metal Products` <- sepindustry$`Fabricated Metal Products`$values
indusvalue$`Food, Beverage & Tobacco` <- sepindustry$`Food, Beverage & Tobacco`$values
indusvalue$Furniture <- sepindustry$Furniture$values
indusvalue$`Leather, Leather Products & Footwear` <- sepindustry$`Leather, Leather Products & Footwear`$values
indusvalue$`Machinery & Equipment` <- sepindustry$`Machinery & Equipment`$values
indusvalue$`Motor Vehicles, Trailers & Semi-trailers` <- sepindustry$`Motor Vehicles, Trailers & Semi-trailers`$values
indusvalue$`Non-metallic Mineral Products` <- sepindustry$`Non-metallic Mineral Products`$values
indusvalue$`Other Manufacturing Industries` <- sepindustry$`Other Manufacturing Industries`$values
indusvalue$`Other Transport Equipment` <- sepindustry$`Other Transport Equipment`$values
indusvalue$`Paper & Paper Products` <- sepindustry$`Paper & Paper Products`$values
indusvalue$`Pharmaceutical & Biological Products` <- sepindustry$`Pharmaceutical & Biological Products`$values
indusvalue$`Printing & Reproduction Of Recorded Media` <- sepindustry$`Printing & Reproduction Of Recorded Media`$values
indusvalue$`Refined Petroleum Products` <- sepindustry$`Refined Petroleum Products`$values
indusvalue$`Rubber & Plastic Products` <- sepindustry$`Rubber & Plastic Products`$values
indusvalue$Textiles <- sepindustry$Textiles$values
indusvalue$`Wearing Apparel` <- sepindustry$`Wearing Apparel`$values
indusvalue$`Wood & Wood Products` <- sepindustry$`Wood & Wood Products`$values

colnames(indusvalue)[colnames(indusvalue) == "sepindustry..Basic.Metal..year"] <- "year"
#scaledIndus <- as.data.frame(apply(indusvalue[,-c(1)], 2, function(x) (x - min(x))/(max(x)-min(x))))
#results <- combn(names(indusvalue), 2, function(x){coefficients(lm(indusvalue[, x]))}, simplify = FALSE)
#vars <- combn(names(indusvalue), 2)
#names(results) <- vars[1 , ]
#results
induspol <- merge(gaspm, indusvalue, by = "year")

corlist <- cor(induspol)
corlist2 <- data.frame(corlist[2:7, 8:28])

corlist3 <- corlist2 %>%
  #filter_all(any_vars(abs(.) > 0.67))
  select_if(funs(any(abs(.) > 0.6)))

corlist3

#basic metal neg cor s02
#electric equipment neg cor No2 and so2
#fabricated metals neg cor so2 and pm2.5
#textiles pos cor so2
#rubber plastic pos cor pm2.5

#################

MetalLM <- lm(formula = induspol$`Basic Metal`~ induspol$SO2_mean-1)
summary(MetalLM)
MetalRS <- residuals(MetalLM)
plot(MetalRS)
ggplot(induspol, aes(x = induspol$SO2_mean, y = induspol$`Basic Metal`)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Regression for Basic Metal and SO2")

#################

ElectricELM <-lm(formula = induspol$`Electrical Equipment`~ induspol$NO2_mean+induspol$SO2_mean-1)
summary(ElectricELM)
ElectricERS<-residuals(ElectricELM)
plot(ElectricERS)
ggplot(induspol, aes(x = induspol$SO2_mean, y = induspol$`Electrical Equipment`)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Regression for Electrical Equipment and SO2")

ggplot(induspol, aes(x = induspol$NO2_mean, y = induspol$`Electrical Equipment`)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Regression for Electrical Equipment and NO2")

#plot(induspol$CO_mean,induspol$`Computer, Electronic & Optical Products`)

#################

FabricateLM <- lm(formula = induspol$`Fabricated Metal Products`~ induspol$SO2_mean+induspol$pm2.5_mean-1)
summary(FabricateLM)
FabricateRS <- residuals(FabricateLM)
plot(FabricateRS)
ggplot(induspol, aes(x = induspol$SO2_mean, y = induspol$`Fabricated Metal Products`)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Regression for Fabricated Metal Products and SO2")

ggplot(induspol, aes(x = induspol$pm2.5_mean, y = induspol$`Fabricated Metal Products`)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Regression for Fabricated Metal Products and pm2.5")

#################

textileLM <- lm(formula = induspol$Textiles~ induspol$SO2_mean-1)
summary(textileLM)
textileRS <- residuals(textileLM)
plot(textileRS)
ggplot(induspol, aes(x = induspol$SO2_mean, y = induspol$Textiles)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Regression for Textiles and SO2")

#################

rubberLM <- lm(formula = induspol$`Rubber & Plastic Products`~ induspol$pm2.5_mean-1)
summary(rubberLM)
rubberRS <- residuals(rubberLM)
plot(rubberRS)
ggplot(induspol, aes(x = induspol$pm2.5_mean, y = induspol$`Rubber & Plastic Products`)) +
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Regression for Rubber & Plastic Products and pm2.5")
#################

scaledinduspoll <- as.data.frame(apply(induspol[,-c(1)], 2, function(x) (x - min(x))/(max(x)-min(x))))

scaledinduspoll$year <- as.factor(induspol$year)
scaledinduspoll$SO2_mean <- as.factor(scaledinduspoll$SO2_mean)
scaledinduspoll$NO2_mean <- as.factor(scaledinduspoll$NO2_mean)
scaledinduspoll$pm2.5_mean <- as.factor(scaledinduspoll$pm2.5_mean)
scaledinduspoll$`Basic Metal` <- as.factor(scaledinduspoll$`Basic Metal`)
scaledinduspoll$`Electrical Equipment` <- as.factor(scaledinduspoll$`Electrical Equipment`)
scaledinduspoll$`Fabricated Metal Products` <- as.factor(scaledinduspoll$`Fabricated Metal Products`)
scaledinduspoll$Textiles <- as.factor(scaledinduspoll$Textiles)
scaledinduspoll$`Rubber & Plastic Products`<- as.factor(scaledinduspoll$`Rubber & Plastic Products`)

ggplot(scaledinduspoll, aes(x = scaledinduspoll$Textiles, group = 1)) +
  #geom_line(aes(y=scaledinduspoll$Textiles, colour = "Textiles"), size = 1.5) +
  geom_line(aes(y=scaledinduspoll$SO2_mean, colour = "SO2"), size = 1.5) +
  scale_y_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ggtitle("SO2 output from Textile Production") +
  scale_x_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ylab("Output change")+
  xlab("")

ggplot(scaledinduspoll, aes(x = scaledinduspoll$`Rubber & Plastic Products`, group = 1)) +
  #geom_line(aes(y = scaledinduspoll$`Rubber & Plastic Products`,
  #              colour = "Rubber & Plastic Products"), size = 1.5) +
  geom_line(aes(y = scaledinduspoll$pm2.5_mean,
                colour = "pm2.5"), size = 1.5) +
  scale_y_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ggtitle("pm2.5 output from Rubber & Plastic Products") +
  scale_x_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ylab("Output change")+
  xlab("Output of Rubber & Plastic Products Produced")

ggplot(scaledinduspoll, aes(x = scaledinduspoll$`Electrical Equipment`, group = 1)) +
  #geom_line(aes(y = scaledinduspoll$`Electrical Equipment`,
  #              colour = "Electrical Equipment"), size = 1.5) +
  geom_line(aes(y = scaledinduspoll$NO2_mean, colour = "NO2"), size = 1.5) +
  geom_line(aes(y = scaledinduspoll$SO2_mean, colour = "SO2"), size = 1.5) +
  scale_y_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ggtitle("NO2 & SO2 output from Electrical Equipment") +
  scale_x_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ylab("Output change")+
  xlab("Output of Electric Equipment Produced")


ggplot(scaledinduspoll, aes(x = scaledinduspoll$`Fabricated Metal Products`, group = 1)) +
  #geom_line(aes(y = scaledinduspoll$`Fabricated Metal Products`,
  #              colour = "Fabricated Metal Products"), size = 1.5) +
  geom_line(aes(y = scaledinduspoll$pm2.5_mean, colour = "pm2.5"), size = 1.5) +
  geom_line(aes(y = scaledinduspoll$SO2_mean, colour = "SO2"), size = 1.5) +
  scale_y_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ggtitle("pm2.5 & SO2 from Fabricated Metal Products") +
  scale_x_discrete(breaks = seq(from=0, to=1, by=0.1)) +
  ylab("Output change")+
  xlab("Output of Fabricated Metal Products Produced")
