dev.off()
setwd("C:/Users/True Gamer/OneDrive/Desktop/MyProject")
options(scipen = 999)
library(ggplot2)
library(corrplot)
library(ellipse)
data <- read.csv("clean_missing_aviation.csv")
names(data)
data$Educational.Attainment <- as.numeric(data$Educational.Attainment)
data$Aircraft.Movements <- as.numeric(data$Aircraft.Movements)
data$Economic <- as.numeric(data$Economic)
data$Official.Language <- as.character(data$Official.Language)
data$Pop.in.1972 <- as.numeric(data$Pop.in.1972)
data$Pop.in.2022 <- as.numeric(data$Pop.in.2022)
data$Continent <- as.factor(data$Continent)

summary(data)
for(i in 3:10){
  data[is.na(data[,i]),i] <- median(data[,i],na.rm=TRUE)
}

str(data)

col <- colnames(data)[3:10]
str(col)

par(mfrow = c(4,4))
for (i in 1:length(col)){
  sub_data = data[col[i]][,1]
  hist(sub_data, main = paste("Hist of",col[i], sep = " "),xlab = col[i])
  qqnorm(sub_data, main = paste("QQ plot of", col[i], sep = " "))
  qqline(sub_data)

}

#checking for outliers#
out <- boxplot.stats(data$Economic)$out
out_ind_eco <- which(data$Economic %in% c(out))
data[out_ind_eco,]
boxplot(data$Economic, ylab="Economy",main="Boxplot of Economy") 
mtext(paste("Outliers: ", paste(out_ind_eco, collapse = ",")))

out <- boxplot.stats(data$Landmass)$out
out_ind_land <- which(data$Landmass %in% c(out))
data[out_ind_land,]
boxplot(data$Landmass, ylab="Landmass",main="Boxplot of Landmass") 
mtext(paste("Outliers: ", paste(out_ind_land, collapse = ",")))

out <- boxplot.stats(data$Pop.in.1972)$out
out_ind_pop1972 <- which(data$Pop.in.1972 %in% c(out))
data[out_ind_pop1972,]
boxplot(data$Landmass, ylab="Population in 1972",main="Boxplot of Population 1972") 
mtext(paste("Outliers: ", paste(out_ind_pop1972, collapse = ",")))

out <- boxplot.stats(data$Pop.in.2022)$out
out_ind_pop2022 <- which(data$Pop.in.2022 %in% c(out))
data[out_ind_pop2022,]
boxplot(data$Pop.in.2022, ylab="Population in 2022",main="Boxplot of Population 2022") 
mtext(paste("Outliers: ", paste(out_ind_pop2022, collapse = ",")))

out <- boxplot.stats(data$Educational.Attainment)$out
out_ind_edu <- which(data$Educational.Attainment %in% c(out))
out_ind_edu
data[out_ind_edu,]
boxplot(data$Educational.Attainment, ylab="Educational Attainment",main="Boxplot of Educational Attainment") 
mtext(paste("Outliers: ", paste(out_ind_edu, collapse = ",")))

out <- boxplot.stats(data$Aircraft.Movements)$out
out_ind_air <- which(data$Aircraft.Movements %in% c(out))
out_ind_air
data[out_ind_air,]
boxplot(data$Aircraft.Movements, ylab="Aircraft Movements",main="Boxplot of Aircraft Movements") 
mtext(paste("Outliers: ", paste(out_ind_air, collapse = ",")))

#End of checking Outliers#

#Start of data visualization
number_continent <- ggplot(data, aes(x = Continent)) + geom_bar(fill = "lightblue") + ggtitle("Number of states in each Continent") + xlab("Continent") + ylab("Number of States")
number_continent

state_in_continent <- ggplot(data,aes(x=State.Abbreviations)) + geom_bar(aes(fill=Continent)) + ggtitle("Name of  States in Each Continent") + xlab("States") + ylab("Count")
state_in_continent

pop_state <- ggplot(data, aes(x=Pop.in.1972,y=Pop.in.2022)) + geom_col(aes(color="States"),position= "dodge")  + ggtitle("Population in 1972 Vs Population in 2022") + xlab("Population in 1972") + ylab("Population in 2022") + facet_wrap(.~States,scales="free_y",nrow=6) + scale_x_log10()
pop_state

land_state <- ggplot(data, aes(x=Landmass,y=Pop.in.2022)) + geom_col(aes(color="States"),position= "dodge")  + ggtitle("Landmass Vs Population for each state") + xlab("Landmass") + ylab("Population in 2022") + facet_wrap(.~States,scales="free_y",nrow=6)
land_state
dev.off()

num_col <- data[, 3:10]
corMatrix <- cor(as.matrix(num_col))
col <- colorRampPalette(c("red","green","blue"))
corrplot.mixed(corMatrix,order="AOE",tl.pos = "lt", lower="number",lower.col="black", upper="ellipse",upper.col=col(10),number.cex=0.8,tl.col="black")

model <- aov(data$Landmass ~ data$Pop.in.2022, data)
summary(model)

data$pop.density <- data$Pop.in.2022/data$Landmass

ggplot(data,aes(x=State.Abbreviations,y=pop.density)) + geom_col() + xlab("States") + ylab("Population Density")

model1 <- aov(data$pop.density ~ data$State.Abbreviations, data)
summary(model1)

data[,c("States","Life.Expectancy","Infant.Mortality")]

ggplot(data, aes(x=Continent, y=Life.Expectancy,fill=Continent)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1)

sub_data <- subset(data,Continent == "Africa" | Continent=="Asia"|Continent=="Oceania", select=c(State.Abbreviations, Life.Expectancy,Infant.Mortality, Continent))

sub_data$Continent <- as.factor(sub_data$Continent)
str(sub_data)

ggplot(sub_data,aes(x=State.Abbreviations,y=Life.Expectancy)) + geom_point(aes(color=Continent,size=8))

model2 <- aov(data$Life.Expectancy ~ data$Continent, data)
summary(model2)

model3 <- aov(sub_data$Life.Expectancy ~ sub_data$Continent, sub_data)
summary(model3)

model4 <- aov(data$Infant.Mortality ~ data$Continent, data)
summary(model4)

ggplot(data, aes(x=Continent, y=Infant.Mortality,fill=Continent)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1)

ggplot(sub_data,aes(x=State.Abbreviations,y=Infant.Mortality)) + geom_point(aes(color=Continent,size=8))

life_edu <- ggplot(data, aes(x=Life.Expectancy,y=Educational.Attainment)) + geom_point()  + ggtitle("Life Expectancy vs Educational Attainment for each State") + xlab("life Expectancy") + ylab("Educational Attainment") + facet_wrap(.~States,scales="free_y",nrow=6)
life_edu

model31 <- aov(data$Life.Expectancy ~ data$Educational.Attainment, data)
summary(model31)

ggplot(data,aes(x=Infant.Mortality,y=Life.Expectancy)) + geom_point(aes(size=Educational.Attainment,color=Continent))

edu_pop <- ggplot(data, aes(x=pop.density,y=Educational.Attainment,color=Continent)) + geom_point()  + ggtitle("Educational attainment per population area in each state and continent") + xlab("Population Density") + ylab("Educational Attainment") + facet_wrap(.~States,scales="free_y",nrow=6)
edu_pop

edu_eco <- ggplot(data, aes(x=Educational.Attainment,y=Economic,color=Continent)) + geom_point()  + ggtitle("Relationship between educational attainment and economy") + xlab("Educational Attainment(>25) that have minimum bacchelors") + ylab("Economy") + facet_wrap(.~States,scales="free_y",nrow=6)
edu_eco

edu_air <- ggplot(data, aes(x=Educational.Attainment,y=Aircraft.Movements,color=Continent)) + geom_point()  + ggtitle("Relationship between educational attainment and aricraft movements") + xlab("Educational Attainment(>25) that have minimum bacchelors") + ylab("Aircraft Movements") + facet_wrap(.~States,scales="free_y",nrow=6)
edu_air

edu_pop_area <- ggplot(data, aes(x=Educational.Attainment,y=pop.density,color=Continent)) + geom_point()  + ggtitle("Relationship between educational attainment per population area") + xlab("Educational Attainment(>25) that have minimum bacchelors") + ylab("Population per area") + facet_wrap(.~States,scales="free_y",nrow=6)
edu_pop_area

air_state <- ggplot(data,aes(x=State.Abbreviations,y=Aircraft.Movements)) + geom_col(fill="lightblue") + ggtitle("Aircraft Movements per state") + xlab("State Abbreviations") + ylab("Aircraft Movements")
air_state

air_state_con <- ggplot(data,aes(x=State.Abbreviations,y=Aircraft.Movements,fill=Continent)) + geom_col() + ggtitle("Aircraft Movements per state") + xlab("State Abbreviations") + ylab("Aircraft Movements")
air_state_con

ggplot(data, aes(x=Continent, y=Aircraft.Movements,fill=Continent)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1)

model4 <- aov(data$Aircraft.Movements ~ data$Continent, data)
summary(model4)

ggplot(data,aes(x=State.Abbreviations,y=Aircraft.Movements)) + geom_point(aes(size=pop.density)) + ggtitle("Aircraft movements per population area per state") + xlab("States") + ylab("Aircraft Movements")
ggplot(data,aes(x=State.Abbreviations,y=Economic)) + geom_col(aes(fill=Continent)) + ggtitle("Economy per state") + xlab("States") + ylab("Economy")

sub_state <- subset(data,State.Abbreviations=="BER"|State.Abbreviations=="FI"|State.Abbreviations=="IM"|State.Abbreviations=="NIU"|State.Abbreviations=="SAH"|State.Abbreviations=="TU",select=c(State.Abbreviations,Economic,Continent))
sub_state

sub_state_con <- ggplot(sub_state,aes(x=State.Abbreviations,y=Economic)) + geom_col(aes(fill=Continent)) + ggtitle("States that have the least economy") + xlab("States") + ylab("Economy")
sub_state_con

ggplot(data,aes(x=Aircraft.Movements,y=Economic)) + geom_point(aes(shape=Continent,size=2)) + ggtitle("Relation between Economic and Aircraft Movements") + xlab("Aircraft Movements") + ylab("Economy") + facet_wrap(.~States,scales="free_y",nrow=6)

data$Official.Language <- as.factor(data$Official.Language)

data$Official.Language

ggplot(data,aes(x=State.Abbreviations,y=Pop.in.2022)) + geom_col(aes(fill=Official.Language)) + ggtitle("Official Language spoken vs Population") + xlab("Language") + ylab("Population")

#End of Data Visualization#