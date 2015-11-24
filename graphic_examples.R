library(ggplot2)  ### input data should be data.frame
library(gcookbook)
require(gridExtra)
library(plyr) # needed for desc() 

#### case 1 plot dot
data(mtcars)
qplot(mtcars$wt, mtcars$mpg)  
qplot(wt, mpg, data=mtcars)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

##### all three above are plot same figure #############

#### case 2 plot line
data(pressure)
#### regular R graphic
plot(pressure$temperature, pressure$pressure, type="l")
points(pressure$temperature, pressure$pressure)
lines(pressure$temperature, pressure$pressure/2, col="red")
points(pressure$temperature, pressure$pressure/2, col="red")

#### gglot2 graphic 
qplot(pressure$temperature, pressure$pressure, geom="line")
qplot(temperature, pressure, data=pressure, geom="line")
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() ### the same as above
# Lines and points together
qplot(temperature, pressure, data=pressure, geom=c("line", "point"))
# Equivalent to:
ggplot(pressure, aes(x=temperature, y=pressure)) + geom_line() + geom_point()


#### case 3 plot bar
data(BOD)
barplot(BOD$demand, names.arg=BOD$Time) 

table(mtcars$cyl)
# Generate a table of counts
barplot(table(mtcars$cyl))     
#### regular graphic above

qplot(BOD$Time, BOD$demand, geom="bar", stat="identity")
# Convert the x variable to a factor, so that it is treated as discrete
qplot(factor(BOD$Time), BOD$demand, geom="bar", stat="identity")

# cyl is continuous here
qplot(mtcars$cyl)
# Treat cyl as discrete
qplot(factor(mtcars$cyl))

grid.arrange(qplot(mtcars$cyl), qplot(factor(mtcars$cyl)), ncol=2) # plot two figures side by side

#### case 4 plot histogram

hist(mtcars$mpg)
# Specify approximate number of bins with breaks
hist(mtcars$mpg, breaks=10)
qplot(mtcars$mpg)
qplot(mpg, data=mtcars, binwidth=4)
# This is equivalent to:
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth=4)

#### case 5 boxplot
data(ToothGrowth)
plot(ToothGrowth$supp, ToothGrowth$len)
#Formula syntax
boxplot(len ~ supp, data = ToothGrowth)
# Put interaction of two variables on x-axis
boxplot(len ~ supp + dose, data = ToothGrowth)

qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")
qplot(supp, len, data=ToothGrowth, geom="boxplot")
# This is equivalent to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
#Using three separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom="boxplot")
# Alternatively, get the columns from the data frame
qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")
# This is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()


#### case 6 function curve

curve(x^3 - 5*x, from=-4, to=4) # R regular plot

# Plot a user-defined function
myfun <- function(xvar) { 1/(1 + exp(-xvar + 10))
}
curve(myfun(x), from=0, to=20)
# Add a line:
curve(1-myfun(x), add = TRUE, col = "red")

# This sets the x range from 0 to 20  ## ggplot2 graphic
qplot(c(0,20), fun=myfun, stat="function", geom="line")
# This is equivalent to:
ggplot(data.frame(x=c(0, 20)), aes(x=x)) + stat_function(fun=myfun, geom="line")

##### long case 1 bar graph
data(pg_mean)
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
str(BOD)
ggplot(BOD, aes(x=Time, y=demand)) + geom_bar(stat="identity")
ggplot(BOD, aes(x=factor(Time), y=demand)) + geom_bar(stat="identity")

ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", fill="lightblue", colour="black")

data(cabbage_exp)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(position="dodge", stat="identity")
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(position="dodge", colour="black", stat="identity") + scale_fill_brewer(palette="Pastel1")

ce <- cabbage_exp[1:5, ] # Copy the data without last row
ce
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(position="dodge", colour="black", stat="identity") + scale_fill_brewer(palette="Pastel1")

data(diamonds)
ggplot(diamonds, aes(x=cut)) + geom_bar()
# Equivalent to using geom_bar(stat="bin")
ggplot(diamonds, aes(x=carat)) + geom_bar()

# colored bars
upc <- subset(uspopchange, rank(Change)>40)
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) + geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) + xlab("State")

# positive & negtive bars
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", position="identity")

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) + geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)  # guide=FALSE is for remove legend

g1<-ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity") # regular width of bar
g2<-ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.5) # narrower bars
g3<-ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=1) # wider bars
grid.arrange(g1, g2, g3, ncol=3)

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position="dodge")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))

# stacked bar

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar, order=desc(Cultivar))) +
  geom_bar(stat="identity") # need library(plyr)

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity", color="black") + guides(fill=guide_legend(reverse=TRUE)) + scale_fill_brewer(palette="Pastel1")

# Do a group-wise transform(), splitting on "Date"
ce <- ddply(cabbage_exp, "Date", transform, percent_weight = Weight / sum(Weight) * 100) # split based on Data
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) + geom_bar(stat="identity")
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) + geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=TRUE)) + scale_fill_brewer(palette="Pastel1")


# add labels in bar graph
# Below the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + geom_bar(stat="identity") + geom_text(aes(label=Weight), vjust=1.5, colour="white")
# Above the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + geom_bar(stat="identity") + geom_text(aes(label=Weight), vjust=-0.2)

# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + geom_bar(stat="identity") + geom_text(aes(label=Weight), vjust=-0.2) + ylim(0, max(cabbage_exp$Weight) * 1.05)

# Map y positions slightly above bar top - y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) + geom_bar(stat="identity") + geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity", position="dodge") + geom_text(aes(label=Weight), vjust=1.5, colour="white", position=position_dodge(.9), size=5)

#for add text in stack bar
# Sort by the day and sex columns
ce <- arrange(cabbage_exp, Date, Cultivar)
# Get the cumulative sum
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight))
ce 

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity") +
  geom_text(aes(y=label_y, label=Weight), vjust=1.5, colour="white")


ce <- arrange(cabbage_exp, Date, Cultivar)
# Calculate y position, placing it in the middle
ce <- ddply(ce, "Date", transform, label_y=cumsum(Weight)-0.5*Weight)
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity") + geom_text(aes(y=label_y, label=Weight), colour="white")
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) + geom_bar(stat="identity", colour="black") + geom_text(aes(y=label_y, label=paste(format(Weight, nsmall=2), "kg")), size=4)+ 
  guides(fill=guide_legend(reverse=TRUE)) + scale_fill_brewer(palette="Pastel1")

# Cleveland Dot Plot
tophit <- tophitters2001[1:25, ] # Take the top 25 from the tophitters data set
ggplot(tophit, aes(x=avg, y=name)) + geom_point()

ggplot(tophit, aes(x=avg, y=reorder(name, avg))) + geom_point(size=3) +                        # Use a larger dot
  theme_bw() + theme(panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank(),panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(tophit, aes(x=reorder(name, avg), y=avg)) + geom_point(size=3) +                        # Use a larger dot
  theme_bw() + theme(axis.text.x = element_text(angle=60, hjust=1), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))

# Get the names, sorted first by lg, then by avg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# Turn name into a factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels=nameorder)

ggplot(tophit, aes(x=avg, y=name)) + geom_segment(aes(yend=name), xend=0, colour="grey50") + geom_point(size=3, aes(colour=lg)) +
  scale_colour_brewer(palette="Set1", limits=c("NL","AL")) + 
  theme_bw() + theme(panel.grid.major.y = element_blank(),   # No horizontal grid lines
        legend.position=c(1, 0.55),             # Put legend inside plot area
        legend.justification=c(1, 0.5))

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey50") + geom_point(size=3, aes(colour=lg)) + scale_colour_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() + theme(panel.grid.major.y = element_blank()) + facet_grid(lg ~ ., scales="free_y", space="free_y")


# line graph

ggplot(BOD, aes(x=Time, y=demand)) + geom_line()
BOD1 <- BOD # Make a copy of the data
BOD1$Time <- factor(BOD1$Time)
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()

# These have the same result
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + ylim(0, max(BOD$demand))
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + expand_limits(y=0)
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()
 

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()

library(gcookbook) # For the data set
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
# Same with a log y-axis
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() +
  scale_y_log10()

# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
# Map supp to colour
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()
# Map supp to linetype
ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line()

ggplot(tg, aes(x=factor(dose), y=length, colour=supp, group=supp)) + geom_line()
ggplot(tg, aes(x=dose, y=length)) + geom_line()

ggplot(tg, aes(x=dose, y=length, shape=supp)) + geom_line() +
  geom_point(size=4)           # Make the points a little larger
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() +
  geom_point(size=4, shape=21) # Also use a point with a color fill
￼
ggplot(tg, aes(x=dose, y=length, shape=supp)) +
  geom_line(position=position_dodge(0.2)) +         # Dodge lines by 0.2
  geom_point(position=position_dodge(0.2), size=4)  # Dodge points by 0.2

ggplot(BOD, aes(x=Time, y=demand)) + geom_line(linetype="dashed", size=1, colour="blue")

# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line() + scale_colour_brewer(palette="Set1")

# If both lines have the same properties, you need to specify a variable to
# use for grouping
ggplot(tg, aes(x=dose, y=length, group=supp)) + geom_line(colour="darkgreen", size=1.5)
# Since supp is mapped to colour, it will automatically be used for grouping
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line(linetype="dashed") + geom_point(shape=22, size=3, fill="white")

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point(size=4, shape=22, colour="darkred", fill="pink")

ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point(size=4, shape=21, fill="white")

# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
# Save the position_dodge specification because we'll use it multiple times
pd <- position_dodge(0.2)
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line(position=pd) + geom_point(shape=21, size=3, position=pd) + scale_fill_manual(values=c("black","white"))

# Convert the sunspot.year data set into a data frame for this example
sunspotyear <- data.frame(Year = as.numeric(time(sunspot.year)), Sunspots = as.numeric(sunspot.year))
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area()
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area(colour="black", fill="blue", alpha=.2)
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area(fill="blue", alpha=.2) + geom_line()

library(gcookbook) # For the data set
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

library(plyr) # For the desc() function
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues")


ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
  geom_area(colour=NA, alpha=.4) +
  scale_fill_brewer(palette="Blues") +
  geom_line(position="stack", size=.2)

library(gcookbook) # For the data set
library(plyr)      # For the ddply() function
# Convert Thousands to Percent
uspopage_prop <- ddply(uspopage, "Year", transform, Percent = Thousands / sum(Thousands) * 100)
ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

library(gcookbook) # For the data set
# Grab a subset of the climate data
clim <- subset(climate, Source == "Berkeley", select=c("Year", "Anomaly10y", "Unc10y"))

# Shaded region
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.2)+geom_line()

# With a dotted line for upper and lower bounds
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_line(aes(y=Anomaly10y-Unc10y), colour="grey50", linetype="dotted") +
  geom_line(aes(y=Anomaly10y+Unc10y), colour="grey50", linetype="dotted") +
  geom_line()

# scatter plots 






