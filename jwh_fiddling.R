# use install.package("plyr") if you don't it installed already
library(plyr)
library(reshape2)
library(scales)
library(ggplot2)

# set working dir to data repository location
setwd("/path/to/car-reliability")

# read in the data we've been provided with four models over all years
# for which there's data; name the columns
data <- read.csv("./four-models.csv", header = F)
names(data) <- c("year", "make", "model", "miles", "trans", "engine")

# purge any oddball miles and 2014's where there are extremely few issues so far
# you may want to purge other years based on n per make/model/year combo
data <- data[data$miles > 10 & data$miles < 750000, ]
data <- data[data$year != 2014, ]

# I've munged the database dump I was provided with the miles and issues
# distributions of all vehicles in the LTQI dataset. Scroll all the way
# down to see the code that wrangled distribution.csv if you don't trust it!
dist_issues <- read.csv("./dist_issues.csv")
dist_miles <- read.csv("./dist_miles.csv")

# create a miles_cut column in data subset to match the distribution set
# uses the round_any() function from the plyr package
data$miles_cut <- round_any(data$miles, accuracy = 10000, f = ceiling)

data_year <- ddply(data, .(year, make, model), summarize,
                    trans = sum(trans, na.rm = T)/length(trans),
                    engine = sum(engine, na.rm = T)/length(engine))

data_miles <- ddply(data, .(make, model, miles_cut), summarize,
                    trans = sum(trans, na.rm = T)/length(trans),
                    engine = sum(engine, na.rm = T)/length(engine))

issues_year <- ddply(dist_issues, .(year), summarize,
                     rate = (trans + engine)/total)

data_year_merge <- merge(data_year, issues_year, by = "year", all.x = T)

data_year_melt <- melt(data_year_merge, id.vars = c("year", "make", "model"))
data_miles_melt <- melt(data_miles, id.vars = c("make", "model", "miles_cut"))


# year vs. issue rates + distribution
png("./plots/year-vs-issue-rate.png", res = 200, width = 1600, height = 900)
p <- ggplot() + geom_point(aes(x = year, y = value, colour = variable),
                           data_year_melt[data_year_melt$variable %in% c("trans", "engine"), ])
p <- p + geom_line(aes(x = year, y = value, colour = variable),
                           data_year_melt[data_year_melt$variable == "rate", ], colour = "black")
p <- p + theme_bw() + facet_grid(~model)
p <- p + scale_x_continuous("year", limits = c(1995, 2015))
p <- p + scale_y_continuous("rate of issues (%)", labels = percent)
p <- p + scale_colour_discrete("issue type")
p <- p + theme(axis.text.x = element_text(angle = 315, hjust = 0))
p
dev.off()

# mileage vs. issue rates + distribution
png("./plots/mileage-vs-issue-rate.png", res = 200, width = 1600, height = 900)
p <- ggplot(data_miles_melt, aes(x = miles_cut, y = value, colour = variable))
p <- p + geom_point() + theme_bw() + facet_grid(~model)
p <- p + scale_x_continuous("miles", breaks = seq(100000, 400000, by = 100000),
                            labels = paste0(1:4, "00k"))
p <- p + scale_y_continuous("rate of issues (%)", labels = percent)
p <- p + scale_colour_discrete("issue type")
p
dev.off()

# miles vs. distribution miles
data_miles_dist <- ddply(data, .(make, model, miles_cut), summarize,
                         model_count = length(miles_cut))

data_miles_dist <- ddply(data_miles_dist, .(make, model), mutate,
                         model_rate = model_count/sum(model_count))

dist_miles$freq <- dist_miles$count / sum(dist_miles$count)

data_miles_dist_merge <- merge(data_miles_dist, dist_miles,
                               by.x = "miles_cut", by.y = "miles",
                               all.x = T)

png("./plots/mileage-distribution-vs-ave.png", res = 200, width = 1600, height = 900)
p <- ggplot() + geom_line(aes(x = miles_cut, y = model_rate, colour = model, group = model),
                          data = data_miles_dist_merge)
p <- p + geom_line(aes(x = miles_cut, y = freq), data = data_miles_dist_merge, colour = "black", linetype = 2)
p <- p + theme_bw() + scale_x_continuous("miles", breaks = seq(0, 400000, by = 100000),
                                         labels = c(0, paste0(1:4, "00k")))
p <- p + scale_y_continuous("% of vehicles", labels = percent)
p
dev.off()
