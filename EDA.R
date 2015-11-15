## Libraries


library(ggplot2)


## Loading data


Blood.Train <- read.csv("9db113a1-cdbe-4b1c-98c2-11590f124dd8.csv")
Blood.Test <- read.csv("5c9fa979-5a84-45d6-93b9-543d1a0efc41.csv")
Blood.Train$Made.Donation.in.March.2007 <- factor(Blood.Train$Made.Donation.in.March.2007)
levels(Blood.Train$Made.Donation.in.March.2007) <- c("No", "Yes")

## Exploratory Data Analysis


pairs(Blood.Train[, -1])
# Seems like "Number.of.donations" and "Total.Volume.Donated" are strongly correlated

cor(Blood.Train[, -c(1, 6)])
# Actually, "Number.of.donations" and "Total.Volume.Donated" are perfectly correlated

ggplot(Blood.Train, aes(Number.of.Donations, Total.Volume.Donated..c.c..)) + geom_point()
# Maybe it would be useful to remove one of these variables in our model

summary(Blood.Train)
# No NA values which is good

ggplot(Blood.Train, aes(x = Months.since.Last.Donation)) + geom_histogram(binwidth = 2) + facet_grid(. ~ Made.Donation.in.March.2007)
# Globally, when a donation was made in March 2007, the donors waited less time to donate

ggplot(Blood.Train, aes(x = Number.of.Donations)) + geom_histogram(binwidth = 2) + facet_grid(. ~ Made.Donation.in.March.2007)

ggplot(Blood.Train, aes(x = Months.since.First.Donation)) + geom_histogram(binwidth = 2) + facet_grid(. ~ Made.Donation.in.March.2007)

ggplot(Blood.Train, aes(Made.Donation.in.March.2007, Months.since.Last.Donation)) + geom_boxplot()

ggplot(Blood.Train, aes(Made.Donation.in.March.2007, Number.of.Donations)) + geom_boxplot()

ggplot(Blood.Train, aes(Made.Donation.in.March.2007, Months.since.First.Donation)) + geom_boxplot()

