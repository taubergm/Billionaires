if (!require(waffle)) {
  install.packages("waffle", repos="http://cran.us.r-project.org")
}
library(waffle)
if (!require(data.table)) {
  install.packages("data.table", repos="http://cran.us.r-project.org")
}
library(data.table)
if (!require(wordcloud)) {
  install.packages("wordcloud", repos="http://cran.us.r-project.org")
}
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)

if (!require(dplyr)) {
  install.packages("dplyr", repos="http://cran.us.r-project.org")
}
library(dplyr)
if (!require(tidyverse)) {
  install.packages("tidyverse", repos="http://cran.us.r-project.org")
}
library(tidyverse)

workingDir = '/Users/mtauberg/projects/Billionaires'
csvName = "merged_billionaires_data.csv"
data_name = "billionaires"
setwd(workingDir)
# update this - https://docs.google.com/spreadsheets/d/1JDz59cSsV2qMkwq3NuMEbfELB8FMaQcqxptEtERhPQA/edit#gid=0
billionaires = read.csv(csvName)
americans =  read.csv('american_billionaires_data.csv')
americans = americans[!duplicated(americans[,c('name')], fromLast=FALSE),]


# GENDER
gender_stats = americans %>%
  count(gender_x) %>%
  # count creates a column called 'n'
  mutate(percent = n / sum(n) * 100)

pie(gender_stats$percent, labels = paste(c("Women", "Men"), sep =" ", round(gender_stats$percent), "%"))
      

# FOUNDERS
founder_stats = americans %>%
  count(founder) %>%
  # count creates a column called 'n'
  mutate(percent = n / sum(n) * 100)

pie(founder_stats$percent, labels = paste(c("Not a Founder", "Founded a Company"), sep =" ", round(founder_stats$percent), "%"))

americans$founder_tech = (americans$founder == "yes") & (americans$X2018_industry == "tech")
americans$founder_nontech = (americans$founder == "yes") & (americans$X2018_industry != "tech")
americans$nonfounder = (americans$founder == "no") 

founder_stats = c( round(length(americans$founder_tech[americans$founder_tech == TRUE])/nrow(americans)*100),
                       round(length(americans$founder_nontech[americans$founder_nontech == TRUE])/nrow(americans)*100),
                       round(length(americans$nonfounder[americans$nonfounder == TRUE])/nrow(americans)*100)
)
pie(founder_stats, labels = paste(c("Founder in Tech", "Founder Non-Tech", "Not a Founder"), 
                                      sep =" ", founder_stats, "%"))


americans$founder_boolean = (americans$founder == "yes") & (americans$major_wealth_inheritance == "no")
americans$inherited =  (americans$founder == "no") & (americans$major_wealth_inheritance == "yes")
americans$other = ((americans$founder == "no") & (americans$major_wealth_inheritance == "no")) | 
                ((americans$founder == "yes") & (americans$major_wealth_inheritance == "yes")) 

founder_stats = c( round(length(americans$founder_boolean[americans$founder_boolean == TRUE])/nrow(americans)*100, digits=0),
                   round(length(americans$inherited[americans$inherited == TRUE])/nrow(americans)*100, digits=0),
                   round(length(americans$other[americans$other == TRUE])/nrow(americans)*100, digits=0)
)
pie(founder_stats, labels = paste(c("Founder", "Inherited", "Other"), 
                                  sep =" ", founder_stats, "%"))




americans$other_fire = (americans$founder == "no") & (americans$major_wealth_inheritance == "no") & (
                      (americans$X2018_industry == "finance") | (americans$X2018_industry == "investment") | 
                      (americans$X2018_industry == "real estate")   
                      )
americans$other_nonfire = (americans$founder == "no") & (americans$major_wealth_inheritance == "no") & (
      (americans$X2018_industry != "finance") & (americans$X2018_industry != "investment") & 
      (americans$X2018_industry != "real estate")   
)

founder_stats = c( round(length(americans$founder_boolean[americans$founder_boolean == TRUE])/nrow(americans)*100, digits=0),
                   round(length(americans$inherited[americans$inherited == TRUE])/nrow(americans)*100, digits=0),
                   round(length(americans$other_fire[americans$other_fire == TRUE])/nrow(americans)*100, digits=0),
                   round(length(americans$other_nonfire[americans$other_nonfire == TRUE])/nrow(americans)*100, digits=0)                 
)

pie(founder_stats, labels = paste(c("Founder", "Inherited", "Finance/Investment/Real Estate", "Non-Fire Sector"), 
                                  sep =" ", founder_stats, "%"))


# TECH/MEDIA/ETC
founder_stats = c( round(length(americans$founder_tech[americans$founder_tech == TRUE])/nrow(americans)*100),
                   round(length(americans$founder_nontech[americans$founder_nontech == TRUE])/nrow(americans)*100),
                   round(length(americans$nonfounder[americans$nonfounder == TRUE])/nrow(americans)*100)
)
pie(founder_stats, labels = paste(c("Founder in Tech", "Founder Non-Tech", "Not a Founder"), 
                                  sep =" ", founder_stats, "%"))


# INHERITANCE
inheritance_stats = americans %>%
  count(major_wealth_inheritance) %>%
  # count creates a column called 'n'
  mutate(percent = n / sum(n) * 100)

americans$women_selfmade = (americans$gender == "female") & (americans$major_wealth_inheritance == "no")
americans$men_selfmade = (americans$gender == "male") & (americans$major_wealth_inheritance == "no")
americans$women_heirs = (americans$gender == "female") & (americans$major_wealth_inheritance == "yes")
americans$men_heirs = (americans$gender == "male") & (americans$major_wealth_inheritance == "yes")

inheritance_stats = c( round(length(americans$women_selfmade[americans$women_selfmade == TRUE])/nrow(americans)*100),
                       round(length(americans$men_selfmade[americans$men_selfmade == TRUE])/nrow(americans)*100),
                       round(length(americans$women_heirs[americans$women_heirs == TRUE])/nrow(americans)*100),
                       round(length(americans$men_heirs[americans$men_heirs == TRUE])/nrow(americans)*100)
)
pie(inheritance_stats, labels = paste(c("Women, Self-made", "Men, Self-made", "Women, Heirs", "Men, Heirs"), 
                                      sep =" ", inheritance_stats, "%"))


# ETHNICITY


americans$old_white_male = (americans$gender == "male") & (americans$X2020_age >= 60) & (americans$ethnicity == "white")
americans$old_white_female = (americans$gender == "female") & (americans$X2020_age >= 60) & (americans$ethnicity == "white")
americans$young_white_male = (americans$gender == "male") & (americans$X2020_age < 60) & (americans$ethnicity == "white")
americans$young_white_female = (americans$gender == "female") & (americans$X2020_age < 60) & (americans$ethnicity == "white")
americans$other = (americans$ethnicity != "white")


ethnicity_stats = c( round(length(americans$old_white_male[americans$old_white_male == TRUE])/nrow(americans)*100),
                     round(length(americans$old_white_female[americans$old_white_female == TRUE])/nrow(americans)*100),
                     round(length(americans$young_white_male[americans$young_white_male == TRUE])/nrow(americans)*100),
                     round(length(americans$young_white_female[americans$young_white_female == TRUE])/nrow(americans)*100),
                     round(length(americans$other[americans$other == TRUE])/nrow(americans)*100)
)
pie(ethnicity_stats, labels = paste(c("Old White Men", "Old White Women", "Young White Men", "Young White Women", "Other"), 
                                    sep =" ", ethnicity_stats, "%"))


ethnicity_stats = americans %>%
  count(ethnicity) %>%
  # count creates a column called 'n'
  mutate(percent = round(n / sum(n) * 100, digits=2))

pie(ethnicity_stats$percent, labels = paste(ethnicity_stats$ethnicity,
                                      sep =" ", ethnicity_stats$percent, "%"))
                                          
#now add jews (I am one)
americans$ethnicity[americans$misc == "jewish"] = "jewish"

ethnicity_stats = americans %>%
  count(ethnicity) %>%
  # count creates a column called 'n'
  mutate(percent = round(n / sum(n) * 100, digits=2))

pie(ethnicity_stats$percent, labels = paste(ethnicity_stats$ethnicity,
                                            sep =" ", ethnicity_stats$percent, "%"))                                      


# INDUSTRIES
# clean this up
industry_stats = americans %>%
  count(X2018_industry) %>%
  # count creates a column called 'n'
  mutate(percent = round(n / sum(n) * 100, digits=2))

pie(industry_stats$percent, labels = paste(industry_stats$X2018_industry,
                                            sep =" ", industry_stats$percent, "%"), cex=0.9)  

americans$tech_media = (americans$X2018_industry == "tech") | (americans$X2018_industry == "media")
americans$fire = (americans$X2018_industry == "finance") | (americans$X2018_industry == "investment") | 
                 (americans$X2018_industry == "real estate")   
americans$other = (americans$X2018_industry != "tech") & (americans$X2018_industry != "media") &
                  (americans$X2018_industry != "finance") & (americans$X2018_industry != "investment") &
                  (americans$X2018_industry != "real estate")


industry_stats = c( round(length(americans$tech_media[americans$tech_media == TRUE])/nrow(americans)*100),
                       round(length(americans$fire[americans$fire == TRUE])/nrow(americans)*100),
                       round(length(americans$other[americans$other == TRUE])/nrow(americans)*100))

pie(industry_stats, labels = paste(c("Tech or Media", "Finance, Investments, Real Estate", "Other"), 
                                      sep =" ", industry_stats, "%"))
                
# clean up with 2021 values
americans$X2020_net_worth = as.numeric(as.character(americans$X2020_net_worth))
americans$X2018_net_worth = as.numeric(as.character(americans$X2018_net_worth))
americans$gain = americans$X2020_net_worth - americans$X2018_net_worth




#######
## Americans AGE
######
data_name = "americans_age"
americans$X2020_age = strtoi(americans$X2020_age)
p = ggplot(data=americans, aes(X2020_age)) + geom_histogram(alpha = .7, fill="red", binwidth = 1)
p = p + xlab("Billionaire Age (years)") + ylab("Number of Billionaires") 
p = p + ggtitle("Age of American Billionaires")
ggsave(filename = sprintf("./%s_age_2020.png", data_name) , plot=p, width=9, height=5)

#######
## Chinese AGE
######
data_name = "chinese_age"
chinese = billionaires[billionaires$X2020_country == "china",]
chinese$X2020_age = strtoi(chinese$X2020_age)
p = ggplot(data=chinese, aes(X2020_age)) + geom_histogram(alpha = .7, fill="red", binwidth = 1)
p = p + xlab("Billionaire Age (years)") + ylab("Number of Billionaires") 
p = p + ggtitle("Age of Chinese Billionaires")
ggsave(filename = sprintf("./%s_age_2020.png", data_name) , plot=p, width=9, height=5)


################
## country waffle plots
##############
data_name = "countries"
billionaires = billionaires[!is.na(billionaires$X2020_country),]
countries = c()
total = nrow(billionaires)
for (country in levels(droplevels(factor(billionaires$X2020_country)))) {
  country_subset = billionaires[billionaires$X2020_country==country,]
  country_total = nrow(country_subset)
  country_total_value = sum(country_subset$net_worth)
  stats_row = c(country,country_total,total,country_total/total*100,country_total_value)
  countries = rbind(countries, stats_row)
}
countries = as.data.table(countries)
countries = as.data.frame(countries)
colnames(countries) = c("country","num_billionaires","total","percent_countries", "total_net_worth")
countries$percent_countries=as.numeric(as.character(countries$percent_countries))

vals = as.numeric(as.character(countries$num_billionaires))
names(vals)  =  countries$country

country_colors = sample(colors(), 69)
p = waffle::waffle(vals,
                   size = 1, 
                   colors = country_colors,
                   rows = 36,
                   title = "Billionaires by Country")
p = p + theme(plot.title = element_text(hjust = 0.5,size = 17, face = "bold", colour = "darkred"),
              legend.text = element_text(size = 6))
ggsave(filename = sprintf("./%s_countries_2020.png", data_name) , plot=p, width=10, height=6)


################
## plot by number of billionaires per country - compare to GDP
##############

countries$num_billionaires = as.numeric(as.character(countries$num_billionaires))
countries = countries[order(countries$num_billionaires, decreasing=FALSE),]
countries$country = factor(countries$country, levels = countries$country[order(countries$num_billionaires, decreasing=FALSE)])
top_countries = countries[69:52,]
p = ggplot(top_countries, aes(x=country, y=num_billionaires)) + geom_bar(stat="identity") 
p = p + xlab("Country") + ylab("Number of Billionaires") 
p = p + ggtitle("Number of Billionaires by Country")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=11), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=17,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s_top_country_bar_2020.png", data_name) , plot=p, width=8, height=6)

data_name = "gdp"
gdp = read.csv("gdp_2017.csv")
gdp$GDP = as.numeric(as.character(gdp$GDP))
gdp$Country = factor(gdp$Country, levels = gdp$Country[order(gdp$GDP, decreasing=FALSE)])
p = ggplot(gdp, aes(x=Country, y=GDP)) + geom_bar(stat="identity") 
p = p + xlab("Country") + ylab("2017 GDP") 
p = p + ggtitle("2017 GDP by Country")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=11), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=17,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s_top_country_bar_2020.png", data_name) , plot=p, width=8, height=6)


################
## plot delta of billionaires in 2021 vs 2020
##############
data_name = "diff"
americans$diff = americans$X2021_net.worth - americans$X2020_net_worth
americans$diff = as.numeric(as.character(americans$diff))
americans = americans[order(americans$diff, decreasing=TRUE),]
americans$name = factor(americans$name)
americans$name = factor(americans$name, levels = americans$name[order(americans$diff, decreasing=TRUE)])
top_names = americans[1:20,]
p = ggplot(top_names, aes(x=name, y=diff)) + geom_bar(stat="identity") 
p = p + xlab("Billionaire") + ylab("Billions of USD Gained in 2020") 
p = p + ggtitle("Top Gaining Billionaires")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=11), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=17,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s_top_bar_2020.png", data_name) , plot=p, width=8, height=6)


# which industries gained the most money
industry_gains = americans %>%
  group_by(X2018_industry) %>%
  summarise(sum = sum(diff), n = n())

data_name = "industry_diff"
industry_gains$X2018_industry = factor(industry_gains$X2018_industry, levels = industry_gains$X2018_industry[order(industry_gains$sum, decreasing=TRUE)])
p = ggplot(industry_gains, aes(x=X2018_industry, y=sum)) + geom_bar(stat="identity") 
p = p + xlab("Billionaire Industry") + ylab("Billions of USD Gained in 2020") 
p = p + ggtitle("Industries in which Billionaires Gained the Most $ in 2020")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=11), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=17,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s_top_bar_2020.png", data_name) , plot=p, width=8, height=6)
