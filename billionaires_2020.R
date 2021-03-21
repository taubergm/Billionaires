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
library(wordcloud)
if (!require(tm)) {
  install.packages("tm", repos="http://cran.us.r-project.org")
}
library(SnowballC)
if (!require(SnowballC)) {
  install.packages("SnowballC", repos="http://cran.us.r-project.org")
}
library(SnowballC)
if (!require(plyr)) {
  install.packages("plyr", repos="http://cran.us.r-project.org")
}
library(plyr)

workingDir = '/Users/michaeltauberg/projects/billionaires/'
csvName = "billionaires_2020b.csv"
data_name = "billionaires"
setwd(workingDir)

billionaires = read.csv(csvName)
billionaires$net_worth = as.numeric(as.character(billionaires$X2020_net_worth))
  
################
## country waffle plots
##############
countries = c()
total = nrow(billionaires)
for (country in levels(droplevels(billionaires$X2020_country))) {
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

country_colors = sample(colors(), 73)
p = waffle::waffle(vals,
                   size = 1, 
                   colors = country_colors,
                   rows = 36,
                   title = "Billionaires by Country")
p = p + theme(plot.title = element_text(hjust = 0.5,size = 17, face = "bold", colour = "darkred"),
              legend.text = element_text(size = 6))
ggsave(filename = sprintf("./%s_countries_2020.png", data_name) , plot=p, width=10, height=6)


################
## plot by number of companies per country - compare to GDP
##############

countries$num_billionaires = as.numeric(as.character(countries$num_billionaires))
countries = countries[order(countries$num_billionaires, decreasing=FALSE),]
countries$country = factor(countries$country, levels = countries$country[order(countries$num_billionaires, decreasing=FALSE)])
top_countries = countries[72:52,]
p = ggplot(top_countries, aes(x=country, y=num_billionaires)) + geom_bar(stat="identity") 
p = p + xlab("Country") + ylab("Number of Unicorns") 
p = p + ggtitle("Number of Billionaires by Country")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=11), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=17,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s_top_country_bar_2020.png", data_name) , plot=p, width=8, height=6)


################
## plot delta 2018 billionaires and 2020
##############

#######
## AGE
######
billionaires$X2020_age = strtoi(billionaires$X2020_age)
p = ggplot(data=billionaires, aes(X2020_age)) + geom_histogram(alpha = .7, fill="red", binwidth = 1)
p = p + xlab("Billionaire Age (years)") + ylab("Number of Billionaires") 
p = p + ggtitle("Age of Billionaires")
ggsave(filename = sprintf("./%s_age_2020.png", data_name) , plot=p, width=9, height=5)



# AMERICANs

csvName = "american_billionaires_b.csv"
data_name = "american_billionaires"
setwd(workingDir)

americans = read.csv(csvName)
americans$net_worth = as.numeric(as.character(americans$X2020_net_worth))




# pie chart of industry
industries = c()
total = nrow(americans)
for (industry in levels(droplevels(americans$X2018_industry))) {
  industries_subset = americans[americans$X2018_industry==industry,]
  industries_total = nrow(industries_subset)
  stats_row = c(industry,industries_total,total,industries_total/total*100)
  industries = rbind(industries, stats_row)
}
industries = as.data.table(industries)
industries = as.data.frame(industries)
colnames(industries) = c("industry","num_industries","total","percent_industry")
industries$percent_industry=as.numeric(as.character(industries$percent_industry))

slices = as.numeric(as.character(industries$percent_industry))
labels =  levels(americans$X2018_industry)
# Pie Chart with Percentages
pct <- round(slices/sum(slices)*100)
labels <- paste(labels, pct) # add percents to labels
labels <- paste(labels,"%",sep="") # ad % to labels
pie(slices,labels = labels, col=rainbow(length(lbls)),
    main="Industries where Billionaires Made their Fortunes")




# pie chart of race
ethnicities = c()
total = nrow(americans)
for (ethnicity in levels(droplevels(americans$ethnicity))) {
  ethnicities_subset = americans[americans$ethnicity==ethnicity,]
  ethnicities_total = nrow(ethnicities_subset)
  stats_row = c(ethnicity,ethnicities_total,total,ethnicities_total/total*100)
  ethnicities = rbind(ethnicities, stats_row)
}
ethnicities = as.data.table(ethnicities)
ethnicities = as.data.frame(ethnicities)
colnames(ethnicities) = c("ethnicity","num_ethnicities","total","percent_ethnicity")
ethnicities$percent_ethnicity=as.numeric(as.character(ethnicities$percent_ethnicity))


slices = as.numeric(as.character(ethnicities$percent_ethnicity))
labels =  levels(americans$ethnicity)
# Pie Chart with Percentages
pct <- round(slices/sum(slices)*100)
labels <- paste(labels, pct) # add percents to labels
labels <- paste(labels,"%",sep="") # ad % to labels
pie(slices,labels = labels, col=rainbow(length(lbls)),
    main="Ethnicities of American Billionaires")

# pie chart of inherited
americans$major_wealth_inheritance


# pie chart of founder
americans$founder