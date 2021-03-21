# billionaires
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
csvName = "billionaires_2018.csv"
data_name = "billionaires"
setwd(workingDir)

billionaires = read.csv(csvName)
billionaires$net_worth = as.numeric(as.character(billionaires$Net.Worth..billions.of.dollars.))
  
################
## country waffle plots
##############
countries = c()
total = nrow(billionaires)
for (country in levels(droplevels(billionaires$Country))) {
  country_subset = billionaires[billionaires$Country==country,]
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

country_colors = sample(colors(), 72)
p = waffle::waffle(vals,
                   size = 1, 
                   colors = country_colors,
                   rows = 36,
                   title = "Billionaires by Country")
#p = p + ggthemes::scale_fill_tableau(name=NULL)
p = p + theme(plot.title = element_text(hjust = 0.5,size = 17, face = "bold", colour = "darkred"),
              legend.text = element_text(size = 6))
ggsave(filename = sprintf("./%s_countries.png", data_name) , plot=p, width=10, height=6)


################
## plot by number of companies per country - compare to GDP
##############
top_countries = countries[72:52,]
top_countries$num_billionaires = as.numeric(as.character(top_countries$num_billionaires))
top_countries = top_countries[order(top_countries$num_billionaires, decreasing=FALSE),]
top_countries$country = factor(top_countries$country, levels = top_countries$country[order(top_countries$num_billionaires, decreasing=FALSE)])
p = ggplot(top_countries, aes(x=country, y=num_billionaires)) + geom_bar(stat="identity") 
p = p + xlab("Country") + ylab("Number of Unicorns") 
p = p + ggtitle("Number of Billionaires by Country")
p = p + theme(plot.title = element_text(hjust = 0.5))
p = p + theme(axis.text=element_text(size=10,face="bold"), axis.title=element_text(size=11), axis.title.y=element_blank())
p = p + theme(plot.title = element_text(size=17,face="bold"))
p = p + guides(fill=FALSE)
p = p + coord_flip() 
p = p + scale_fill_manual(values = c("#003b9b"))
ggsave(filename = sprintf("./%s_top_country_bar.png", data_name) , plot=p, width=8, height=6)


#######
## AGE
######
billionaires$Age = strtoi(billionaires$Age)
p = ggplot(data=billionaires, aes(Age)) + geom_histogram(alpha = .7, fill="red", binwidth = 1)
p = p + xlab("Billionaire Age (years)") + ylab("Number of Billionaires") 
p = p + ggtitle("Age of Billionaires")
ggsave(filename = sprintf("./%s_age.png", data_name) , plot=p, width=9, height=5)

