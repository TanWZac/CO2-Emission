rm(list = ls())
setwd("C:/Users/twz18/Downloads")
data = read.csv("co2_emissions_tonnes_per_person.csv", header=TRUE)
data
library(tidyverse)
library(ggplot2)
library(reshape2)
co2emi = select(data, c(country, 207:220))
co2emi <- na.omit(co2emi)
colnames(co2emi) = gsub("X", "", colnames(co2emi))
melted = melt(co2emi)
melted
co2emi = melted
colnames(co2emi)= c('Country', 'Year', 'Co2_emission')
co2emi

totalEmi = ggplot(co2emi, aes(Co2_emission)) + geom_histogram(bins=30) # histogram on total emission 
totalEmi

emiBoxplt = ggplot(co2emi, aes(y=log(Co2_emission), x=Year)) + geom_boxplot(notch = TRUE, outlier.alpha = 0.1)
emiBoxplt

emiScatter = ggplot(co2emi, aes(x=Year, y=log(Co2_emission))) + geom_point()
emiScatter


TotalEmiYear = aggregate(Co2_emission ~ Year, co2emi, sum)
sdEmiYear = aggregate(Co2_emission ~ Year, co2emi, sd)
AvgEmiYear = aggregate(Co2_emission ~ Year, co2emi, mean)

TotalEmiYearplt = ggplot(TotalEmiYear, aes(x=Year, y=Co2_emission, group=1)) +
  geom_line() + geom_point() + theme_minimal() + 
  labs(title = "Total CO2 emissions each year (tonnes per person)",subtitle = "2007 - 2018"
      ,x = "Year", y = "CO2 tonnes per person")
TotalEmiYearplt

sdEmiYearplt = ggplot(sdEmiYear, aes(x=Year, y=Co2_emission, group=1)) + geom_line() + geom_point() + theme_minimal()
sdEmiYearplt
colnames(sdEmiYear) = c("Year", "SDCO2")

AvgEmiplt = ggplot(AvgEmiYear, aes(x=Year, y=Co2_emission, group=1)) + geom_line() + geom_point() + theme_minimal()
AvgEmiplt
colnames(AvgEmiYear) = c("Year","AvgCO2")

p = ggplot() +
    geom_smooth(data=sdEmiYear, aes(x=Year, y=SDCO2, group=1, color = "Standard Deviation"), fill='red') + 
    geom_point(data=sdEmiYear, aes(x=Year, y=SDCO2, group=1)) +
    geom_smooth(data=AvgEmiYear, aes(x=Year, y=AvgCO2, group=1, color = "Mean"), fill='blue') +
    geom_point(data=AvgEmiYear,  aes(x=Year, y=AvgCO2, group=1)) +
    labs(title = "CO2 emission each year (tonnes per person)", x = "Year", y = "CO2 tonnes per person") +
    scale_color_manual(name="Line", values=c("blue", "red")) + 
    theme(legend.text=element_text(size=12),legend.justification=c(1,1),legend.position=c(1,1), 
          panel.background = element_rect(fill = "white", color="white", linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "lightgrey"))   # show standard deviation and mean of CO2
p

topEmi = aggregate(Co2_emission~Country, co2emi, sum)     # Each country Total Emission 
AvgConEmi = aggregate(Co2_emission~Country, co2emi, mean) # Each country Average Emission
AvgConEmi$Co2_emission = round(AvgConEmi$Co2_emission,2)

topEmiCon = head(topEmi[order(topEmi$Co2_emission, decreasing= T),], n = 10)
topEmiCon %>% mutate(Country=fct_reorder(Country, desc(Co2_emission))) %>%
  ggplot(aes(x=Country, y=Co2_emission, fill=Country)) + geom_bar(stat='identity') + 
  theme_minimal() +scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  geom_text(aes(label=Co2_emission), position=position_dodge(width=0.9), vjust=-0.25) + 
  labs(title="Top 10 CO2 emission Countries ")   # top 10 countries with highest CO2 emission

avgEmiCon = head(AvgConEmi[order(AvgConEmi$Co2_emission, decreasing= T),], n = 10)
avgEmiCon %>% mutate(Country=fct_reorder(Country, desc(Co2_emission))) %>%
  ggplot(aes(x=Country, y=Co2_emission, fill=Country)) + geom_bar(stat='identity') + 
  geom_text(aes(label=Co2_emission), position=position_dodge(width=0.9), vjust=-0.25) + 
  theme_minimal() + scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  labs(title = "Top 10 Average CO2 emission")

qatarCO2 = as.data.frame(co2emi[co2emi$Country=="Qatar",])
qatarCO2
qatar = ggplot(qatarCO2, aes(x=Year, y=Co2_emission, group=1)) + geom_line() + geom_point()+theme_minimal()
qatar
# create function to merge 5 country graph together
lst = c("Trindad and Tobago", "Kuwait", "Bahrain", " United Arab Emirates", "Luxembourg", 
       "Brunei", "Saudi Arabia", "United States", "Australia")
get_data = function(a){
  pass = as.data.frame(co2emi[co2emi$Country==a,])
  return(pass)
}

Merge = as.data.frame(co2emi[co2emi$Country=="Qatar",])
for (i in lst){
  Merge = rbind(Merge, get_data(i))
}
Merge %>% ggplot(aes(x=Year, y= Co2_emission, fill=Country, group=Country, color = Country)) + 
  geom_line(size=0.8) + labs(title="Top 10 Countries with highest CO2 Emissions",subtitle = "2005 - 2018") +
  theme(legend.text=element_text(size=12),legend.justification=c(1,1),legend.position=c(1,1), 
        panel.background = element_rect(fill = "white", color="white", linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid', colour = "lightgrey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "lightgrey"))

#find top 10 countries with most co2 emission
