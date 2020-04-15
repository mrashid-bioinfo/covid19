require(tidyverse)
require(dplyr)



# https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide

# http://www.exceluser.com/formulas/how-to-calculate-both-types-of-compound-growth-rates.html

## Temparature Data ##
# https://towardsdatascience.com/obtain-historical-weather-forecast-data-in-csv-format-using-python-5a6c090fc828


setwd("/home/mamun/Personal/Pet_Projects")
#covid19_spread_df = read.delim("COVID-19-geographic-disbtribution-worldwide-2020-03-22.csv", sep = ",", stringsAsFactors = F )

source("utlis.R")
covid19_spread_df = download_data()
write.table( covid19_spread_df, "COVID-19-geographic-disbtribution-worldwide-2020-03-29.csv", sep = ",", quote = F, row.names = F )
head(covid19_spread_df)

covid19_spread_nousa = covid19_spread_df[ covid19_spread_df$countriesAndTerritories != "United_States_of_America",  ]

## Load US curated data 
us_curated_data = read.delim("us-curated_data.csv", sep = ",", stringsAsFactors  = F )

## Merge USA and All Data

covid19_spread_all = rbind( covid19_spread_nousa, us_curated_data )

area_col = "countriesAndTerritories"
new_case_col = "cases"
date_col = "dateRep"
covid19_spread_all[,area_col] = as.character( covid19_spread_all[,area_col] )
uniq_countries = unique( covid19_spread_all[,area_col] )
country_wise_df = c()
country_wise_nonzero_df_list = list()
for( i in 1:length(  uniq_countries ) )
{
	temp_df = covid19_spread_all[ covid19_spread_all[,area_col] ==  uniq_countries[i], ]
	require(dplyr)
	require(tidyverse)
	temp_df_rev = as.data.frame(temp_df %>% map_df(rev))
	temp_df_rev[,date_col] = as.character( temp_df_rev[,date_col] )
	
	if( dim(temp_df_rev)[1] <= 10)
	{
		next;
	}
	
	temp_rev_interval_summary = summarise_over_interval(df=temp_df_rev, interval = 3, method = "average", columns = c(new_case_col,new_case_col), columns_to_add = date_col )

	## Remove all the dates before 1st infection ##
	cumul_sum = cumsum(as.numeric(as.character(temp_rev_interval_summary[,new_case_col])))
	
	temp_rev_interval_summary_nonzero = temp_rev_interval_summary[ cumul_sum > 0, ]
	colnames(temp_rev_interval_summary_nonzero)[1] = date_col
	country_wise_df = rbind( country_wise_df, c( Country = uniq_countries[i], compute_spread_rate( temp_rev_interval_summary_nonzero, new_case_col, date_col), Total_cases = cumul_sum[ length(cumul_sum) ] ) )

	country_wise_nonzero_df_list[[i]] = temp_df_rev
}

names(country_wise_nonzero_df_list) = uniq_countries
country_wise_df = data.frame(country_wise_df, stringsAsFactors = F )
country_wise_df$mean_delta = as.numeric( as.character(country_wise_df$mean_delta) )
country_wise_df_o = dplyr::arrange( country_wise_df, desc(mean_delta) )

country_wise_mean_delta_top = country_wise_df_o[ as.numeric(country_wise_df_o$Total_cases) >= 500 & country_wise_df_o$data_size >= 10, ]

write.table( country_wise_mean_delta_top, "Top_Regions.tsv", sep = "\t", quote = F, row.names = F )
max_delta_country_data = country_wise_mean_delta_top[1,]
min_delta_country_data = country_wise_mean_delta_top[dim(country_wise_mean_delta_top)[1],]

wd = "/home/mamun/Personal/Pet_Projects/Plots/"
for( i in 1:dim( country_wise_mean_delta_top )[1] )
{
	temp_country = country_wise_mean_delta_top[i, "Country"]
	temp_df = country_wise_nonzero_df_list[[temp_country]]
	temp_df$rank = temp_df$countriesAndTerritories
	temp_df$days_since_first = 1:dim(temp_df)[1]
	temp_df$Total_cases = cumsum( temp_df$cases )

	max_df = country_wise_nonzero_df_list[[max_delta_country_data$Country]]
	max_df$rank = paste0("Region with highest rate : ", max_delta_country_data$Country )
	max_df$days_since_first = 1:dim(max_df)[1]
	max_df$Total_cases = cumsum( max_df$cases )
	min_df = country_wise_nonzero_df_list[[min_delta_country_data$Country]]	
	min_df$rank = paste0("Region with lowest rate : ", min_delta_country_data$Country )
	min_df$days_since_first = 1:dim(min_df)[1]
	min_df$Total_cases = cumsum( min_df$cases )

	merged_df = rbind( min_df, temp_df, max_df )	

	require(ggplot2)
	color_code = c("indianred1", "darkorange2", "deepskyblue3")
	names( color_code ) = c(paste0("Region with highest rate : ", max_delta_country_data$Country ), temp_country,  paste0("Region with lowest rate : ", min_delta_country_data$Country ) )

	plot_list = list()
	plot_list[[1]] = ggplot( merged_df, aes( x = days_since_first, y = Total_cases, color = rank ) ) + geom_point(size = 3 ) + geom_line( size = 2 , alpha = 0.5 ) + scale_y_log10() + xlab("Days since the first inspection") + ylab("Total Cases") + theme_bw() + theme(axis.text = element_text(size = 12 )) + scale_color_manual(values=color_code)

	plot_list[[2]] = ggplot( merged_df, aes( x = days_since_first, y = cases, color = rank ) ) + geom_point(size = 3 ) + geom_line( size = 2, alpha = 0.5 ) + scale_y_log10() + xlab("Days since the first inspection") + ylab("New Daily Cases") + theme_bw() + theme(axis.text = element_text(size = 12 )) + scale_color_manual(values=color_code)

	pdf( file = paste0( wd, "/", temp_country, ".pdf"), width = 16, height = 6 )
	multiplot( plotlist=plot_list, cols = 2 )
	dev.off()
}


## Bin temparature change ##

	temp_file_lists = dir( path = "Temp_data", include.dirs=F, full.names = TRUE)
	library(Hmisc)
	Country_wise_temp_delta = c( )
	for( i in 1:length( temp_file_lists ) )
	{
		temparature_df = read.delim(temp_file_lists[i], sep = ",", header = T, stringsAsFactors = F )
		temparature_df$min_max_midpoint = (temparature_df$maxtempC + temparature_df$mintempC ) / 2 
		temparature_interval_summary = summarise_over_interval(df=temparature_df, interval = 24, method = "average", columns = c("min_max_midpoint", "humidity"), columns_to_add = "date_time")
		colnames(temparature_interval_summary)[1] = "Date";
		temparature_change_rate = compute_spread_rate( temparature_interval_summary, "min_max_midpoint", "Date" )
		names(temparature_change_rate) = paste0( "temparature_", names(temparature_change_rate) )

		humidity_change_rate = compute_spread_rate( temparature_interval_summary, "humidity", "Date" )
		names(humidity_change_rate) = paste0( "humidity_", names(humidity_change_rate) )
		country = gsub( "Temp_data/|.csv", "", temp_file_lists[i] )
		Country_wise_temp_delta = rbind( Country_wise_temp_delta, c( Country = capitalize(country), temparature_change_rate, humidity_change_rate ) )
	}

	Country_wise_temp_delta = as.data.frame( Country_wise_temp_delta )


## Merge Spread data and Temparature Data ##
setdiff( Country_wise_temp_delta$Country, country_wise_df_o$Country )

## Manual fiddling ##
 setdiff( Country_wise_temp_delta$Country, country_wise_df_o$Country )
# [1] "Cairo"  "Durban" "Haiti"  "La"     "Moscow" "Ny"     "Prague" "Riyadh"
# [9] "Seoul"  "Uk"     "Wuhan" 



## manual correction of temp data regions names ##
Country_wise_temp_delta$Country = as.character(Country_wise_temp_delta$Country)
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Cairo", "Country" ] = "Egypt"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Durban", "Country" ] = "South_Africa"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Haiti", "Country" ] = "Dominican_Republic"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "La", "Country" ] = "Los Angeles"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Moscow", "Country" ] = "Russia"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Ny", "Country" ] = "New York"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Prague", "Country" ] = "Czech_Republic"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Riyadh", "Country" ] = "Saudi_Arabia"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Seoul", "Country" ] = "South_Korea"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Uk", "Country" ] = "United_Kingdom"
Country_wise_temp_delta[ Country_wise_temp_delta$Country == "Wuhan", "Country" ] = "China"


country_wise_df_o_merged = dplyr::inner_join( country_wise_df_o, Country_wise_temp_delta, by = "Country" )
country_wise_df_o_merged$temparature_mean_delta = as.numeric( as.character(country_wise_df_o_merged$temparature_mean_delta) )
country_wise_df_o_merged$humidity_mean_delta = as.numeric( as.character(country_wise_df_o_merged$humidity_mean_delta) )


write.table( country_wise_df_o_merged, "Infection_rate_and_temp_change_mapped.tsv", sep = "\t", quote = F, row.names = F )

# Correlation panel
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07") 
panel.cor <- function(x, y)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round(cor(x, y), digits=2)
    txt <- paste0("R = ", r)
    cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols)
}
# Create the plots
pdf( file = "Correlation_between_infection_delta_and_temp_and_humidity_delta.pdf" )
pairs(country_wise_df_o_merged[,c("mean_delta", "temparature_mean_delta", "humidity_mean_delta")], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)
dev.off()

