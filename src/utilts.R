#these libraries need to be loaded

download_data = function()
{
	library(utils)
	library(httr)

	#download the dataset from the ECDC website to a local temporary file
	GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".csv")))

	#read the Dataset sheet into “R”. The dataset will be called "data".
	data <- read.csv(tf)
	return( data[,1:8] )
}

summarise_over_interval = function(df=NULL, interval = 5, method = "average", columns = NULL, columns_to_add = "date_time")
{
	summarised_over_interval_df = c()
	loop_end = ceiling( dim(df)[1]/ interval)

	for( i in 1:loop_end)
	{
		start = ((i-1)*interval ) + 1;
		end = ((i-1)*interval ) + interval
		end = ifelse( dim(df)[1]  <= end, dim(df)[1], end )
		#print( paste0( "Start : ", start , "End : ", end ) )
		
		temp_df = df[ start:end, ]

		if( method == "average" )
		{	
			#print(temp_df)
			temp_val = c( as.character(temp_df[1,columns_to_add]) , colMeans( temp_df[ , columns ] ) )
			summarised_over_interval_df = rbind(summarised_over_interval_df, temp_val  )	
		}else
		{
			print("Currently only supports average")
			next;
		} 
	}

	return( summarised_over_interval_df )
}

compute_spread_rate = function(df=NULL, column_name = NULL, date_col = NULL)
{
	data_size = dim( df )[1];
	deltas = c() 
	#print(data_size)
	for( i in 2:data_size )
	{
		#print(i)
		deltas = append( deltas, ( as.numeric(df[i,column_name]) - as.numeric(df[i-1,column_name]) ) )
		#print(deltas)
	}

	start_val = as.numeric(df[1,column_name])
	end_val = as.numeric(df[ data_size ,column_name])
	start_date = df[1,date_col]
	end_date = df[data_size,date_col]

	mean_delta = mean(deltas)
	#print(deltas)

	return( c( mean_delta = mean_delta, start_val = start_val, end_val = end_val, start_date = start_date, end_date = end_date, data_size = data_size ) )
}
