pip install wwo-hist
from wwo_hist import retrieve_hist_data
import os
os.chdir("/home/mamun/Personal/Pet_Projects")


## Examples Runs 
	frequency = 120
	start_date = '31-DEC-2019'
	end_date = '29-MAR-2020'
	api_key = '8e5c03175bce4474abe03358202803'
	location_list = ['india','california']
	hist_weather_data = retrieve_hist_data(api_key, location_list, start_date, end_date, frequency, location_label = False, export_csv = True, store_df = True)

	location_list = ['india']
	hist_weather_data = retrieve_hist_data(api_key, location_list, start_date, end_date, frequency, location_label = False, export_csv = True, store_df = True)



## Top Corona Affected Country Temp data 
os.chdir("/home/mamun/Personal/Pet_Projects")
frequency = 120  ## [5 days frequecny ]
start_date = '31-DEC-2019'
end_date = '29-MAR-2020'
api_key = '8e5c03175bce4474abe03358202803'

location_list = ['spain','turkey','italy','iran','france','germany','michigan','uk','netherlands','portugal','belgium','switzerland','florida','austria','israel','brazil','prague','chile','ireland','canada','poland','luxembourg','serbia','maryland','texas','ecuador','norway','pakistan', 'moscow','riyadh','colombia','finland','greece','japan','estonia','slovenia','iceland','malaysia','argentina','croatia','india','peru','thailand','iraq','wuhan','seoul','china','qatar','singapore','cairo','durban', 'la', 'ny' ]

hist_weather_data = retrieve_hist_data(api_key, location_list, start_date, end_date, frequency, location_label = False, export_csv = True, store_df = True)

