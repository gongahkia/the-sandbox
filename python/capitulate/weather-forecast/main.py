# implement this in scrapy instead  to prepare for ian chai's internship and toolchains
    # https://scrapy.org/

import requests
import json

fhand = open("log.txt","w")
weather_forecast = requests.get("https://api.data.gov.sg/v1/environment/24-hour-weather-forecast")
contents = json.loads(weather_forecast.text)["items"][0]
general_forecast:str = contents["general"]["forecast"]
general_temp_range:(int) = (contents["general"]["relative_humidity"]["low"], contents["general"]["relative_humidity"]["high"])
latest_weather_forecast:{str:str} = contents["periods"][-1]["regions"]
fhand.write(f"general forecast: {general_forecast}\ngeneral temp range: {general_temp_range[0]} - {general_temp_range[1]}\nlatest weather forecast: {latest_weather_forecast}")
fhand.close()
