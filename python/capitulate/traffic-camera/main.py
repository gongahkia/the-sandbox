# data retrieved every 20 seconds from cameras API, creates local log file w sifted data

import requests
import json

fhand = open("log.txt","w")
traffic_camera = requests.get("https://api.data.gov.sg/v1/transport/traffic-images")
contents = json.loads(traffic_camera.text)["items"]
count:int = 0
final:dict = {}
for camera in contents[0]["cameras"]:
    count += 1
    temp:dict = {
            "camera_id": 0,
            "image_url": ""
            }
    temp["camera_id"] = camera["camera_id"]
    temp["image_url"] = camera["image"]
    final[count] = temp
fhand.write(str(final))
fhand.close()
