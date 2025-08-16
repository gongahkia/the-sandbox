# For interaction with different Web APIs
# source = https://medium.com/quick-code/absolute-beginners-guide-to-slaying-apis-using-python-7b380dc82236

import requests
from requests.api import request
import json

# --------------------

### NO PROPER END POINT 

testrequest = requests.get("http://api.open-notify.org")
print(testrequest.text)
# prints out the pure html code if a specified target endpoint (indicated in the URL) is not hit

# --------------------

### EXIT CODES

print(f"successful request: {testrequest.status_code}")
# this will print out an exit code of 200 since the endpoint is hit succesfully

eg_failedrequest = requests.get("http://api.open-notify.org/fake-endpoint")
print(f"failed request: {eg_failedrequest.status_code}")
# this will return an exit code of 404 due to us hitting a failed endpoint
# exit codes of the 500~ level indicate that the server has failed, and an endpoint was not hit at all

# --------------------

### A PROPER END POINT
# open notify API makes NASA's data available to the public in the form of APIs

people_in_space = requests.get("http://api.open-notify.org/astros.json")
# here is an example of hitting a succesful end point (URL), which returns specific information in our desired format

print(json.loads(people_in_space.text))
# we can use the json in built python library and the json.loads() method to convert json string to a json file format

print(json.loads(people_in_space.text)["number"])
# with this, we are able to access the key-value pair's value for the key "number" in the converted .json information

for person in json.loads(people_in_space.text)["people"]:
    print(person["name"])
# we are also able to access each indivual element in the json using dictionary methods

# --------------------

### DATA MUSE
# we pass query parameters (search parameters) to the URL string in the specified format below

datamuse_url = "https://api.datamuse.com/words?rel_rhy=jingle"
# copying and pasting the below string url to a web browser will show a page of words that rhyme with "jingle"
# here, "words" under the datamuse website is the endpoint we are targetting, and query parameters are prefixed with a "?", "rel_rhy" (keyword indicates to datamuse to find perfect rhymes) and "jingle" (query parameter)

query_param = {"rel_rhy": "jingle"}
search_request = requests.get("https://api.datamuse.com/words", query_param)
# we can implement this query paramters with data muse's words as our endpoint by using requests.get() method as seen above

for word in json.loads(search_request.text):
    print(word["word"])
# as covered above, we can then use json conversion to parse and extract the desired rhyming words as our target data
# BEAR IN MIND that we need to inspect the "shape" (format) of our returned json string before we begin to parse it accordingly, and should not assume that every website's API returns json files in a similar format

# --------------------

### REST APIS

# most APIs are restful, and we can access the organised information (in the form of string JSONs and other formats) by targetting the specific endpoint (specified in the URL which differentiate different kinds of data resources on the server), a prime example being the NASA open-notify rest API we referenced above
# the main thing to note about Rest APIs are that they are stateless, so neither the requesting client nor the server has to hold any information about the other party, and every http get request returns the requested output

# ----------

### GRAPHQL

# GraphQL (graphical query language) is opposed to Rest APIs in that they allow for targetted fetching of specific elements within a string JSON that would normally be returned as a whole dictionary with a Rest API's http get request
# the client sends in their get request in the format of a nested JSON, and it is up to the server to implement a solution to resolve said targetted get request

# --------------------

### IN CONCLUSION

# working with APIs is actually damn simple:

    # (1) Choose the API to work with
    # (2) Read the API documentation
    # (3) Start with small code, and complement it with more features

# Here are some good websites to reference to get started:

    # https://github.com/public-apis/public-apis ⭐
    # https://github.com/public-api-lists/public-api-lists
    # https://github.com/n0shake/Public-APIs
    # https://github.com/VNAPNIC/public-apis
    # https://www.pythonforbeginners.com/api/list-of-python-apis
    # https://rapidapi.com/blog/most-popular-api/
    # https://apilist.fun/
    # https://mixedanalytics.com/blog/list-actually-free-open-no-auth-needed-apis/
    # https://blog.hubspot.com/website/free-open-apis
    # https://github.com/realpython/list-of-python-api-wrappers

# --------------------
