#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 19 14:50:41 2019

@author: antongaev
"""
from selenium import webdriver
import re
import json
import csv
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait # available since 2.4.0
from selenium.webdriver.support import expected_conditions as EC # available since 2.26.0

#add headless parameter
options = webdriver.firefox.options.Options()    
options.add_argument("--headless")
driverFF = webdriver.Firefox(firefox_options=options, executable_path='/Users/antongaev/Downloads/geckodriver')

route = []
latitudeX = []
longitudeX = []
latitudeY = []
longitudeY = []

#create list for jsons
data = []

reader = csv.reader(open('distance_search.csv', 'r', encoding="utf-8", newline =''))
#convert reader into json
for row in reader:
            h, k, v, a, b, c, e = row
            route.append(v)
            latitudeX.append(a)
            longitudeX.append(b)
            latitudeY.append(c)
            longitudeY.append(e)

u = 'https://www.openstreetmap.org/directions?engine=fossgis_osrm_car&route='
r = '%2C'
l = '%3B'
y = '%2C'

for i in range(1,len(route)):  
    url = u+latitudeX[i]+r+longitudeX[i]+l+latitudeY[i]+y+longitudeY[i]
    try:
        driverFF.get(url)
        element = WebDriverWait(driverFF, timeout=8).until(
                                        EC.visibility_of_element_located((By.ID, 'routing_summary')))
        elem = driverFF.find_element_by_id('routing_summary').text.strip().split()[1]
        elem = re.sub("\D", "", elem)
        #form json
        datasuka = {"distance": elem, "route": route[i]}
        data.append(datasuka)
    except:
        #form json
        datasuka = {"distance": "NA", "route": route[i]}
        data.append(datasuka)

#finally json saver should be added
with open('parsed_distances.json', 'w') as fp:
            json.dump(data, fp)
