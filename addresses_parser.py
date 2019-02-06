#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 25 09:58:25 2019

@author: antongaev
"""
import selenium
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
driverFF = webdriver.Firefox(firefox_options=options, executable_path='/Users/antongaev/Documents/geckodriver')

d = {}
driverFF.get('https://yandex.ru/maps/2/saint-petersburg/?ll=30.315635%2C59.938951&z=11')
reader = csv.reader(open('to_maps.csv', 'r', encoding="utf-8", newline =''))

#convert reader into json
for row in reader:
            k, v = row
            d[k] = v

#set constants
elem = driverFF.find_element_by_xpath('//input[@class="input_air-search-large__control"]')
button = driverFF.find_element_by_xpath('//button[@class="button_air-search-large _pin-left _pin-right"]')

#create list for jsons
data = []

#send every row into the text input
for m in range(1,(len(d))):
    #json key recognises str, not digits
    elem.send_keys(d[str(m)])
    button.click()
    try:
        element = WebDriverWait(driverFF, timeout=2).until(
                                    EC.visibility_of_element_located((By.CLASS_NAME, 'toponym-card-title-view__coords')))
        whole_div = driverFF.find_element_by_xpath('/html/body/div[1]/div[4]/div/div').text.strip().split()
        for i in range(0,15):
            #need to add "stop when found" expression later
            #search for coordinates in a large div
            if re.search('\.[0-9]{6}\,', whole_div[i]) == None:
                #if no coords then pass
                pass
            else:
                latitude = whole_div[i]
                longitude = whole_div[i+1]
                
                #creaate empty list for new data
                capitals = []
                
                #add region/city/street recognizer                    
                #check whether n start with capital letter
                for n in range(0,i):
                    if whole_div[n][0].isupper() == True:
                        #delete Panorama and Russia, Russia
                        
                        #i guess i should delete the word "Republic"
                        #Novgorods should be checked somehow later
                        
                        if whole_div[n] == "Республика,":
                            pass
                        else:
                            if whole_div[n] == "Россия,":
                                pass
                            else:
                                if whole_div[n] == "Россия":
                                    pass
                                else:
                                    if whole_div[n] == "Панорама":
                                        pass
                                    else:
                                        capitals.append(whole_div[n])
                
                #use the first from what is left as the street, the last as the region
                street = capitals[0]
                region = capitals[-1]
                
                #as for the city it has been decided to store only St.Petersburg and Moscow, in other cases store the original value
                #due to inability to define the city name correctly
                #the issue explanation is described below
                if region == "Санкт-Петербург,":
                    city = "Санкт-Петербург"
                elif region == "Москва,":
                    city = "Москва"
                else:
                    city = d[str(m)]
                #form json
                datasuka = {"X":m, "region": region, "city": city, "street": street, 'latitude': latitude, "longitude": longitude}
                data.append(datasuka)

                        #as for the city there are several cases:
                            #1. there were only 2 words left, recent cases show that then the second word whould be the name of the city
                            #but i should check it
                            #2. there are several words left, and mostly the first one from all of them is the city name, but it may depend
                            #3. still if the region name is Moscow or St.Petersburg, i should dublicate it as the city name
                
    except:
        #if no coords spotted then an empty lat-long pair should be added
        city = d[str(m)]
        datasuka = {"X":m, "region": "NA", "city": city, "street": "NA", 'latitude': "NA", "longitude": "NA"}
        data.append(datasuka)
    #no matter what the input should be cleared
    elem.clear()

#finally json saver should be added
with open('parsed_addresses.json', 'w') as fp:
            json.dump(data, fp)
