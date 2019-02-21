#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 02:48:14 2019

@author: antongaev
"""
import requests
from bs4 import BeautifulSoup
import datetime
import csv
from time import sleep
import json

def get_html(url):
    r = requests.get(url)
    return r.text

def get_all_links(html):
    soup = BeautifulSoup(html, 'lxml')
    
    links = soup.find('table', class_="fuel_price").text.split("ДТ")
    closer_to_the_price = links[1].split(" руб.")
    closer_to_the_price_two = closer_to_the_price[2].split(".")
    the_price = closer_to_the_price_two[3][2] + closer_to_the_price_two[3][3] + "." + closer_to_the_price_two[4][0] + closer_to_the_price_two[4][1]
    return the_price

def get_dollars(html):
    soup = BeautifulSoup(html, 'lxml')
    links = soup.find('table', class_="data").text.split("Доллар США")
    closer_to_the_price = links[1]
    return closer_to_the_price

def main():
    all_html = get_html('http://natrakte.ru/fuel/leningradskaya_oblast/')
    price = get_all_links(all_html)
    data = []

    reader = csv.reader(open('usd_diesel.csv', 'r', encoding="utf-8", newline =''))
    #convert reader into json
    for row in reader:
            datasuka = {"calendar": row[1], "diesel_price": row[2], "dollar": row[3]}
            data.append(datasuka)
    this_day = datetime.date.today() 
    
    last_date = datetime.datetime.strptime(data[1].get("calendar"), "%Y-%M-%d").date()
    ur = "http://www.cbr.ru/currency_base/daily/?date_req="    
    
    while last_date != this_day:
        url = ur + str(last_date)
        all_html = get_html(url)
        got_dollars = get_dollars(all_html)
        the_dollar = got_dollars[1:6]
        last_date += datetime.timedelta(days=1)
        data = [{"calendar": str(last_date), "diesel_price": price, "dollar": the_dollar}] + data
        sleep(1)
        
    #finally json saver should be added
    with open('usd_dollar.json', 'w') as fp:
            json.dump(data, fp)

if __name__ == '__main__':
    main()
