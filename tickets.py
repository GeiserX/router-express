# -*- coding: utf-8 -*-
# system('python tickets.py "Sergio" "Fernandez" "sergio@ole" "950001" "968868968"')
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
import unittest, time, re

from selenium.webdriver.common.action_chains import ActionChains
import sys

TLFN = sys.argv.pop()
CC = sys.argv.pop()
EMAIL = sys.argv.pop()
SURN = sys.argv.pop()
NAME = sys.argv.pop()

driver = webdriver.PhantomJS()
driver.get("http://averias.emartinez.es:8081/helpdesk/WebObjects/Helpdesk.woa/wa")

driver.find_element_by_id("userName").send_keys("admin")
driver.find_element_by_id("password").send_keys("PASSWORD")
driver.find_element_by_name("11.1.1.9.7.4.1.11.0.1.0").click()
#driver.save_screenshot("/home/tecnico/WebApp/data/prueba.png")
    
#Nuevo cliente
driver.find_element_by_xpath("//img[@alt='Clientes']").click()
driver.find_element_by_css_selector("div.squareButtonMiddle").click()
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.13.0.0.1").clear()
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.13.0.0.1").send_keys(NAME) #NOMBRE
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.17.0.0.1").clear()
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.17.0.0.1").send_keys(SURN) #APELLIDO
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.21.1.1.0.0.1").clear()
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.21.1.1.0.0.1").send_keys(EMAIL) #EMAIL
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.33.0.0.1").clear()
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.33.0.0.1").send_keys(CC) #CÓDIGO CLIENTE
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.41.0.0.1").clear()
driver.find_element_by_name("7.25.0.0.0.2.5.5.1.8.1.3.0.1.3.1.1.0.0.1.1.41.0.0.1").send_keys(TLFN) #TELÉFONO
driver.find_element_by_css_selector("div.aquaMiddleSel").click()
    
#Nuevo ticket
driver.find_element_by_xpath("//img[@alt='Configuración']").click()
element = driver.find_element_by_xpath("//div[@id='preferences-menu']/div/div[23]")
hover = ActionChains(driver).move_to_element(element)
hover.perform()
time.sleep(1)
driver.find_element_by_xpath("//div[@id='preferences-menu']/div/div[24]/ul/li[5]/a/div/div[2]").click()
driver.find_element_by_xpath("//input[@name='Field Separator' and @value='1']").click()
driver.find_element_by_xpath("//input[@type='file']").send_keys("ticketFIN.csv")
driver.find_element_by_css_selector("div.aquaMiddleSel").click()
time.sleep(4)
driver.find_element_by_id("logoutLink").click()
driver.close()
driver.quit()

