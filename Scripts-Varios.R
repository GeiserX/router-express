#########################################################################
### Cambiar parámetro en todos los Tenda vía archivo de configuración ###
#########################################################################

setwd("/media/tftpSRV/")
dirs <- dir(pattern = ".cfg$")

for(i in 42:length(dirs)){
  archivo <<- scan(dirs[i], what = character(), skip = 0, sep = "\n")
  id <- pmatch('rm_web_ip=', archivo)
  archivo[id] <- "rm_web_ip=X.X.X.X"
  write(archivo, paste0("/home/tecnico/WebApp/archivos-samba/", dirs[i]))
  system(sprintf("smbclient -U dhcp66 //Y.Y.Y.Y/tftpSRV t1l2cm3r -c \"put %s %s\"", paste0("/home/tecnico/WebApp/archivos-samba/", dirs[i]), dirs[i]))
}

#################################
### Cambiar parámetro vía WEB ### (código python)
#################################

# import csv, os, signal
# 
# def signal_handler(signum, frame):
#   raise Exception("Timed out!")
# 
# signal.signal(signal.SIGALRM, signal_handler)
# signal.alarm(12) #segundos
# 
# c = csv.reader(open("/home/tecnico/Descargas/a.csv"))
# clist = list(c)
# for i in range(106,len(clist)):
#   print i
# print clist[i][1]
# try:
#   os.system('python /home/tecnico/Escritorio/tendaWEB.py ' + clist[i][1])
# except Exception, msg:
#   print "Timed out!"


# # -*- coding: utf-8 -*-
# from selenium import webdriver
# from selenium.webdriver.common.by import By
# from selenium.webdriver.common.keys import Keys
# from selenium.webdriver.support.ui import Select
# from selenium.common.exceptions import NoSuchElementException
# from selenium.common.exceptions import NoAlertPresentException
# import unittest, time, re
# import sys
# 
# class TendaWEB(unittest.TestCase):
#   def setUp(self):
#   self.driver = webdriver.Firefox()
# self.driver.implicitly_wait(30)
# self.base_url = "http://%s:8080" % (self.IP)
# self.verificationErrors = []
# self.accept_next_alert = True
# 
# def test_tenda_w_e_b(self):
#   driver = self.driver
# driver.get(self.base_url + "/login.asp")
# driver.find_element_by_name("Password").clear()
# driver.find_element_by_name("Password").send_keys("PASSWORD")
# driver.find_element_by_link_text("OK").click()
# time.sleep(2)
# driver.find_element_by_link_text("Advanced").click()
# time.sleep(2)
# driver.find_element_by_css_selector("span.nav-text").click()
# time.sleep(2)
# driver.find_element_by_link_text("Remote Web Management").click()
# driver.switch_to_frame(driver.find_element_by_tag_name("iframe")) #iframe
# driver.find_element_by_id("RMsIP1").clear()
# driver.find_element_by_id("RMsIP1").send_keys("X.X.X.X")
# driver.find_element_by_css_selector("input.btn").click()
# driver.switch_to_default_content()
# driver.find_element_by_css_selector("#advance > span.nav-text").click()
# time.sleep(2)
# driver.find_element_by_link_text("DNS Settings").click()
# driver.switch_to_frame(driver.find_element_by_tag_name("iframe")) #iframe
# if driver.find_element_by_name("DNSEN").is_selected():
#   driver.find_element_by_name("DNSEN").click()
# driver.find_element_by_css_selector("input.btn").click()
# 
# def is_element_present(self, how, what):
#   try: self.driver.find_element(by=how, value=what)
# except NoSuchElementException, e: return False
# return True
# 
# def is_alert_present(self):
#   try: self.driver.switch_to_alert()
# except NoAlertPresentException, e: return False
# return True
# 
# def close_alert_and_get_its_text(self):
#   try:
#   alert = self.driver.switch_to_alert()
# alert_text = alert.text
# if self.accept_next_alert:
#   alert.accept()
# else:
#   alert.dismiss()
# return alert_text
# finally: self.accept_next_alert = True
# 
# def tearDown(self):
#   self.driver.quit()
# self.assertEqual([], self.verificationErrors)
# 
# if __name__ == "__main__":
#   if len(sys.argv) > 1:
#   TendaWEB.IP = sys.argv.pop()
# unittest.main()

