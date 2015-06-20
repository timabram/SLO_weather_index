#Grabbing weather data for 2014 for SLO from wunderground.com

import urllib2
from bs4 import BeautifulSoup
 
#Create/open a file called wunder.txt (which will be a csv)
f = open('Tuscany_weather_2014.txt', 'w')
 
#Iterate through months and day
for m in range(1, 13):
  for d in range(1, 32):
 
#Check if already gone through month
    if (m == 2 and d > 28):
      break
    elif (m in [4, 6, 9, 11] and d > 30):
      break
 
	#Open wundergorund.com url
    timestamp = '2014' + str(m) + str(d)
    print "Getting data for " + timestamp
	#Airport code: KSBP = San Luis Obispo, CA
	#Airport code: KMCJ = Houston, TX (77030)
	#Airport code: KMDW = Chicago, IL
	#Airport code: LIRV = Tuscany, Italy
    url = "http://www.wunderground.com/history/airport/LIRV/2014/" + str(m) + "/" + str(d) + "/DailyHistory.html"
    page = urllib2.urlopen(url)
	
	#Format day for timestamp
    if len(str(d)) < 2:
      dStamp = '0' + str(d)
    else:
      dStamp = str(d)
    if len(str(m)) < 2:
      mStamp = '0' + str(m)
    else:
      mStamp = str(m) 
	#Build timestamp
    timestamp2 = '2014' + '-' + mStamp + '-' + dStamp
	
	#Get temperature from page
    #datanew=[]
    soup = BeautifulSoup(page)
    total_rows = len(soup('tbody')[6].find_all('tr'))
    total_cols = len(soup('tbody')[6].find_all('tr')[0].find_all('td'))
    #print 'total rows: ' + str(total_rows) + '    ' + 'total cols: ' + str(total_cols)
    for r in range(1,total_rows+1):
        for c in range(1,total_cols+1):
            if total_cols ==12 and c ==3:
              f.write('-' + ';')
            data = soup('tbody')[6].find_all('tr')[r-1].find_all('td')[c-1].get_text().encode('latin-1')
            data = data.replace('\n', '')
            data = data.replace('\t', '')			
	#datanew = datanew + ',' + data
	#Format month for timestamp
            f.write(data + ';')
        f.write(timestamp2 + ';' + 'Tuscany' + '\n')


	
    #f.write(timestamp +'\n')

	#th = table header (titles)
	#tr = table rows
	#td = table columns
	
	# can also use: soup('tbody')[6]
	
	#ex: soup('table')[4].find_all('tr')[0].find_all('th') -> length = 13 columns (find_all('td') -> 0)
	
	
	# a = the row number of the table (particular time of day)
	# b = the column of the extracted row (should be the same across all tables)
	# dayTemp = soup.body.nobr.b.string
 
f.close()