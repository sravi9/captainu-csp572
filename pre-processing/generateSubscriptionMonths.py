import datetime
from dateutil.relativedelta import relativedelta

def generateMonths(athleteId, subscriptionId, startDate, cancelDate):
    generatedData = list()
    element = dict(athleteId=athleteId,subscriptionId=subscriptionId)
    if startDate.day == 31:
        if startDate.month == 7 or startDate.month == 12:
            endDate = startDate.replace(hour=23,minute=59,second=59) + relativedelta(months=1)
        else:
            endDate = startDate.replace(hour=23,minute=59,second=59) + relativedelta(months=1) \
                + relativedelta(days=1)
    elif startDate.day == 29 or startDate.day == 30:
        if startDate.month == 1:
            endDate = startDate.replace(hour=23,minute=59,second=59) + relativedelta(months=1) \
                + relativedelta(days=1)
        else:
            endDate = startDate.replace(hour=23,minute=59,second=59) + relativedelta(months=1)
    else:
        endDate = startDate.replace(hour=23,minute=59,second=59) + relativedelta(months=1)
    if endDate < cancelDate:
        element.update(startDate=startDate, endDate=endDate.replace(hour=23,minute=59,second=59)
                       + relativedelta(days=-1))
        generatedData.append(element)
        generatedData.extend(generateMonths(athleteId, subscriptionId, endDate.replace(hour=0,minute=0,second=0),
                                    cancelDate))
    else:
        element.update(startDate=startDate, endDate=endDate.replace(hour=23,minute=59,second=59)) # Replace with End Date to complete billing cycle
        generatedData.append(element)
    return generatedData
	
import psycopg2

conn = \
    psycopg2.connect(host='captainu.c72j1qnfp0sw.us-east-1.rds.amazonaws.com'
                     , port=5432, dbname='chunkdb', user='csp572',
                     password='captainUcsp572')
conn.set_session(autocommit=True)
cursor = conn.cursor()
cursor.execute('SELECT S.ATHLETE_ID,\
       S.SUBSCRIPTION_ID,\
       S.PLAIN_ID,\
       S.CREATED_AT AS S_CREATED_AT,\
       S.UPDATED_AT,\
       S.STATUS,\
       D.DOWNGRADE_ID,\
       D.MADE_TEAM,\
       D.CREATED_AT AS D_CREATED_AT \
FROM CAPTAINU.SUBSCRIPTIONS S\
  LEFT OUTER JOIN (SELECT *\
                   FROM CAPTAINU.DOWNGRADES\
                   WHERE DOWNGRADE_ID IN (SELECT MAX(DOWNGRADE_ID) \
                                          FROM CAPTAINU.DOWNGRADES\
                                          GROUP BY ATHLETE_ID,\
                                                   SUBSCRIPTION_ID)) D \
               ON S.SUBSCRIPTION_ID = D.SUBSCRIPTION_ID \
              AND EXTRACT (YEAR FROM D.CREATED_AT) < 2017 \
WHERE EXTRACT(YEAR FROM S.CREATED_AT) < 2017 \
AND   S.PLAIN_ID IN (0,1,2,5,6,7) \
AND   S.STATUS IS NOT NULL \
ORDER BY S.ATHLETE_ID,\
         S.CREATED_AT ASC;'
               )

previousAthleteId = -1
startDate = datetime.date.today()
endDate = datetime.date.today()
tempCache = list()
for record in cursor:
    athleteId = record[0]
    if athleteId == previousAthleteId:
        if record[5] == 'canceled':
            endDate = (record[8] if not record[8]
                       is None else record[4])
            tempCache.extend(generateMonths(record[0], record[1], startDate,
                             endDate))
        elif record[5] == 'current':
            endDate = (record[8] if not record[8]
                       is None else datetime.datetime(2017,1,31,0,0,0,))
            tempCache.extend(generateMonths(record[0], record[1], startDate,
                             endDate))
    elif record[5] == 'canceled':
        previousAthleteId = athleteId
        endDate = (record[8] if not record[8] is None else record[4])
        tempCache.extend(generateMonths(record[0], record[1], record[3], endDate))
    elif record[5] == 'current':
        previousAthleteId = athleteId
        endDate = (record[8] if not record[8]
                   is None else datetime.datetime(2017,1,31,0,0,0,))
        tempCache.extend(generateMonths(record[0], record[1], record[3], endDate))
    else:
        previousAthleteId = athleteId
        startDate = record[3]
        endDate = record[4]


count = 0
previousAthleteId = -1

for item in tempCache:
    if item['athleteId'] == previousAthleteId:
        count = count + 1
        cursor.execute('INSERT INTO CAPTAINU.SUBSCRIPTION_MONTHS (athlete_id,subscription_id,month_num,start_dt,end_dt) VALUES (%s,%s,%s,%s,%s);'
                       , [item['athleteId'], item['subscriptionId'], count, item['startDate'],
                       item['endDate']])
    else:
        count = 1
        previousAthleteId = item['athleteId']
        cursor.execute('INSERT INTO CAPTAINU.SUBSCRIPTION_MONTHS (athlete_id,subscription_id,month_num,start_dt,end_dt) VALUES (%s,%s,%s,%s,%s);'
                       , [item['athleteId'], item['subscriptionId'], count, item['startDate'],
                       item['endDate']])