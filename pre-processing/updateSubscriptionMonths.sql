csp572/captainUcsp572
readonly/re@d0n!y

-- prospects count, prospects last date
UPDATE captainu.subscription_months as S
   SET COLLEGEPROSPECTS = U.prospect_cnt,
       COLLEGEPROSPECTS_LASTDATE = U.prospect_lastdate
FROM (SELECT sm.athlete_id,
             sm.subscription_id,
             sm.month_num,
             sm.start_dt,
             sm.end_dt,
             SUM(CASE WHEN p.prospect_id IS NOT NULL THEN 1 ELSE 0 END) AS prospect_cnt,
             MAX(p.created_at) AS prospect_lastdate
      FROM captainu.subscription_months sm
        LEFT OUTER JOIN CAPTAINU.PROSPECTS p
                     ON sm.athlete_id = p.athlete_id
                    AND p.created_at BETWEEN sm.start_dt
                    AND sm.end_dt
      GROUP BY sm.athlete_id,
               sm.subscription_id,
               sm.month_num,
               sm.start_dt,
               sm.end_dt) AS U
WHERE U.ATHLETE_ID = S.ATHLETE_ID
AND   U.MONTH_NUM = S.MONTH_NUM;

commit;

--prospects frequency
UPDATE CAPTAINU.subscription_months AS S
SET COLLEGEPROSPECTS_FREQUENCY = U.CUMM_SUM,
COLLEGEPROSPECTS_MOVINGAVG = U.MOVING_AVG,
COLLEGEPROSPECTS_MOVINGSD = U.MOVING_SD
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(COLLEGEPROSPECTS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM,
AVG(COLLEGEPROSPECTS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_AVG,
STDDEV_POP(COLLEGEPROSPECTS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_SD
FROM CAPTAINU.subscription_months) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;

commit;
--videos count, videos last date
UPDATE captainu.subscription_months AS S
   SET VIDEOS = U.video_cnt,
       VIDEOS_LASTDATE = U.video_lastdate
FROM (SELECT sm.athlete_id,
             sm.subscription_id,
             sm.month_num,
             sm.start_dt,
             sm.end_dt,
             SUM(CASE WHEN v.video_id IS NOT NULL THEN 1 ELSE 0 END) AS video_cnt,
             MAX(v.created_at) AS video_lastdate
      FROM captainu.subscription_months sm
        LEFT OUTER JOIN CAPTAINU.VIDEOS v
                     ON sm.athlete_id = v.athlete_id
                    AND v.created_at BETWEEN sm.start_dt
                    AND sm.end_dt
      GROUP BY sm.athlete_id,
               sm.subscription_id,
               sm.month_num,
               sm.start_dt,
               sm.end_dt) U
WHERE U.ATHLETE_ID = S.ATHLETE_ID
AND   U.MONTH_NUM = S.MONTH_NUM;

commit;
--videos frequency
UPDATE CAPTAINU.subscription_months AS S
SET VIDEOS_FREQUENCY = U.CUMM_SUM,
VIDEOS_MOVINGAVG = U.MOVING_AVG,
VIDEOS_MOVINGSD = U.MOVING_SD
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(VIDEOS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM,
AVG(VIDEOS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_AVG,
STDDEV_POP(VIDEOS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_SD
FROM CAPTAINU.subscription_months) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;
commit;

--emails count, emails last date, clicks count and clicks response duration
UPDATE CAPTAINU.subscription_months S
SET EMAILS = U.EMAILS,
EMAILS_LASTDATE = U.EMAILS_LASTDATE,
CLICKS =U.CLICKS_CNT
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN E.EMAIL_ID IS NOT NULL THEN 1 ELSE 0 END) AS EMAILS,
SUM(CASE WHEN E.CLICKED_AT IS NOT NULL THEN 1 ELSE 0 END) AS CLICKS_CNT,
0 AS CLICKSDELAY_AVG, -- missing here
MAX(E.CREATED_AT) AS EMAILS_LASTDATE
 FROM CAPTAINU.subscription_months SM
LEFT OUTER JOIN CAPTAINU.EMAILS E ON
SM.ATHLETE_ID= E.ATHLETE_ID AND
E.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;
commit;
--emails frequency
UPDATE CAPTAINU.subscription_months AS S
SET EMAILS_FREQUENCY = U.CUMM_SUM,
EMAILS_MOVINGAVG = U.MOVING_AVG,
EMAILS_MOVINGSD = U.MOVING_SD
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(EMAILS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM,
AVG(EMAILS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_AVG,
STDDEV_POP(EMAILS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_SD
FROM CAPTAINU.subscription_months) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;
commit;

commit;
--messages sent, messages last date

UPDATE CAPTAINU.subscription_months S
SET messagessent = U.messagessent,
messagessent_lastdate = U.messagessent_lastdate
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN M.MESSAGE_ID IS NOT NULL THEN 1 ELSE 0 END) AS messagessent,
MAX(M.CREATED_AT) AS messagessent_lastdate
 FROM CAPTAINU.subscription_months SM
LEFT OUTER JOIN CAPTAINU.MESSAGES M ON
SM.ATHLETE_ID= M.ATHLETE_ID AND
M.FROM_TYPE='Athlete' AND
M.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT 
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;
commit;
--messages sent frequency
UPDATE CAPTAINU.subscription_months AS S
SET messagessent_frequency = U.CUMM_SUM,
messagessent_movingavg = U.moving_avg,
messagessent_movingsd = U.moving_sd
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(messagessent) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM,
AVG(messagessent) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_AVG,
STDDEV_POP(messagessent) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_SD
FROM CAPTAINU.subscription_months) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;
commit;
--messages received, messages last date

UPDATE CAPTAINU.subscription_months S
SET messagesreceived = U.messagesreceived,
messagesreceived_lastdate = U.messagesreceived_lastdate
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN M.MESSAGE_ID IS NOT NULL THEN 1 ELSE 0 END) AS messagesreceived,
MAX(M.CREATED_AT) AS messagesreceived_lastdate
 FROM CAPTAINU.subscription_months SM
LEFT OUTER JOIN CAPTAINU.MESSAGES M ON
SM.ATHLETE_ID= M.ATHLETE_ID AND
M.TO_TYPE='Athlete' AND (M.FROM_TYPE IS NOT NULL OR M.REPLY_TO_ID IS NOT NULL) AND
M.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT 
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;
commit;
-- messages received frequency
UPDATE CAPTAINU.subscription_months AS S
SET messagesreceived_frequency = U.CUMM_SUM,
messagesreceived_movingavg = U.moving_avg,
messagesreceived_movingsd = U.moving_sd
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(messagesreceived) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM,
AVG(messagesreceived) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_AVG,
STDDEV_POP(messagesreceived) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_SD
FROM CAPTAINU.subscription_months) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;
commit;

--events enrolled, events enroll last date
UPDATE CAPTAINU.subscription_months S
SET eventsattended = U.eventsattended,
eventsattended_lastdate = U.eventsattended_lastdate
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN E.EVENT_ID IS NOT NULL THEN 1 ELSE 0 END) AS eventsattended,
MAX(E.CREATED_AT) AS eventsattended_lastdate
 FROM CAPTAINU.subscription_months SM
LEFT OUTER JOIN (SELECT AE.* FROM CAPTAINU.ATHLETE_EVENTS AE,CAPTAINU.EVENTS E WHERE AE.EVENT_ID=E.EVENT_ID) E ON
SM.ATHLETE_ID= E.ATHLETE_ID AND
E.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT 
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;
commit;
--events enrolled frequency
UPDATE CAPTAINU.subscription_months AS S
SET eventsattended_frequency = U.CUMM_SUM,
eventsattended_movingavg = U.moving_avg,
eventsattended_movingsd = U.moving_sd
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(eventsattended) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM,
AVG(eventsattended) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_AVG,
STDDEV_POP(eventsattended) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_SD
FROM CAPTAINU.subscription_months) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;
commit;
--canceled & made team update
UPDATE CAPTAINU.subscription_months S
SET CANCELED_FLG=true,
MADE_TEAM=UPT.MADE_TEAM
FROM
(SELECT * FROM 
(SELECT S.ATHLETE_ID,S.MONTH_NUM,RANK() OVER (PARTITION BY S.ATHLETE_ID,S.MONTH_NUM ORDER BY U.DOWNGRADE_ID DESC) AS RNK,
U.DOWNGRADE_ID,U.MADE_TEAM,U.CANCELED_AT FROM
CAPTAINU.SUBSCRIPTION_MONTHS S,
(SELECT S.ATHLETE_ID,
        S.SUBSCRIPTION_ID,
        S.PLAIN_ID,
        S.CREATED_AT AS S_CREATED_AT,
        S.UPDATED_AT,
        S.STATUS,
        (CASE WHEN D.DOWNGRADE_ID IS NULL THEN -1 ELSE D.DOWNGRADE_ID END ) AS DOWNGRADE_ID,
        D.MADE_TEAM,
        D.CREATED_AT AS D_CREATED_AT,
        (CASE WHEN S.STATUS='current' THEN D.CREATED_AT ELSE S.UPDATED_AT END) AS CANCELED_AT 
FROM CAPTAINU.SUBSCRIPTIONS S
  LEFT OUTER JOIN (SELECT *
                   FROM CAPTAINU.DOWNGRADES
                   WHERE DOWNGRADE_ID IN (SELECT MAX(DOWNGRADE_ID) 
                                          FROM CAPTAINU.DOWNGRADES
                                          GROUP BY ATHLETE_ID,
                                                    SUBSCRIPTION_ID)) D 
               ON S.SUBSCRIPTION_ID = D.SUBSCRIPTION_ID 
              AND EXTRACT (YEAR FROM D.CREATED_AT) < 2017 
WHERE EXTRACT(YEAR FROM S.CREATED_AT) < 2017 
AND   S.PLAIN_ID IN (0,1,2,5,6,7) 
AND   S.STATUS IS NOT NULL
ORDER BY S.ATHLETE_ID,
          S.CREATED_AT ASC) U
WHERE S.ATHLETE_ID=U.ATHLETE_ID AND
U.CANCELED_AT BETWEEN S.START_DT AND S.END_DT) F
WHERE F.RNK = 1) UPT
WHERE S.ATHLETE_ID=UPT.ATHLETE_ID AND
S.MONTH_NUM = UPT.MONTH_NUM;
COMMIT;

--EMAIL type fields
UPDATE CAPTAINU.subscription_months S
SET Eathlete_newsletter=U.Eathlete_newsletter,
Eathlete_new=U.Eathlete_new,
Eathlete_new_info_request=U.Eathlete_new_info_request,
ECCNote=U.ECCNote,
ECCNote_camp=U.ECCNote_camp,
Ecoach_list_known_updated=U.Ecoach_list_known_updated,
ECoachEmailOpen=U.ECoachEmailOpen,
ECoachEval=U.ECoachEval,
ECoachImport=U.ECoachImport,
ECoachSearchHit=U.ECoachSearchHit,
ECoachVideoViewHit=U.ECoachVideoViewHit,
ECoachVisit=U.ECoachVisit,
Ecolleges_going_to_the_event=U.Ecolleges_going_to_the_event,
Efailed_subscription=U.Efailed_subscription,
Ehighlight_video__regathlete_11=U.Ehighlight_video__regathlete_11,
Ehighlight_video__regathlete_2=U.Ehighlight_video__regathlete_2,
Ehighlight_video__regathlete_4=U.Ehighlight_video__regathlete_4,
EEmailsDigest=U.EEmailsDigest,
Eparent_new=U.Eparent_new,
Eparent_welcome=U.Eparent_welcome,
Epost_event_email=U.Epost_event_email,
Esms_update=U.Esms_update
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN E.EMAIL_ID IS NOT NULL THEN 1 ELSE 0 END) AS EMAILS,
SUM(CASE WHEN E.EMAIL_TYPE='Athlete newsletter' THEN 1 ELSE 0 END) AS Eathlete_newsletter,
SUM(CASE WHEN E.EMAIL_TYPE='athlete_new' THEN 1 ELSE 0 END) AS Eathlete_new,
SUM(CASE WHEN E.EMAIL_TYPE='athlete_new_info_request' THEN 1 ELSE 0 END) AS Eathlete_new_info_request,
SUM(CASE WHEN E.EMAIL_TYPE='CCNote' THEN 1 ELSE 0 END) AS ECCNote,
SUM(CASE WHEN E.EMAIL_TYPE='CCNote-camp' THEN 1 ELSE 0 END) AS ECCNote_camp,
SUM(CASE WHEN E.EMAIL_TYPE='coach_list_known_updated' THEN 1 ELSE 0 END) AS Ecoach_list_known_updated,
SUM(CASE WHEN E.EMAIL_TYPE='CoachEmailOpen' THEN 1 ELSE 0 END) AS ECoachEmailOpen,
SUM(CASE WHEN E.EMAIL_TYPE='CoachEval' THEN 1 ELSE 0 END) AS ECoachEval,
SUM(CASE WHEN E.EMAIL_TYPE='CoachImport' THEN 1 ELSE 0 END) AS ECoachImport,
SUM(CASE WHEN E.EMAIL_TYPE='CoachSearchHit' THEN 1 ELSE 0 END) AS ECoachSearchHit,
SUM(CASE WHEN E.EMAIL_TYPE='CoachVideoViewHit' THEN 1 ELSE 0 END) AS ECoachVideoViewHit,
SUM(CASE WHEN E.EMAIL_TYPE='CoachVisit' THEN 1 ELSE 0 END) AS ECoachVisit,
SUM(CASE WHEN E.EMAIL_TYPE='colleges_going_to_the_event' THEN 1 ELSE 0 END) AS Ecolleges_going_to_the_event,
SUM(CASE WHEN E.EMAIL_TYPE='failed_subscription' THEN 1 ELSE 0 END) AS Efailed_subscription,
SUM(CASE WHEN E.EMAIL_TYPE='highlight_video__regathlete_11' THEN 1 ELSE 0 END) AS Ehighlight_video__regathlete_11,
SUM(CASE WHEN E.EMAIL_TYPE='highlight_video__regathlete_2' THEN 1 ELSE 0 END) AS Ehighlight_video__regathlete_2,
SUM(CASE WHEN E.EMAIL_TYPE='highlight_video__regathlete_4' THEN 1 ELSE 0 END) AS Ehighlight_video__regathlete_4,
SUM(CASE WHEN E.EMAIL_TYPE='HitsDigest' THEN 1 ELSE 0 END) AS EEmailsDigest,
SUM(CASE WHEN E.EMAIL_TYPE='parent_new' THEN 1 ELSE 0 END) AS Eparent_new,
SUM(CASE WHEN E.EMAIL_TYPE='parent_welcome' THEN 1 ELSE 0 END) AS Eparent_welcome,
SUM(CASE WHEN E.EMAIL_TYPE='post_event_email' THEN 1 ELSE 0 END) AS Epost_event_email,
SUM(CASE WHEN E.EMAIL_TYPE='sms_update' THEN 1 ELSE 0 END) AS Esms_update
 FROM CAPTAINU.subscription_months SM
LEFT OUTER JOIN CAPTAINU.EMAILS E ON
SM.ATHLETE_ID= E.ATHLETE_ID AND
E.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;
commit;

--Calendar month
ALTER TABLE CAPTAINU.subscription_months ADD CAL_MONTH VARCHAR(10);

UPDATE CAPTAINU.subscription_months SET CAL_MONTH=CAST(DATE_PART('year',start_dt) AS VARCHAR) || '-' ||
(CASE WHEN DATE_PART('month',start_dt) < 10 THEN '0' ||cast (DATE_PART('month',start_dt) AS VARCHAR) ELSE 
CAST(DATE_PART('month',start_dt) AS VARCHAR) END);

COMMIT;

--One month Churn

UPDATE CAPTAINU.SUBSCRIPTION_MONTHS S
SET ONE_MONTH_CHURN = TRUE
FROM
(SELECT N.* FROM
(SELECT ATHLETE_ID,(CASE WHEN CAST (split_part(CAL_MONTH,'-',2) AS INTEGER) = 12 
THEN CAST (CAST (split_part(CAL_MONTH,'-',1) AS INTEGER) + 1 AS VARCHAR)||'-01'
ELSE CAST(DATE_PART('year',start_dt) AS VARCHAR) || '-' ||
(CASE WHEN DATE_PART('month',start_dt) < 9 THEN '0' ||cast (DATE_PART('month',start_dt)+1 AS VARCHAR) ELSE 
CAST(DATE_PART('month',start_dt)+1 AS VARCHAR) END) END ) NEXT_MONTH,CAL_MONTH FROM CAPTAINU.SUBSCRIPTION_MONTHS WHERE CANCELED_FLG=TRUE AND MADE_TEAM=FALSE) N
LEFT OUTER JOIN CAPTAINU.SUBSCRIPTION_MONTHS S ON
S.ATHLETE_ID=N.ATHLETE_ID AND
S.CAL_MONTH = N.NEXT_MONTH
WHERE S.ATHLETE_ID IS NULL ) U
WHERE S.ATHLETE_ID = U.ATHLETE_ID AND
S.CAL_MONTH = U.CAL_MONTH;
COMMIT;


--Hits and Hits last date
UPDATE CAPTAINU.subscription_months S
SET hits = U.hits,
hits_lastdate = U.hits_lastdate
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN H.ATHLETE_ID IS NOT NULL THEN 1 ELSE 0 END) AS hits,
MAX(H.HIT_DATE) AS hits_lastdate
 FROM CAPTAINU.subscription_months SM
LEFT OUTER JOIN CAPTAINU.HITS H ON
SM.ATHLETE_ID= H.ATHLETE_ID AND
H.HIT_DATE BETWEEN SM.START_DT AND SM.END_DT 
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;
COMMIT;

--Hits frequency, moving average and sd
UPDATE CAPTAINU.subscription_months AS S
SET HITS_FREQUENCY = U.CUMM_SUM,
HITS_MOVINGAVG = U.MOVING_AVG,
HITS_MOVINGSD = U.MOVING_SD
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(HITS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM,
AVG(HITS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_AVG,
STDDEV_POP(HITS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS MOVING_SD
FROM CAPTAINU.subscription_months) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;
COMMIT;

--Rest all hit columns
UPDATE CAPTAINU.subscription_months S
SET Hprofileview = U.Hprofileview,
Hcoachimport = U.Hcoachimport,
Hmessage = U.Hmessage,
Hsearchhit = U.Hsearchhit,
HNULL = U.HNULL,
Hcoacheval = U.Hcoacheval,
Hvideoview = U.Hvideoview,
Hemailopen = U.Hemailopen
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN H.ATHLETE_ID IS NOT NULL THEN 1 ELSE 0 END) AS hits,
SUM(CASE WHEN H.HIT_TYPE='Profile_view' THEN 1 ELSE 0 END) as Hprofileview,
SUM(CASE WHEN H.HIT_TYPE='coach_import' THEN 1 ELSE 0 END) as Hcoachimport,
SUM(CASE WHEN H.HIT_TYPE='message' THEN 1 ELSE 0 END) as Hmessage,
SUM(CASE WHEN H.HIT_TYPE='search_hit' THEN 1 ELSE 0 END) as Hsearchhit,
SUM(CASE WHEN H.HIT_TYPE IS NULL AND H.ATHLETE_ID IS NOT NULL THEN 1 ELSE 0 END) as HNULL,
SUM(CASE WHEN H.HIT_TYPE='coach_eval' THEN 1 ELSE 0 END) as Hcoacheval,
SUM(CASE WHEN H.HIT_TYPE='video_view' THEN 1 ELSE 0 END) as Hvideoview,
SUM(CASE WHEN H.HIT_TYPE='email_open' THEN 1 ELSE 0 END) as Hemailopen
 FROM CAPTAINU.subscription_months SM
LEFT OUTER JOIN CAPTAINU.HITS H ON
SM.ATHLETE_ID= H.ATHLETE_ID AND
H.HIT_DATE BETWEEN SM.START_DT AND SM.END_DT 
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;
COMMIT;
