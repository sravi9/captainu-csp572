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

--prospects frequency
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS AS S
SET COLLEGEPROSPECTS_FREQUENCY = U.CUMM_SUM
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(COLLEGEPROSPECTS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM
FROM CAPTAINU.SUBSCRIPTION_MONTHS) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;

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

--videos frequency
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS AS S
SET VIDEOS_FREQUENCY = U.CUMM_SUM
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(VIDEOS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM
FROM CAPTAINU.SUBSCRIPTION_MONTHS) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;


--emails count, emails last date, clicks count and clicks response duration
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS S
SET EMAILS = U.EMAILS,
EMAILS_LASTDATE = U.EMAILS_LASTDATE,
CLICKS =U.CLICKS_CNT
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN E.EMAIL_ID IS NOT NULL THEN 1 ELSE 0 END) AS EMAILS,
SUM(CASE WHEN E.CLICKED_AT IS NOT NULL THEN 1 ELSE 0 END) AS CLICKS_CNT,
0 AS CLICKSDELAY_AVG, -- missing here
MAX(E.CREATED_AT) AS EMAILS_LASTDATE
 FROM CAPTAINU.SUBSCRIPTION_MONTHS SM
LEFT OUTER JOIN CAPTAINU.EMAILS E ON
SM.ATHLETE_ID= E.ATHLETE_ID AND
E.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;

--emails frequency
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS AS S
SET EMAILS_FREQUENCY = U.CUMM_SUM
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(EMAILS) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM
FROM CAPTAINU.SUBSCRIPTION_MONTHS) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;

--messages sent, messages last date

UPDATE CAPTAINU.SUBSCRIPTION_MONTHS S
SET messagessent = U.messagessent,
messagessent_lastdate = U.messagessent_lastdate
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN M.MESSAGE_ID IS NOT NULL THEN 1 ELSE 0 END) AS messagessent,
MAX(M.CREATED_AT) AS messagessent_lastdate
 FROM CAPTAINU.SUBSCRIPTION_MONTHS SM
LEFT OUTER JOIN CAPTAINU.MESSAGES M ON
SM.ATHLETE_ID= M.ATHLETE_ID AND
M.FROM_TYPE='Athlete' AND
M.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT 
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;

--messages sent frequency
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS AS S
SET messagessent_frequency = U.CUMM_SUM
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(messagessent) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM
FROM CAPTAINU.SUBSCRIPTION_MONTHS) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;

--messages received, messages last date

UPDATE CAPTAINU.SUBSCRIPTION_MONTHS S
SET messagesreceived = U.messagesreceived,
messagesreceived_lastdate = U.messagesreceived_lastdate
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN M.MESSAGE_ID IS NOT NULL THEN 1 ELSE 0 END) AS messagesreceived,
MAX(M.CREATED_AT) AS messagesreceived_lastdate
 FROM CAPTAINU.SUBSCRIPTION_MONTHS SM
LEFT OUTER JOIN CAPTAINU.MESSAGES M ON
SM.ATHLETE_ID= M.ATHLETE_ID AND
M.TO_TYPE='Athlete' AND (M.FROM_TYPE IS NOT NULL OR M.REPLY_TO_ID IS NOT NULL) AND
M.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT 
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;

-- messages received frequency
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS AS S
SET messagesreceived_frequency = U.CUMM_SUM
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(messagesreceived) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM
FROM CAPTAINU.SUBSCRIPTION_MONTHS) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;

--events enrolled, events enroll last date
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS S
SET eventsattended = U.eventsattended,
eventsattended_lastdate = U.eventsattended_lastdate
FROM
(SELECT SM.ATHLETE_ID,SM.MONTH_NUM,SUM(CASE WHEN E.EVENT_ID IS NOT NULL THEN 1 ELSE 0 END) AS eventsattended,
MAX(E.CREATED_AT) AS eventsattended_lastdate
 FROM CAPTAINU.SUBSCRIPTION_MONTHS SM
LEFT OUTER JOIN (SELECT AE.* FROM CAPTAINU.ATHLETE_EVENTS AE,CAPTAINU.EVENTS E WHERE AE.EVENT_ID=E.EVENT_ID) E ON
SM.ATHLETE_ID= E.ATHLETE_ID AND
E.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT 
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;

--events enrolled frequency
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS AS S
SET eventsattended_frequency = U.CUMM_SUM
FROM
(SELECT ATHLETE_ID,MONTH_NUM,SUM(eventsattended_frequency) OVER (PARTITION BY ATHLETE_ID ORDER BY MONTH_NUM ASC) AS CUMM_SUM
FROM CAPTAINU.SUBSCRIPTION_MONTHS) U
WHERE U.ATHLETE_ID=S.ATHLETE_ID AND
U.MONTH_NUM=S.MONTH_NUM;

--canceled & made team update
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS S
SET CANCELED_FLG=true,
MADE_TEAM=U.MADE_TEAM
FROM
(SELECT S.ATHLETE_ID,
       S.SUBSCRIPTION_ID,
       S.PLAIN_ID,
       S.CREATED_AT AS S_CREATED_AT,
       S.UPDATED_AT,
       S.STATUS,
       D.DOWNGRADE_ID,
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
AND S.STATUS  IN ('current','canceled')
AND (CASE WHEN S.STATUS='current' THEN D.CREATED_AT ELSE S.UPDATED_AT END) IS NOT NULL
ORDER BY S.ATHLETE_ID,
         S.CREATED_AT ASC) U
WHERE S.ATHLETE_ID=U.ATHLETE_ID AND
U.CANCELED_AT BETWEEN S.START_DT AND S.END_DT;

--EMAIL type fields
UPDATE CAPTAINU.SUBSCRIPTION_MONTHS S
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
 FROM CAPTAINU.SUBSCRIPTION_MONTHS SM
LEFT OUTER JOIN CAPTAINU.EMAILS E ON
SM.ATHLETE_ID= E.ATHLETE_ID AND
E.CREATED_AT BETWEEN SM.START_DT AND SM.END_DT
GROUP BY SM.ATHLETE_ID,SM.MONTH_NUM) U
WHERE
U.ATHLETE_ID= S.ATHLETE_ID AND
U.MONTH_NUM= S.MONTH_NUM;