psql -h captainu.c72j1qnfp0sw.us-east-1.rds.amazonaws.com -p 5432 -U csp572 -W chunkdb

DROP TABLE IF EXISTS captainu.athletes CASCADE;

CREATE TABLE captainu.athletes
(
   athlete_id       integer,
   city             varchar(128),
   state            varchar(64),
   zip              varchar(128),
   gender           varchar(8),
   birthday         date,
   graduation_year  integer,
   gpa              numeric(10,2),
   sat_math         integer,
   sat_reading      integer,
   sat_writing      integer,
   act              integer,
   height           numeric(10,2),
   weight           integer,
   sport            varchar(64),
   created_at       timestamp,
   updated_at       timestamp,
   committed        varchar(64),
   tag              varchar(64)
);

COMMIT;

\copy captainu.athletes FROM '/home/ec2-user/Athletes.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.downgrades CASCADE;

CREATE TABLE captainu.downgrades
(
   athlete_id       integer,
   downgrade_id     integer,
   subscription_id  integer,
   made_team        boolean,
   created_at       timestamp,
   updated_at       timestamp
);

COMMIT;

\copy captainu.downgrades FROM '/home/ec2-user/downgrades.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.videos CASCADE;

CREATE TABLE captainu.videos
(
   athlete_id  integer,
   video_id    integer,
   created_at  timestamp,
   updated_at  timestamp
);

COMMIT;

\copy captainu.videos FROM '/home/ec2-user/Videos.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.users CASCADE;

CREATE TABLE captainu.users
(
   athlete_id  integer,
   user_id     integer,
   email       boolean,
   created_at  timestamp,
   updated_at  timestamp
);

COMMIT;

\copy captainu.users FROM '/home/ec2-user/Users.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.subscriptions CASCADE;

CREATE TABLE captainu.subscriptions
(
   athlete_id       integer,
   subscription_id  integer,
   plain_id         integer,
   created_at       timestamp,
   updated_at       timestamp,
   status           varchar(32),
   montly_price     numeric(10,2),
   under_promotion  boolean
);

COMMIT;

\copy captainu.subscriptions FROM '/home/ec2-user/subscriptions.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.relations CASCADE;

CREATE TABLE captainu.relations
(
   relation_id   integer,
   user_id       integer,
   athlete_id    integer,
   account_type  varchar(32),
   role          varchar(32),
   created_at    timestamp,
   updated_at    timestamp
);

COMMIT;

\copy captainu.relations FROM '/home/ec2-user/Relations.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.prospects CASCADE;

CREATE TABLE captainu.prospects
(
   athlete_id       integer,
   prospect_id      integer,
   college_team_id  integer,
   created_at       timestamp,
   updated_at       timestamp
);

COMMIT;

\copy captainu.prospects FROM '/home/ec2-user/prospects.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.plans CASCADE;

CREATE TABLE captainu.plans
(
   plan_id  integer,
   name     varchar(32),
   price    numeric(10,2)
);

COMMIT;

\copy captainu.plans FROM '/home/ec2-user/Plans.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.events CASCADE;

CREATE TABLE captainu.events
(
   event_id    integer,
   city        varchar(256),
   state       varchar(16),
   event_type  varchar(16),
   start_date  date,
   created_at  timestamp,
   updated_at  timestamp,
   sport_id    integer,
   sport       varchar(64),
   sponsored   boolean
);

COMMIT;

\copy captainu.events FROM '/home/ec2-user/events.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.athlete_events CASCADE;

CREATE TABLE captainu.athlete_events
(
   athlete_id  integer,
   event_id    integer,
   created_at  timestamp,
   updated_at  timestamp
);

COMMIT;

\copy captainu.athlete_events FROM '/home/ec2-user/athlete_events.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.messages CASCADE;

CREATE TABLE captainu.messages
(
   athlete_id   integer,
   message_id   integer,
   from_id      integer,
   from_type    varchar(16),
   to_id        integer,
   to_type      varchar(16),
   created_at   timestamp,
   updated_at   timestamp,
   reply_to_id  integer
);

COMMIT;

\copy captainu.messages FROM '/home/ec2-user/Messages.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';

DROP TABLE IF EXISTS captainu.emails CASCADE;

CREATE TABLE captainu.emails
(
   email_id        integer,
   email_type      varchar(32),
   athlete_id      integer,
   recipient_type  varchar(32),
   clicked_at      timestamp,
   created_at      timestamp,
   updated_at      timestamp
);

COMMIT;

\copy captainu.emails FROM '/home/ec2-user/Emails.csv' WITH DELIMITER ',' NULL 'NULL' CSV HEADER ESCAPE '\';


CREATE ROLE readonly LOGIN PASSWORD 're@d0n!y';
GRANT CONNECT ON DATABASE chunkdb TO readonly;
GRANT USAGE ON SCHEMA captainu TO readonly;
GRANT SELECT ON ALL TABLES IN SCHEMA captainu TO readonly;
COMMIT;
