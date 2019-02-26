cd 'Desktop/Bamberg 2019/Bayes_project/Data'.
GET 
  FILE='ZA6801_v4-0-0.sav'. 
DATASET NAME postElection WINDOW=FRONT.


*Create standard codings between pre- and post-survey.
COMPUTE birthyear = q2c.
COMPUTE birthmonth = q2b.

*Drop ineligible to vote.

IF(birthyear > 1999) eligible_2017 = 2.
IF(birthyear = 1999 AND birthmonth > 9) eligible_2017 = 2.
IF(birthyear < 1999) eligible_2017 = 1.
IF(birthyear = 1999 AND birthmonth <=9) eligible_2017 = 1.
SELECT IF(eligible_2017 = 1).
VALUE LABELS eligible_2017 1 "Eligible to vote in 2017" 2 "Not eligible to vote in 2017".


*Drop ineligible to vote.

IF(q2c > 1999) eligible_2017 = 2.
IF(q2c = 1999 AND q2b > 9) eligible_2017 = 2.
IF(q2c < 1999) eligible_2017 = 1.
IF(q2c = 1999 AND q2b <=9) eligible_2017 = 1.
SELECT IF(eligible_2017 = 1).
VALUE LABELS eligible_2017 1 "Eligible to vote in 2017" 2 "Not eligible to vote in 2017".


*---PastVote and political questions---.

COMPUTE vote2_2017 = q19ba.
IF(q17 = 2 OR MISSING(q17)) vote2_2017 = 99.
IF(MISSING(vote2_2017)) vote2_2017 = 97.
RECODE vote2_2017 (801 = 96) (322 = 8).
VALUE LABELS vote2_2017 1 "CDU/CSU" 4 "SPD" 5 "FDP" 6 "Green" 7 "Left" 8 "AfD" 96 "Other" 97 "Ref/NA" 99 "Abstain".

RECODE vote2_2017 (8 = 1) (1 THRU 7 = 0 ) (96 = 0) (97 = SYSMIS) (99 = SYSMIS)  INTO voteAfD_2017.
VALUE LABELS voteAfD_2017 1 "Yes" 2 "No".


IF(q2c > 1995) eligible_2013 = 2.
IF(q2c = 1995 AND q2b > 9) eligible_2013 = 2.
IF(q2c < 1995) eligible_2013 = 1.
IF(q2c = 1995 AND q2b <=9) eligible_2013 = 1.
VALUE LABELS eligible_2013 1 "Eligible to vote in 2013" 2 "Not eligible to vote in 2013".

MISSING VALUES q25ba().
COMPUTE vote2_2013 = q25ba.
IF(q24 = 2) vote2_2013 = 99.
IF(q23 = 2) vote2_2013 = 95.
IF(eligible_2013 = 2) vote2_2013 = 95.
RECODE vote2_2013 (801 = 96) (322 = 8) (-99 = 97) (-98 = 97) (-97 = 97) (-83 = 97).
VALUE LABELS vote2_2013 1 "CDU/CSU" 4 "SPD" 5 "FDP" 6 "Green" 7 "Left" 8 "AfD" 95 "Ineligible" 96 "Other" 97 "Ref/NA" 99 "Abstain".

RECODE vote2_2013 (8 = 1) (1 THRU 7 = 0 ) (96 = 0) (97 = SYSMIS) (99 = SYSMIS) INTO voteAfD_2013.
VALUE LABELS voteAfD_2013 1 "Yes" 2 "No".

RECODE q135 (1 = 1) (2 = 1) (3 = 2) (4 = 3) (5 = 4) (6 = 2) (9 = 4) INTO educ_recode.
VALUE LABELS educ_recode 1 "Hauptschule/Low technical education" 2 "Realschule/Medium technical education" 3 "Fachhochschule/Applied post-secondary" 4 "Abeitur/University".

COMPUTE pol_interest = Q60.
IF(MISSING(pol_interest)) pol_interest = 3.
VALUE LABELS pol_interest 1 "Very strong" 2 "Somewhat strong" 3 "Medium" 4 "Not very strong" 5 "Not at all".

COMPUTE pol_media = q7.
IF(MISSING(pol_media)) pol_media = 3.
VALUE LABELS pol_media 1 "Very frequently" 2 "Fairly frequently" 3 "Not very frequently" 4 "Never".

COMPUTE  vote_efficacy = q27.
IF(MISSING(vote_efficacy)) vote_efficacy = 3.

COMPUTE dem_satisfaction = q33.
IF(MISSING(dem_satisfaction)) dem_satisfaction = 3.
VALUE LABELS dem_satisfaction 1 "Totally satisfied" 2 "Fairly satisfied" 3 "Not very satisfied" 4 "Not at all satisfied".

*---ISCO codes---.

RECODE q140_i08 (110 =1) (210 = 2) (310 = 3) (1000 THRU 2999 = 1) (3000 THRU 4999 = 2) (5000 THRU 9999 = 3) into isco_self_current.
RECODE q149_i08 (110 =1) (210 = 2) (310 = 3) (1000 THRU 2999 = 1) (3000 THRU 4999 = 2) (5000 THRU 9999 = 3) into isco_self_former.
RECODE q157_i08 (110 =1) (210 = 2) (310 = 3) (1000 THRU 2999 = 1) (3000 THRU 4999 = 2) (5000 THRU 9999 = 3) into isco_partner_current.
RECODE q162_i08 (110 =1) (210 = 2) (310 = 3) (1000 THRU 2999 = 1) (3000 THRU 4999 = 2) (5000 THRU 9999 = 3) into isco_partner_former.
VALUE LABELS isco_self_current isco_self_former isco_partner_current isco_partner_former 1 "Manger/Professional" 2 "Clerical/Associate Professional" 3 "Manufacturing and Personal Services".
EXECUTE.

RECODE isco_self_current isco_self_former isco_partner_current isco_partner_former (MISSING = -1).

COMPUTE isco_self = isco_self_current.
IF(isco_self = -1) isco_self = isco_self_former.
COMPUTE isco_partner = isco_partner_current.
IF(isco_partner = -1) isco_partner = isco_partner_former.

COMPUTE isco_hh = isco_self.
IF(isco_partner >0 AND (isco_partner < isco_self OR isco_self = -1)) isco_hh = isco_partner.
IF MISSING(isco_hh) isco_hh = -1.

VALUE LABELS isco_hh 1 "Manger/Professional" 2 "Clerical/Associate Professional" 3 "Manufacturing and Personal Services" -1 "Unknown".
EXECUTE.


*---ISCO codes---.

COMPUTE sector_now = q143.
IF MISSING(sector_now) sector_now = q159.
IF MISSING(sector_now) sector_now = q151.
IF MISSING(sector_now) sector_now = q164.
IF MISSING(sector_now) sector_now = -1.

RECODE sector_now (1 = 2) (2 = 2) (3 = 2) (4 = 1) (-1 = 3) into sector_simple.
VALUE LABELS sector_simple 1 "Private" 2 "Public/Third" 3 "NA".

*---other---.

RECODE q167 (1 = 1) (2= 1) (3 =2) (4 =3) (5 = 4) (6 = 4) INTO subj_class.
VALUE LABELS subj_class 1 "Lower/Working" 2 "Lower Middle" 3 "Middle" 4 "Upper Middle/Upper" - 1 "NA".
IF MISSING(subj_class) subj_class = -1.

COMPUTE religiosity = q170.
IF MISSING(religiosity) religiosity = -1.
VALUE LABELS religiosity 1 "Not at all" 2 "Not very" 3 "Somewhat" 4 "Very "  -1 "NA".

RECODE q174 (2 = 1) (16 = 1) (3 = 2) (4 = 2) (5 =2) (6 = 2) (7 = 2) (9 = 2) (8 = 3) (10 = 3) (11 = 3) (12 = 3) (13 = 3) (14 = 3) (15 = 3) (1 = 4) (MISSING = 4) INTO immigrant_self.
RECODE q179 (2 = 1) (16 = 1) (3 = 2) (4 = 2) (5 =2) (6 = 2) (7 = 2) (9 = 2) (8 = 3) (10 = 3) (11 = 3) (12 = 3) (13 = 3) (14 = 3) (15 = 3) (1 = 4) (MISSING = 4) INTO immigrant_father.
RECODE q180 (2 = 1) (16 = 1) (3 = 2) (4 = 2) (5 =2) (6 = 2) (7 = 2) (9 = 2) (8 = 3) (10 = 3) (11 = 3) (12 = 3) (13 = 3) (14 = 3) (15 = 3) (1 = 4) (MISSING = 4) INTO immigrant_mother.
VALUE LABELS immigrant_self immigrant_mother immigrant_father 1 "Other (inc Turkey)" 2 "Southern/Eastern Europe (exc Turkey)" 3 "Western Europe + USA" 4 "Not An Immigrant".

IF((immigrant_self = 1 OR immigrant_self = 2) OR (immigrant_father = 1 OR immigrant_father = 2) OR (immigrant_mother = 1 OR immigrant_mother = 2)) immigrant_family = 1.
IF(MISSING(immigrant_family) AND (immigrant_self = 3 OR immigrant_father = 3 OR immigrant_mother = 3)) immigrant_family = 1.
IF MISSING(immigrant_family) immigrant_family = 2.
VALUE LABELS immigrant_family 1 "Immigrant background" 2 "No immigrant background".
* We don't have enough western europe + US immigrants to classify them separately.
*VALUE LABELS immigrant_fanily 1 "Immigrant background (outside Western Europe + US)" 2 "Immigrant background (Western Europe + US only)" 3 "No immigrant background".

COMPUTE union = q191.
IF(MISSING(union) AND q190a <= 3) union = 1.
IF(missing(union) AND q190a = 4) union = 2.
VALUE LABELS union 1 "HH union member" 2 "HH not union member".

MISSING VALUES q192 ().
RECODE q192 (1 = 1) (2 = 1) (3 = 1) (4 = 1) (5 = 1) (6 = 2) (7 = 3) (8 = 4) (9 = 5) (10 =6) (11 = 7) (12 = 8) (13 = 8) (-99 = -99) (-98 = -98) INTO income.
VALUE LABELS income 1 "<1.5k/mo" 2 "1.5-2k/mo" 3 "2-2.5k/mo" 4 "2.5-3k/mo" 5 "3-4k/mo" 6 "4-5k/mo" 7 "5-7.5k/mo" 8 "7.5k+/mo" -99 "Ref" -98 "DK".

COMPUTE  gender = q1.
VALUE LABELS gender 1 "Male" 2 "Female".

RECODE q168 (1 = 2) (2 = 3) (3 = 1) (4 = 4) (5 = 4) (6 =4) (7 = 4) (MISSING = 4) INTO denomination.
VALUE LABELS denomination 1 "Roman Catholic" 2 "Protestant - Not Freikirche" 3 "Protestant - Freikirche"  4 "Other/None".
MISSING VALUES denomination (4).

IF(q2c < 1999 AND q2c > 1987) agecat = 1.
IF(q2c = 1999 AND q2b <= 9) agecat = 1.
IF(q2c = 1987 AND q2b > 9) agecat = 1.

IF(q2c <1987 AND q2c > 1977) agecat = 2.
IF(q2c = 1987 AND q2b <= 9) agecat = 2.
IF(q2c = 1977 AND q2b > 9) agecat = 2.

IF(q2c <1977 AND q2c > 1967) agecat = 3.
IF(q2c = 1977 AND q2b <= 9) agecat = 3.
IF(q2c = 1967 AND q2b > 9) agecat = 3.

IF(q2c <1967 AND q2c > 1952) agecat = 4.
IF(q2c = 1967 AND q2b <= 9) agecat = 4.
IF(q2c = 1952 AND q2b > 9) agecat = 4.

IF(q2c <1952 AND q2c > 1942) agecat = 5.
IF(q2c = 1952 AND q2b <= 9) agecat = 5.
IF(q2c = 1942 AND q2b > 9 ) agecat = 5.

IF(q2c <1942) agecat = 6.
IF(q2c = 1942 AND q2b <= 9) agecat = 6.

VALUE LABELS agecat 1 "18-29" 2 "30-39" 3 "40-49" 4 "50-64" 5 "65-74" 6 "75+".

COMPUTE Fav_AfD = q28g.
COMPUTE leftright_self = q32.


save outfile 'Post-election - Processed.sav'.

