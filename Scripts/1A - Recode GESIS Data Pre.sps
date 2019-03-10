cd 'Desktop/Bamberg 2019/Bayes_project/Data'.
GET 
  FILE='ZA6800_v5-0-0.sav'. 
DATASET NAME preElection WINDOW=FRONT.


*q12ba pre = 19ba post. (vote intent)
*q10 pre = 17 post (turnout
*q38ba pre = q25ba post. (past vote)
*q37 pre = q24 post. (can remember past vote.
*q36 pre = q23 post (eligible to vote in last election)


*Create standard codings between pre- and post-survey.
COMPUTE birthyear = q2a.
COMPUTE birthmonth = q2b.

*Drop ineligible to vote.

IF(birthyear > 1999) eligible_2017 = 2.
IF(birthyear = 1999 AND birthmonth > 9) eligible_2017 = 2.
IF(birthyear < 1999) eligible_2017 = 1.
IF(birthyear = 1999 AND birthmonth <=9) eligible_2017 = 1.
SELECT IF(eligible_2017 = 1).
VALUE LABELS eligible_2017 1 "Eligible to vote in 2017" 2 "Not eligible to vote in 2017".


*---PastVote and political questions---.
MISSING VALUES q10 q11ba q12ba ().
COMPUTE vote2_2017 = q11ba.
IF(q10 = 5 OR q10 = 4 OR q10 = -97) vote2_2017 = 99.
IF(q10 = 6) vote2_2017 = q12ba.
RECODE vote2_2017 (801 = 96) (322 = 8) (-83 = 99).
IF(MISSING(vote2_2017)) vote2_2017 = 97.

VALUE LABELS vote2_2017 1 "CDU/CSU" 4 "SPD" 5 "FDP" 6 "Green" 7 "Left" 8 "AfD" 96 "Other" 97 "Ref/NA" 99 "Abstain".

RECODE vote2_2017 (8 = 1) (1 THRU 7 = 0 ) (96 = 0) (97 = SYSMIS) (99 = SYSMIS)  INTO voteAfD_2017.
VALUE LABELS voteAfD_2017 1 "Yes" 0 "No".

IF(birthyear > 1995) eligible_2013 = 2.
IF(birthyear = 1995 AND birthmonth > 9) eligible_2013 = 2.
IF(birthyear < 1995) eligible_2013 = 1.
IF(birthyear = 1995 AND birthmonth <=9) eligible_2013 = 1.
VALUE LABELS eligible_2013 1 "Eligible to vote in 2013" 2 "Not eligible to vote in 2013".

MISSING VALUES q38ba().
COMPUTE vote2_2013 = q38ba.
IF(q37 = 2) vote2_2013 = 99.
IF(q36 = 2) vote2_2013 = 95.
IF(eligible_2013 = 2) vote2_2013 = 95.
RECODE vote2_2013 (801 = 96) (322 = 8) (-99 = 97) (-98 = 97) (-97 = 97) (-83 = 97).
VALUE LABELS vote2_2013 1 "CDU/CSU" 4 "SPD" 5 "FDP" 6 "Green" 7 "Left" 8 "AfD" 95 "Ineligible" 96 "Other" 97 "Ref/NA" 99 "Abstain".

RECODE vote2_2013 (8 = 1) (1 THRU 7 = 0 ) (96 = 0) (97 = SYSMIS) (99 = SYSMIS) INTO voteAfD_2013.
VALUE LABELS voteAfD_2013 1 "Yes" 2 "No".

*leave missing values for now - we can think about imputation later!.
COMPUTE Fav_AfD = q20g.
COMPUTE leftright_self = q54.

RECODE q55a q55b q55c q55d q55e q55f (1 = 1) (2 = 2) (3 = 3) (4 = 4) (5 = 5) (6 = 6) (7 = 7) INTO
q55a_recode q55b_recode q55c_recode q55d_recode q55e_recode q55f_recode.

save outfile 'Pre-election - Processed.sav'.

