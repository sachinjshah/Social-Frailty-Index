libname in "M:\SVI";
options nofmterr;

data d10; set in.Data_2010_to_sjs;
run;

data d12; set in.Cohort_2012_05072021_to_sjs;
run;

*** step 1: need to create a common variable name so I can stack the 2010 and 2012 data;
** step 1.1:
there are 3 types of variables
(1) HRS variables where we need to strip the first character
(2) RAND variables where we need to strip the first three characters
(3) custom varibles that need to be kept intact;

data d10_set1; set d10;
** drop the RAND variables and the variables that do not need to be parsed;
drop 
RAEDUC RAGENDER RAHISPAN RARACEM RASSRECV death_flag
H10ATOTB H10ATOTBC H10CHILD H10CPL H10HHRES R10CONDE R10COVR
R10DADLIV R10HIGOV R10HLTHLM R10JPHYS R10JSTRES R10LBRF
R10LIVSIB R10MOMLIV R10MSTAT R10PENINC R10RETSAT R10SAYRET R10WORK
R10WORK2;
run;

proc sql noprint;
	SELECT cats(NAME,"=",substr(NAME,2))
	INTO :renames SEPARATED BY " "
	FROM dictionary.columns
	WHERE LIBNAME="WORK" AND MEMNAME="D10_SET1";
quit;

data d10_set1;
set d10_set1;
rename &renames;
YR = 10;
run;

data d10_set1;
set d10_set1;
hhidpn=hidpn;
drop hidpn;
run;

data d10_set2; set d10;
** keep the RAND variables;
keep HHIDPN  
H10ATOTB H10ATOTBC H10CHILD H10CPL H10HHRES R10CONDE R10COVR
R10DADLIV R10HIGOV R10HLTHLM R10JPHYS R10JSTRES R10LBRF
R10LIVSIB R10MOMLIV R10MSTAT R10PENINC R10RETSAT R10SAYRET R10WORK
R10WORK2;
run; 

proc sql noprint;
	SELECT cats(NAME,"=",substr(NAME,4))
	INTO :renames SEPARATED BY " "
	FROM dictionary.columns
	WHERE LIBNAME="WORK" AND MEMNAME="D10_SET2";
quit;

data d10_set2;
set d10_set2;
rename &renames;
YR = 10;
run;

data d10_set2;
set d10_set2;
HHIDPN = DPN;
drop DPN;
run;

data d10_set3; set d10;
* keep the variables that do not need to be parsed;
keep HHIDPN  RAEDUC RAGENDER RAHISPAN RARACEM RASSRECV death_flag;
YR =10;
run;

proc sql;
create table d10_fin
as select * from d10_set3
left join d10_set2 on
d10_set3.hhidpn = d10_set2.hhidpn

left join d10_set1 on 
d10_set3.hhidpn = d10_set1.hhidpn;
quit;

proc print data = d10_fin (obs=10);
run;

proc datasets;
delete d10 d10_s:;
run;
quit;

** repeat the same for 2012; 

data d12_set1; set d12;
drop 
death_flag ADL_2012 ADL_2016 pred_death NH_14_16 NH_12 
RAEDUC RAGENDER RAHISPAN RARACEM RASSRECV
H11ATOTB H11ATOTBC H11CHILD H11CPL H11HHRES R11COVR 
R11DADLIV R11HIGOV R11HLTHLM R11JPHYS R11JSTRES R11LBRF 
R11LIVSIB R11MOMLIV R11MSTAT R11PENINC R11RETSAT R11SAYRET R11WORK 
R11WORK2;
run;

proc sql noprint;
	SELECT cats(NAME,"=",substr(NAME,2))
	INTO :renames SEPARATED BY " "
	FROM dictionary.columns
	WHERE LIBNAME="WORK" AND MEMNAME="D12_SET1";
quit;

data d12_set1;
set d12_set1;
rename &renames;
YR = 12;
run;

data d12_set1;
set d12_set1;
hhidpn=hidpn;
drop hidpn;
run;

data d12_set2; set d12;
keep HHIDPN  
H11ATOTB H11ATOTBC H11CHILD H11CPL H11HHRES R11COVR 
R11DADLIV R11HIGOV R11HLTHLM R11JPHYS R11JSTRES R11LBRF 
R11LIVSIB R11MOMLIV R11MSTAT R11PENINC R11RETSAT R11SAYRET R11WORK 
R11WORK2;
run; 

proc sql noprint;
	SELECT cats(NAME,"=",substr(NAME,4))
	INTO :renames SEPARATED BY " "
	FROM dictionary.columns
	WHERE LIBNAME="WORK" AND MEMNAME="D12_SET2";
quit;

data d12_set2;
set d12_set2;
rename &renames;
YR = 12;
run;

data d12_set2;
set d12_set2;
HHIDPN = DPN;
drop DPN;
run;

data d12_set3; set d12;
keep HHIDPN death_flag RAEDUC RAGENDER RAHISPAN RARACEM RASSRECV 
ADL_2012 ADL_2016 pred_death NH_14_16 NH_12;
YR =12;
run;

proc sql;
create table d12_fin
as select * from d12_set3
left join d12_set2 on
d12_set3.hhidpn = d12_set2.hhidpn

left join d12_set1 on 
d12_set3.hhidpn = d12_set1.hhidpn;
quit;

proc print data = d12_fin (obs=10);
run;

proc datasets;
delete d12 d12_s:;
run;

data d; 
set d10_fin d12_fin;
run;

proc freq;
tables LB026
		LB017C/ missing;
where yr =10 ;
run;

proc contents data = d12 order =varnum;
run;

** Part 2 **;

* narrow data set d to just the varibles we need;

data d_skinny; set d;
keep 
HHIDPN AGE RAGENDER RARACEM RAHISPAN LB021F LB026 LB037C LB039C WORK
RETSAT RAEDUC  LB004 LB007 LB015 LIVSIB LB020F MSTAT LB001B
LB001D LB001F LB009A LB017C LB020C LB020D LB020J LB030A LB030C LB032C
death_flag NH_12 NH_14_16 ADL_2012 ADL_2016 pred_death yr CONDE;
run;

** Part 3 **;
* merge in comorbidity data ;

data c; set in.analytic_long_2021_05_13;
where wave in (10,11);
** https://www.rand.org/content/dam/rand/www/external/labor/aging/dataprod/lining_up_years.pdf;
* wave 10 = 2010;
* wave 11 = 2012; 
if wave = 10 then yr = 10;
else if wave = 11 then yr = 12;

keep 
hhidpn ADLhelp ARTHRITIS BADHEARING BADVISION CANCER COG
DEPRESSION DIABETES HEARTD HSGRAD INC_ABV_MED LA PAIN SMOKING STROKE poorhealth yr
RAVETRN;
run;

proc sql;
create table e
as select * from d_skinny 
left join c
on d_skinny.hhidpn = c.hhidpn and d_skinny.yr = c.yr;
quit;

proc contents data = e;
run;

data e1; set e;
length race $5.;
length cog_status $24.; 
length edu $17.;
length married $21.;
length lonely $20.;

RAFEMALE = (RAGENDER = 2);
if RARACEM = 1 then race = "White"; else if RARACEM = 2 then race = "Black"; 
if COG = 1 then cog_status = "Intact"; else if COG = 2 then cog_status = "Impairment, not dementia"; else if COG = 3 then cog_status = "Dementia";

local_area_litter = (LB021F ge 4); if LB021F = . then local_area_litter = .;
low_fin_control = (LB026 le 3); if LB026 = . then low_fin_control = .;
vet = (RAVETRN = 1); if RAVETRN in (.D, .M, .R) then vet = .;
combat = (LB037C = 1); if LB037C = . then combat = . ;
not_sat_retirement = (RETSAT = 3); if RETSAT = . then not_sat_retirement = .;
if RAEDUC = 1 then EDU = "Less than HS";
	else if RAEDUC in (2,3) then EDU = "HS or GED";
	else if RAEDUC = 4 then EDU = "Some college";
	else if RAEDUC =5 then EDU = "College and above";
if MSTAT in (1, 2, 3) then married = "Married or partnered";
	else if MSTAT in (4, 5, 6) then married = "Separated or divorced";
	else if MSTAT = 7 then married = "Widowed";
	else if MSTAT = 8 then married = "Never married";
children = (LB007 = 1); if LB007 = . then children = .;
friends = (LB015 = 1); if LB015 = . then friends = .;
talk_with_someone = (LB020F = 1); if LB020F = . then talk_with_someone = .;
never_charity_work = (LB001D = 7); if LB001D = . then never_charity_work = . ;
children_lt_yearly = (LB009A = 6); if LB009A = . then children_lt_yearly = .;
write_friends = (LB017C in (4, 5, 6)); if LB017C = . then write_friends = .;
if LB020C = 1 then lonely = "Often";
	else if LB020C = 2 then lonely = "Some of the time";
	else if LB020C = 3 then lonely = "Hardly ever or never";
z_in_tune = (LB020D = 1); if LB020D = . then z_in_tune = .;
never_less_respect = (LB030A in (5, 6)); if LB030A = . then never_less_respect = .;
never_not_smart = (LB030C in (5, 6)); if LB030A = . then never_not_smart = .;
no_major_activity = (LB032C = 6); if LB032C = . then no_major_activity = .;

drop RAGENDER RARACEM COG LB021F LB026 RAVETRN LB037C RETSAT RAEDUC MSTAT LB007 LB015 LB020F LB001D LB009A LB017C
LB020C LB020D LB030A LB030C LB032C;
;
run;

proc contents data = e1;
run;
data in.table1; set work.e1;
run;

