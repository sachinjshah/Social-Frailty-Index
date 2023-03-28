/* ########################################################################

Goal: Create HRS analytic file

Program created by Rae Wannier 11/15/2018

Update log:
- 12/20/18 last update

V2 created 4/10/19
- 4/10/19 Sachin review to comment code
- 4/11/19 Sachin re-code edit interviews to include those who were skipped since skip
are often NOT disabled.

update on 2/13/20
- clean up code

V5 
add in 2016 data


2-12-2021
updated file with exclusion criteria specific of SVI project
changed the out directory to match SVI project

###########################################################################*/

OPTIONS nofmterr;

%LET path = C:\Users\sachi\Box Sync\HRS;
libname in "&path\DATA";
libname track "&path\temp\trk2016";
libname inrand "&path\randpsas\randhrs1992_2016v2_SAS";
libname lw "C:\Users\sachi\Box Sync\data\HRS\LangaWeir2016";
libname out "C:\Users\sachi\Box Sync\data\SVI";

options nonotes;

/*
Importing Rand data with variables of interest
*/

data _temp; set inrand.randhrs1992_2016v2;
keep
HHIDPN HHID PN RAHHIDPN
H5HHRES H6HHRES H7HHRES  H8HHRES  H9HHRES  H10HHRES  H11HHRES  H12HHRES H13HHRES
INW5-INW13
R5ADLA R6ADLA R7ADLA R8ADLA R9ADLA R10ADLA R11ADLA R12ADLA R13ADLA
R5WALKRH R5DRESSH R5BATHH R5EATH R5TOILTH R5BEDH
R6WALKRH R6DRESSH R6BATHH R6EATH R6TOILTH R6BEDH
R7WALKRH R7DRESSH R7BATHH R7EATH R7TOILTH R7BEDH
R8WALKRH R8DRESSH R8BATHH R8EATH R8TOILTH R8BEDH
R9WALKRH R9DRESSH R9BATHH R9EATH R9TOILTH R9BEDH
R10WALKRH R10DRESSH R10BATHH R10EATH R10TOILTH R10BEDH
R11WALKRH R11DRESSH R11BATHH R11EATH R11TOILTH R11BEDH
R12WALKRH R12DRESSH R12BATHH R12EATH R12TOILTH R12BEDH
R13WALKRH R13DRESSH R13BATHH R13EATH R13TOILTH R13BEDH
R4IWSTAT R5IWSTAT R6IWSTAT R7IWSTAT R8IWSTAT R9IWSTAT R10IWSTAT R11IWSTAT R12IWSTAT R13IWSTAT
R7HOSP R8HOSP R9HOSP R10HOSP R11HOSP R12HOSP R13HOSP
HACOHORT
RAGENDER RARACEM RAHISPAN RAEDEGRM RAVETRN
R6MSTAT R7MSTAT R8MSTAT R9MSTAT R10MSTAT R11MSTAT R12MSTAT R13MSTAT
H6ATOTN H7ATOTN H8ATOTN H9ATOTN H10ATOTN H11ATOTN H12ATOTN H13ATOTN
H6INPOVA H7INPOVA H8INPOVA H9INPOVA H10INPOVA H11INPOVA H12INPOVA H13INPOVA
R6SHLT R7SHLT R8SHLT R9SHLT R10SHLT R11SHLT R12SHLT R13SHLT
R6HIBPE R7HIBPE R8HIBPE R9HIBPE R10HIBPE R11HIBPE R12HIBPE R13HIBPE
R6DIABE R7DIABE R8DIABE R9DIABE R10DIABE R11DIABE R12DIABE R13DIABE
R6CANCRE R7CANCRE R8CANCRE R9CANCRE R10CANCRE R11CANCRE R12CANCRE R13CANCRE
R6LUNGE R7LUNGE R8LUNGE R9LUNGE R10LUNGE R11LUNGE R12LUNGE R13LUNGE
R6HEARTE R7HEARTE R8HEARTE R9HEARTE R10HEARTE R11HEARTE R12HEARTE R13HEARTE
R6ARTHRE R7ARTHRE R8ARTHRE R9ARTHRE R10ARTHRE R11ARTHRE R12ARTHRE R13ARTHRE
R6CESD R7CESD R8CESD R9CESD R10CESD R11CESD R12CESD R13CESD
R6STROKE R7STROKE R8STROKE R9STROKE R10STROKE R11STROKE R12STROKE R13STROKE
R6SMOKEV R7SMOKEV R8SMOKEV R9SMOKEV R10SMOKEV R11SMOKEV R12SMOKEV R13STROKE
R6DY R7DY R8DY R9DY R10DY R11DY R12DY R13DY
R6MO R7MO R8MO R9MO R10MO R11MO R12MO R13MO
R6YR R7YR R8YR R9YR R10YR R11YR R12YR R13MO
R6DW R7DW R8DW R9DW R10DW R11DW R12DW R13DW
R6BWC20 R7BWC20 R8BWC20 R9BWC20 R10BWC20 R11BWC20 R12BWC20 R13BWC20
R6BWC86 R7BWC86 R8BWC86 R9BWC86 R10BWC86 R11BWC86 R12BWC86 R13BWC86
R6SER7 R7SER7 R8SER7 R9SER7 R10SER7 R11SER7 R12SER7 R13SER7
R6DLRC R7DLRC R8DLRC R9DLRC R10DLRC R11DLRC R12DLRC R13DLRC
R7COGTOT R8COGTOT R9COGTOT R10COGTOT R11COGTOT R12COGTOT R13COGTOT
R5WALKR R5DRESS R5BATH R5EAT R5TOILT R5BED
R6WALKR R6DRESS R6BATH R6EAT R6TOILT R6BED
R7WALKR R7DRESS R7BATH R7EAT R7TOILT R7BED
R8WALKR R8DRESS R8BATH R8EAT R8TOILT R8BED
R9WALKR R9DRESS R9BATH R9EAT R9TOILT R9BED
R10WALKR R10DRESS R10BATH R10EAT R10TOILT R10BED
R11WALKR R11DRESS R11BATH R11EAT R11TOILT R11BED
R12WALKR R12DRESS R12BATH R12EAT R12TOILT R12BED
R13WALKR R13DRESS R13BATH R13EAT R13TOILT R13BED
R6HSPNIT R7HSPNIT R8HSPNIT R9HSPNIT R10HSPNIT R11HSPNIT R12HSPNIT R13HSPNIT
R6WTRESP R7WTRESP R8WTRESP R9WTRESP R10WTRESP R11WTRESP R12WTRESP R13WTRESP
R6PROXY R7PROXY R8PROXY R9PROXY R10PROXY R11PROXY R12PROXY R13PROXY
;
run;

/*cleaning and categorizing confounding RAND variables*/
PROC FORMAT;
  VALUE  racef 1="white/caucasian"
                2="black/african american"
                3="other";
run;

OPTIONS notes;

data _tempclean; set _temp;

** recode categorical variables;

*1= males, 2= female;
if RAGENDER=2 then RAFEMALE = 1;
if RAGENDER=1 then RAFEMALE=0;
*1= white, 2= black, 3= other;
if RARACEM=1 then RACE=1;
if RARACEM=2 then RACE=2;
if RARACEM=3 then RACE=3;
drop RARACEM;
rename RACE=RARACEM;
format RARACEM racef.;
if RAHISPAN=0 then hisp=0;
if RAHISPAN=1 then hisp=1;
drop RAHISPAN;
rename hisp=RAHISPAN;
if RAEDEGRM =0 then HSGRAD=0;
if RAEDEGRM >0 then HSGRAD=1;
if H7ATOTN>=52100.00 then INC_ABV_MED7 = 1;
    if -2245510.00<H7ATOTN<52100.00 then INC_ABV_MED7 = 0;
if H8ATOTN>=55425.00 then INC_ABV_MED8 = 1;
    if -2245500.00<H8ATOTN<55425.0 then INC_ABV_MED8 = 0;
if H9ATOTN>=59000.00 then INC_ABV_MED9 = 1;
    if -2245500.00<H9ATOTN<59000.00 then INC_ABV_MED9 = 0;
if H10ATOTN>=31000.00 then INC_ABV_MED10 = 1;
    if -2245500.00<H10ATOTN<31000.00 then INC_ABV_MED10 = 0;
if H11ATOTN>=31000.00 then INC_ABV_MED11 = 1;
    if -2245500.00<H11ATOTN<31000.00 then INC_ABV_MED11 = 0;
if H12ATOTN>=31000.00 then INC_ABV_MED12 = 1;
    if -2245500.00<H12ATOTN<31000.00 then INC_ABV_MED12 = 0;
if H13ATOTN>=31000.00 then INC_ABV_MED13 = 1;
    if -2245500.00<H13ATOTN<31000.00 then INC_ABV_MED13 = 0;

%macro catey(wave);
*collapse 1-3 (marries/married,spouse abset/partners) vs. 4-8 (separated, divorced, widowed, never married);
if R&wave.MSTAT>0 then married&wave.=1;
if R&wave.MSTAT>3 then married&wave.=0;
*collapse 1-3 (excellent, very good, good) vs. 4-5 (fair, poor) health;
if R&wave.SHLT>0 then poorhealth&wave.=0;
if R&wave.SHLT>3 then poorhealth&wave.=1;
*create clean High Blood Pressure variable 1=Yes, 0 = No, D=Dont know, M=Missing, R=Refused;
if R&wave.HIBPE=1 then HIGHBP&wave.=1;
if R&wave.HIBPE=0 then HIGHBP&wave.=0;
*create clean diabetes variable 1=Yes, 0 = No, D=Dont know, M=Missing, R=Refused;
if R&wave.DIABE=1 then DIABETES&wave.=1;
if R&wave.DIABE=0 then DIABETES&wave.=0;
*create clean cancer history variable 1=Yes, 0 = No, D=Dont know, M=Missing, R=Refused;
if R&wave.CANCRE=1 then CANCER&wave.=1;
if R&wave.CANCRE=0 then CANCER&wave.=0;
*create clean lung disease variable 1=Yes, 0 = No, D=Dont know, M=Missing, R=Refused;
if R&wave.LUNGE=1 then LUNGD&wave.=1;
if R&wave.LUNGE=0 then LUNGD&wave.=0;
%mend;
%catey(7); %catey(8); %catey(9); %catey(10); %catey(11); %catey(12); %catey(13);

%macro cateytwo(wave);
*create clean arthritis variable 1=Yes, 0 = No, D=Dont know, M=Missing, R=Refused;
if R&wave.ARTHRE=1 then ARTHRITIS&wave.=1;
if R&wave.ARTHRE=0 then ARTHRITIS&wave.=0;
*create clean heart disease variable 1=Yes, 0 = No, D=Dont know, M=Missing, R=Refused;
if R&wave.HEARTE=1 then HEARTD&wave.=1;
if R&wave.HEARTE=0 then HEARTD&wave.=0;
*create clean stroke variable 1=Yes, 0 = No, D=Dont know, M=Missing, R=Refused;
if R&wave.STROKE=1 then STROKE&wave.=1;
if R&wave.STROKE=0 then STROKE&wave.=0;
*create dichotomized CESD variable >=4 is CESD depressed, and <4 is not depressed;
if R&wave.CESD>=4 then DEPRESSION&wave.=1;
if 0<=R&wave.CESD<4 then DEPRESSION&wave.=0;
*create clean Smoking variable 1=Yes, 0 = No, D=Dont know, M=Missing, R=Refused;
if R&wave.SMOKEV=1 then SMOKING&wave.=1;
if R&wave.SMOKEV=0 then SMOKING&wave.=0;
*create dichotomized Walking variable 0 = no difficulty, 1= >0 (some, lots, don't/can't);
if R&wave.WALKR>0 then WALKDIFF&wave.=1;
if R&wave.WALKR=0 then WALKDIFF&wave.=0;
*create dichotomized dressing variable 0 = no difficulty, 1= >0 (some, lots, don't/can't);
if R&wave.DRESS>0 then DRESSDIFF&wave.=1;
if R&wave.DRESS=0 then DRESSDIFF&wave.=0;
*create dichotomized bathing variable 0 = no difficulty, 1= >0 (some, lots, don't/can't);
if R&wave.BATH>0 then BATHDIFF&wave.=1;
if R&wave.BATH=0 then BATHDIFF&wave.=0;
*create dichotomized eating variable 0 = no difficulty, 1= >0 (some, lots, don't/can't);
if R&wave.EAT>0 then EATDIFF&wave.=1;
if R&wave.EAT=0 then EATDIFF&wave.=0;
*create dichotomized toilet variable 0 = no difficulty, 1= >0 (some, lots, don't/can't);
if R&wave.TOILT>0 then TOILTDIFF&wave.=1;
if R&wave.TOILT=0 then TOILTDIFF&wave.=0;
*added by sachin 4/11/19: create dichotomized bed variable 0 = no difficulty, 1= >0 (some, lots, don't/can't);
if R&wave.BED>0 then BEDDIFF&wave.=1;
if R&wave.BED=0 then BEDDIFF&wave.=0;
%mend;
%cateytwo(7); %cateytwo(8); %cateytwo(9); %cateytwo(10); %cateytwo(11); %cateytwo(12); %cateytwo(13);

run;

/*
importing tracker file with variables of interest
*/
data tracker; set in.trk2016tr_r;
PWHY0RWT=PWHY0RWTE; **updated variable name to keep concordant syntax;
keep
HHID PN
JWHY0RWT KWHY0RWT LWHY0RWT MWHY0RWT NWHY0RWT OWHY0RWT PWHY0RWT
JRESCODE KRESCODE LRESCODE MRESCODE NRESCODE ORESCODE pRESCODE
JAGE KAGE LAGE MAGE NAGE OAGE PAGE
JPROXY KPROXY LPROXY MPROXY NPROXY OPROXY PPROXY
JNURSHM KNURSHM LNURSHM MNURSHM NNURSHM ONURSHM PNURSHM;
run;


/*merging files with wave 7 - 2004*/
proc sql;
create table tracker2 as
select * from _tempclean right join tracker
on _tempclean.HHID = tracker.HHID and _tempclean.PN = tracker.PN;
quit;

data wave7; set in.H04G_R;
run;

data wave72; set wave7;
keep JG097 HHID PN JG015 JG020 JG022 JG024 JG029 JG031;
run;

proc sql;
create table _temp2 as
select * from tracker2 left join wave72
on tracker2.HHID = wave72.HHID and tracker2.PN = wave72.PN;
quit;

/*
creating the living alone variables three levels: Yes, No, missing
*/

data _temp2; set _temp2;
W6LA = "Yes";
if H6HHRES ne 1 then W6LA = "No";
if H6HHRES = . then W6LA = "";
W7LA = "Yes";
if H7HHRES ne 1 then W7LA = "No";
if H7HHRES = . then W7LA = "";
W8LA = "Yes";
if H8HHRES ne 1 then W8LA = "No";
if H8HHRES = . then W8LA = "";
W9LA = "Yes";
if H9HHRES ne 1 then W9LA = "No";
if H9HHRES = . then W9LA = "";
W10LA = "Yes";
if H10HHRES ne 1 then W10LA = "No";
if H10HHRES = . then W10LA = "";
W11LA = "Yes";
if H11HHRES ne 1 then W11LA = "No";
if H11HHRES = . then W11LA = "";
W12LA = "Yes";
if H12HHRES ne 1 then W12LA = "No";
if H12HHRES = . then W12LA = "";
W13LA = "Yes";
if H13HHRES ne 1 then W13LA = "No";
if H13HHRES = . then W13LA = "";
options fmtsearch=(in.sasfmts);
run;

** data check;
/*
proc freq;
tables W7LA * JG097 / nopercent missing;
where INW7 = 1;
run;
*/

/*importing wave 8 - 2006 data and merging to rand dataset*/
data wave82; set in.H06G_R;
** these are help questions that I think are already in RAND file;
keep KG097 HHID PN KG015 KG020 KG022 KG024 KG029 KG031;
run;

proc sql;
create table _temp3 as
select * from _temp2 left join wave82
on _temp2.HHID = wave82.HHID and _temp2.PN = wave82.PN;
quit;

/*importing wave 9 - 2008 data and merging to Rand dataset*/
data wave9; set in.H08G_R;

keep LG097 HHID PN LG015 LG020 LG022 LG024 LG029 LG031;
run;

proc sql;
create table _temp4 as
select * from _temp3 left join wave9
on _temp3.HHID = wave9.HHID and _temp3.PN = wave9.PN;
quit;

/*importing wave 10 - 2010 data and merging to Rand dataset*/
data wave10; set in.H10G_R;
keep MG097 HHID PN MG015 MG020 MG022 MG024 MG029 MG031;
run;

proc sql;
create table _temp5 as
select * from _temp4 left join wave10
on _temp4.HHID = wave10.HHID and _temp4.PN = wave10.PN;
quit;

/*importing wave 11 - 2012 data and merging to Rand dataset*/
data wave11; set in.H12G_R;
keep NG097 HHID PN NG015 NG020 NG022 NG024 NG029 NG031;
run;

proc sql;
create table _temp6 as
select * from _temp5 left join wave11
on _temp5.HHID = wave11.HHID and _temp5.PN = wave11.PN;
quit;

/*importing wave 12 - 2014 data and merging to Rand dataset*/
data wave12; set in.H14G_R;
keep OG097 HHID PN OG015 OG020 OG022 OG024 OG029 OG031;
run;

proc sql;
create table _temp61 as
select * from _temp6 left join wave12
on _temp6.HHID = wave12.HHID and _temp6.PN = wave12.PN;
quit;

/*importing wave 13 - 2016 data and merging to Rand dataset*/
data wave13; set in.H16G_R;
keep PG097 HHID PN PG015 PG020 PG022 PG024 PG029 PG031;
run;

proc sql;
create table _temp7 as /*has to be named _temp7*/
select * from _temp61 left join wave13
on _temp61.HHID = wave13.HHID and _temp61.PN = wave13.PN;
quit;


/*--------------------------
import exit interview data
----------------------------*/

%macro inputexit(wave, year, temp0, temp1, temp2, temp3, temp4, temp5, wavecore);

** EXIT function file G;
data wave&wave.; set in.x&year.G_R;
keep HHID PN &wave.G015 &wave.G020 &wave.G022 &wave.G024 &wave.G029 &wave.G031 &wave.G129
&wave.G001 &wave.G002 &wave.G003 &wave.G004 &wave.G005 &wave.G006 &wave.G007 &wave.G008
&wave.G009 &wave.G010 &wave.G011 &wave.G012;

    * below added by Sachin 4/11/19 to recode missing to note that these
        are most likely skips not missing;
if &wave.G015 = . then &wave.G015 = .i;
if &wave.G020 = . then &wave.G020 = .i;
if &wave.G022 = . then &wave.G022 = .i;
if &wave.G024 = . then &wave.G024 = .i;
if &wave.G029 = . then &wave.G029 = .i;
if &wave.G031 = . then &wave.G031 = .i;
run;

proc sql;
create table _&temp1. as
select * from _&temp0. left join wave&wave.
on _&temp0..HHID = wave&wave..HHID and _&temp0..PN = wave&wave..PN;
quit;

** EXIT health services and insurance file N;
data wave&wave.; set in.x&year.N_R;
keep HHID PN &wave.N099 &wave.N100 &wave.N302 &wave.N101 &wave.N114 &wave.N116 &wave.N117;
run;

proc sql;
create table _&temp2. as
select * from _&temp1. left join wave&wave.
on _&temp1..HHID = wave&wave..HHID and _&temp1..PN = wave&wave..PN;
quit;

** EXIT cover and screening file A;
data wave&wave.; set in.x&year.A_R;
keep HHID PN &wave.A124 &wave.A028 &wave.A167;
run;

proc sql;
create table _&temp3. as
select * from _&temp2. left join wave&wave.
on _&temp2..HHID = wave&wave..HHID and _&temp2..PN = wave&wave..PN;
quit;

** CORE function file N;
data wave&wavecore.; set in.h&year.n_r;
keep HHID PN &wavecore.N114 &wavecore.N116 &wavecore.N117 &wavecore.N099 &wavecore.N302 &wavecore.N101 &wavecore.N100;
run;

proc sql;
create table _&temp4. as
select * from _&temp3. left join wave&wavecore.
on _&temp3..HHID = wave&wavecore..HHID and _&temp3..PN = wave&wavecore..PN;

** CORE health condition file;
data wave&wavecore.; set in.h&year.c_r;
keep HHID PN &wavecore.C104 &wavecore.C105  &wavecore.C095 &wavecore.C103;
run;

proc sql;
create table _&temp5. as
select * from _&temp4. left join wave&wavecore.
on _&temp4..HHID = wave&wavecore..HHID and _&temp4..PN = wave&wavecore..PN;
quit;

%mend;

%inputexit(T, 04, temp7, temp8, temp9, temp10, temp11, temp12, J);
%inputexit(U, 06, temp12, temp13, temp14, temp15, temp16, temp17, K);
%inputexit(V, 08, temp17, temp18, temp19, temp20, temp21, temp22, L);
%inputexit(W, 10, temp22, temp23, temp24, temp25, temp26, temp27, M);
%inputexit(X, 12, temp27, temp28, temp29, temp30, temp31, temp32, N);
%inputexit(Y, 14, temp32, temp33, temp34, temp35, temp36, temp37, O);
%inputexit(Z, 16, temp37, temp38, temp39, temp40, temp41, temp42, P);

** import EXIT health condition file = C;

%macro exitc(   X, /*exit interview letter */
                year, /*two digit year*/
                N /*corresponding RAND letter*/
            );

data _xc&n.; set in.x&year.C_R;
keep HHID PN EXIT_: ;

EXIT_CANCER&N. = &X.C018;
EXIT_HEARTD&N. = &X.C036;
EXIT_STROKE&N. = &X.C053;
run;
%mend;

%exitc(U, 06, 8);  %exitc(V, 08, 9);
%exitc(W, 08, 10); %exitc(X, 12, 11);
%exitc(Y, 14, 12); %exitc(Z, 16, 13);

** merge the back to the main file;
proc sql;
create table _temp43 as
select * from _temp42

left join _xc8 on
_temp42.hhid = _xc8.hhid and _temp42.pn = _xc8.pn
left join _xc9 on
_temp42.hhid = _xc9.hhid and _temp42.pn = _xc9.pn
left join _xc10 on
_temp42.hhid = _xc10.hhid and _temp42.pn = _xc10.pn
left join _xc11 on
_temp42.hhid = _xc11.hhid and _temp42.pn = _xc11.pn
left join _xc12 on
_temp42.hhid = _xc12.hhid and _temp42.pn = _xc12.pn
left join _xc13 on
_temp42.hhid = _xc13.hhid and _temp42.pn = _xc13.pn;
quit;

/*generating the state variable for living alone, support, disabled, nursing home, dead, loss to F/U*/
data wide; set _temp43;
if HHIDPN = . then delete; ** deletes empty rows generated from joins;
run;

** clean up;
proc datasets lib = work nolist;
	delete wave: _temp: tracker: _x:;
run;
quit;

/*###*/

data wide; set wide;
%macro cateythree(wave, var);
*collapse Vision 1-3 (excellent, very good, good) 4-6 (fair, poor, legally blind);
if 0<&var.C095<4 then BADVISION&wave.=0;
if 3<&var.C095<=6 then BADVISION&wave.=1;
*collapse Hearing 1-3 (excellent, very good, good) 4-6 (fair, poor, deaf);
if 0<&var.C103<4 then BADHEARING&wave.=0;
if 3<&var.C103<=6 then BADHEARING&wave.=1;
*collapse pain - 104 1=Yes, 5=NO then 105 3=Severe 1&2&8=mild and moderate and dont know;
if &var.C104=5 then PAIN&wave.=0;
if 1<=&var.C105<=2 then PAIN&wave.=0;
if &var.C105=3 then PAIN&wave.=1;
if &var.C105=8 then PAIN&wave.=0;
%mend;
%cateythree(7,J); %cateythree(8,K); %cateythree(9,L); 
%cateythree(10,M); %cateythree(11,N); %cateythree(12,O);
%cateythree(13,P);


%macro adlhelp(	A, /* core wave letter*/
				N, /* rand wave number */
				T /* exit wave letter */ );

&A._ADLhelp = 0; 
&A._disability_total= 0;

/*RAND variables 1 = gets help with ADL */
&A._disability_total = (R&N.DRESSH=1)+(R&N.BATHH in (1, .X))+(R&N.EATH=1)+
						(R&N.BEDH in (1, .X))+(R&N.TOILTH in (1, .X))+(R&N.WALKRH in (1, 2));

if &A._disability_total ne 0 then &A._ADLhelp = 1;

** if missing any ADL help data then set count to missing;
if ((R&N.DRESSH in (.D, .R, .M, .)) + 
	(R&N.BATHH in (.D, .R, .M, .)) + 
	(R&N.EATH in (.D, .R, .M, .)) + 
	(R&N.BEDH in (.D, .R, .M, .)) + 
	(R&N.TOILTH in (.D, .R, .M, .)) + 
	(R&N.WALKRH in (.D, .R, .M, .)) ge 1) then do;
		&A._ADLhelp = .;
		&A._disability_total = .;
	end;


** exit interview skip logic assumes those in bed for 86 days prior to death
	were disabled and skip ADL questions;

** exit interview with value = 1 (Yes help), 6 (could not do), or 7 (did not do) 
	identifies those who need help;

** if days in bed prior to death is more than 85 days then considered fully disabled;

if &T.G129 > 85 then do;
	&A._ADLhelp = 1;
	&A._disability_total = 6; 
	end;
	

if &A._ADLhelp = . then &A._disability_total = (&T.G015 in (1, 6, 7)) + (&T.G020 in (1, 6, 7)) +
		 (&T.G022 in (1, 6, 7)) + (&T.G024 in (1, 6, 7)) +
		 (&T.G029 in (1, 6, 7)) + (&T.G031 in (1, 6, 7));

if &A._ADLhelp = . and &A._disability_total ge 1 then &A._ADLhelp = 1;

/*
if &A._ADLhelp = . and 
		((&T.G015 in (1, 6, 7)) + (&T.G020 in (1, 6, 7)) +
		 (&T.G022 in (1, 6, 7)) + (&T.G024 in (1, 6, 7)) +
		 (&T.G029 in (1, 6, 7)) + (&T.G031 in (1, 6, 7)) ge 1)
		 	then 
				&A._ADLhelp=1;
*/
if &A._ADLhelp = . and 
		((&T.G015 in (5)) + (&T.G020 in (5)) +
		 (&T.G022 in (5)) + (&T.G024 in (5)) +
		 (&T.G029 in (5)) + (&T.G031 in (5)) = 6 )
		 	then do;
				&A._ADLhelp=0;
				&A._disability_total=0;
			end;

** ADL difficulty variable;
ADLdiff&N. = 0;

if ((BEDDIFF&N.>=1) + (DRESSDIFF&N.>=1)+(BATHDIFF&N.>=1)+(EATDIFF&N.>=1)+(TOILTDIFF&N.>=1)+(WALKDIFF&N.>=1))> 0 
then ADLdiff&N. = 1;


if ((BEDDIFF&N.=.) + (DRESSDIFF&N.=.) + (BATHDIFF&N.=.) + (EATDIFF&N.=.) + (TOILTDIFF&N.=.) + (WALKDIFF&N.=.) +
    (BEDDIFF&N.= .D) + (DRESSDIFF&N.= .D) + (BATHDIFF&N.= .D) + (EATDIFF&N.= .D) + (TOILTDIFF&N.= .D) + (WALKDIFF&N.= .D) +
    (BEDDIFF&N.= .M) + (DRESSDIFF&N.= .M) + (BATHDIFF&N.= .M) + (EATDIFF&N.= .M) + (TOILTDIFF&N.= .M) + (WALKDIFF&N.= .M) +
    (BEDDIFF&N.= .R) + (DRESSDIFF&N.= .R) + (BATHDIFF&N.= .R) + (EATDIFF&N.= .R) + (TOILTDIFF&N.= .R) + (WALKDIFF&N.= .R)) ge 6 

then ADLdiff&N. = .;

%mend;

%adlhelp(J,7,T); %adlhelp(K,8,U); %adlhelp(L,9,V); %adlhelp(M,10,W); 
%adlhelp(N,11,X); %adlhelp(O,12,Y); %adlhelp(P,13,Z);

run;

* data checks;
/*
proc freq;
where KAGE > 65;
tables K_ADLhelp * KWHY0RWT/ missing;
tables R8DRESSH * KWHY0RWT/ missing;
run;
*/

** clean code on inclusion criteria;

/*
Inclusion criteria: 
0.0 Alive and in interview
0.1 Community dwelling
0.2 65 and older 
0.3 Not disabled 
0.4 missing ADL data
0.5 gets help with iadls or other activities
0.6 refused to answer future help question
*/

PROC FORMAT;
VALUE  eli
	1="Met inclusion criteria"
	0.1="Resides in NH (not included)"
	0.2="< 65yrs (not included)"
	0.3="Get's help with and ADL (not included)"
	0.4="Missing ADL data (not included)"
	0.5="Get's help with an IADL or other activity (not included)"
	0.6="Refused to answer support question (not included)";
run;

/*

ASSIGN ELIGIBILITY

TRACKER CODEBOOK
https://hrs.isr.umich.edu/sites/default/files/meta/tracker/codebook/trk2018tr_ri.htm

This section heavily updated on 2/12/21

*/
%macro makeithappen(L /*CORE wave letter*/
					,N /*RAND wave number*/ );
data wide; set wide;
if &L.WHY0RWT in (0, 2, 3) then eli&N. = 1; 
* REMOVE nursing home as an exclusion 
*if &L.NURSHM in (1,3) and eli&N. = 1 then eli&N. = 0.1; 
if &L.AGE < 65 and eli&N. = 1  then eli&N. = 0.2;
*if &L._ADLhelp = 1 and eli&N. = 1  then eli&N. = 0.3;
*if &L._ADLhelp = . and eli&N. = 1  then eli&N. = 0.4;
*if &L.G097 = . and eli&N. = 1 then eli&N. = 0.5;
*if &L.G097 = 9 and eli&N. = 1 then eli&N. = 0.6;
format eli&N. eli.;
run;
%mend;

%makeithappen(J,7); %makeithappen(K,8); 
%makeithappen(L,9); %makeithappen(M,10); 
%makeithappen(N,11); %makeithappen(O,12);
%makeithappen(P,13);

proc freq;
tables eli:;
run;


** outcome assessment;
%macro outcomes( L /*CORE wave letter*/
				,N /*RAND wave number*/
				,X /*EXIT interview leter*/
				,M /*RAND wave N-1*/
				);

data wide; set wide;

** if they are eligible in wave n-1 then 
set outcome in wave n to 0 rest are missing;
if eli&M. = 1 then do;
	dead&N. = 0;
	nh&N. = 0;
	new_disable&N. = 0;
end;

* code death outocme; 
if dead&N. ne . then do;
	if &L.WHY0RWT = 1 then dead&N. = 1;
	else if &L.WHY0RWT in (0, 2, 3) then dead&N. = 0;
	else if &L.WHY0RWT in (5, 6, 7) then dead&N. = .C;
	else dead&N. = .M;
end;

** code nursing home outcome
	have to have spent 30+ days in a NH;

if nh&N. = 0  then do;
	if &L.WHY0RWT = 2  then nh&N. = 1; * R interview in NH;
	else if &L.WHY0RWT in (1, 5, 6, 7) then nh&N. = .C;
end;

if nh&N. in (0, .C) then do;
	if &L.N114 = 1 and (998>&L.N116>29) then nh&N. = 1; *core interview days in NH;
		else if &L.N114 = 1 and &L.N116 = 998 and nh&N. = .C then nh&N. = 0;
		else if &L.N114 = 1 and &L.N116 le 29 and nh&N. = .C then nh&N. = 0;
		else if &L.N114 = 5 and nh&N. = .C then nh&N. = 0; 
	if &X.N114 = 1 and (998>&X.N116>29) then nh&N. = 1; *exit interview days in NH;
		else if &X.N114 = 1 and &X.N116 = 998 and nh&N. = .C then nh&N. = 0;
		else if &X.N114 = 1 and &X.N116 le 29 and nh&N. = .C then nh&N. = 0;
		else if &X.N114 = 5 and nh&N. = .C then nh&N. = 0; 
	if &L.N114 = 1 and (98>&L.N117>1) then nh&N. = 1; *core intervie mo in NH;
		else if &L.N114 = 1 and &L.N117 = 98 and nh&N. = .C then nh&N. = 0;
		else if &L.N114 = 1 and &L.N117 < 1 and nh&N. = .C then nh&N. = 0;
	if &X.N114 = 1 and (98>&X.N117>1) then nh&N. = 1; *exit interview mo in NH;
		else if &X.N114 = 1 and &X.N117 = 98 and nh&N. = .C then nh&N. = 0;
		else if &X.N114 = 1 and &X.N117 < 1 and nh&N. = .C then nh&N. = 0;
	if &X.A124 = 2 then nh&N. = 1; * died in NH;
end;

** code ADL outcome;

if new_disable&N. = 0 then do;
	if &L._ADLhelp = . then new_disable&N. = .C;
	else new_disable&N. = &L._ADLhelp;
end;

** convert all the outcomes to the prior wave with the OUT_ prefix;
OUT_deceased&M. = dead&N.;
OUT_nursinghome_&M. = nh&N.;
OUT_ADLhelp_&M. = new_disable&N.;
run;	
%mend;

* %outcomes(J,7,T,6); %outcomes(K,8,U,7); 
%outcomes(L,9,V,8); %outcomes(M,10,W,9); 
%outcomes(N,11,X,10); %outcomes(O,12,Y,11);
%outcomes(P,13,Z,12);

data wide; set wide;
drop dead7-dead13 nh7-nh13 new_disable7-new_disable13;
run;


** data check;
/*
proc freq;
tables OUT_: / missing;
run;

proc freq;
tables nh8 / missing;
tables dead8 / missing;
tables new_disable8 / missing;
where eli7 = 1;
run;
*/


** data check -> many exit interviews have 
terminal hospitalizatios;
/*
proc freq;
tables VN100 * VA124 / missing;
where eli7 =1  ;
where VN100 ne .;
run;
*/
%macro healthshock(	L,/*CORE wave letter*/
					N,/*RAND wave number*/
					X,/*EXIT wave letter*/
					M/*RAND wave N-1*/
					);

data wide; set wide;

/*********************** Hospitalization ************************/

**ID ppl eligible for outcome, set to 0;
if eli&M. = 1 then shockhospitalized&M. = 0; 

** if they are not in the f/u wave OR did not answer hospitalization question
		then set to .C = censor;
if shockhospitalized&M. = 0 and (&L.WHY0RWT in (1, 5, 6, 7)) 
	then shockhospitalized&M. = .C; 
if shockhospitalized&M. = 0 and (&L.N099 in (8,9,.)) 
	then shockhospitalized&M. = .C; 
if shockhospitalized&M. = 0 and &L.N099 = 1 and (&L.N101 in (998, 999))  
	then shockhospitalized&M. = .C;
 
** death is already considered to be a censoring event so no need
	to recode 0 -> .C for those who did not answer XN099
	BUT
	we need to code from .C -> 0 for those who died, have an exit
	interview and replied no hospitalizations;
if shockhospitalized&M. = .C and &X.N099 = 5 
	then shockhospitalized&M. = 0;
if shockhospitalized&M. = .C and &X.N099 = 1 and &X.N101 < 2 
	then shockhospitalized&M. = 0;

** if they spent 2 or more nights in the hospital then shock = 1;
if (shockhospitalized&M. in (0,.C)) and &L.N101 ge 2 
	then shockhospitalized&M. = 1;
if (shockhospitalized&M. in (0,.C)) and &X.N101 ge 2 
	then shockhospitalized&M. = 1;

** set terminal hospitalizations to 0 b/c not really a shock;
if shockhospitalized&M. = 1 and &X.N100 = 1 and &X.A124 = 1 
	then shockhospitalized&M. = 0;

/*********************** CANCER + HEART + STROKE ************************/

**ID ppl eligible for outcome, set to 0;
if eli&M. = 1 then do;
	SHOCKCANCER&M. = 0; 
	SHOCKHEARTD&M. = 0;
	SHOCKSTROKE&M. = 0;
end;

if SHOCKCANCER&M. = 0 then do;
	if CANCER&M. = . or CANCER&N. = . then SHOCKCANCER&M. = .C;
	if CANCER&M. = 0 and (CANCER&N. = 0 OR EXIT_CANCER&N. =5) then SHOCKCANCER&M. = 0;
	if CANCER&M. = 0 and (CANCER&N. = 1 OR EXIT_CANCER&N. =1) then SHOCKCANCER&M. = 1;
	if CANCER&M. = 1 and (CANCER&N. in (0,1) OR EXIT_CANCER&N. in (1,5)) then SHOCKCANCER&M. = 0;
end;

if SHOCKHEARTD&M. = 0 then do;
	if HEARTD&M. = . or HEARTD&N. = . then SHOCKHEARTD&M. = .C;
	if HEARTD&M. = 0 and (HEARTD&N. = 0 OR EXIT_HEARTD&N. =5) then SHOCKHEARTD&M. = 0;
	if HEARTD&M. = 0 and (HEARTD&N. = 1 OR EXIT_HEARTD&N. =1) then SHOCKHEARTD&M. = 1;
	if HEARTD&M. = 1 and (HEARTD&N. in (0,1) OR EXIT_HEARTD&N. in (1,5)) then SHOCKHEARTD&M. = 0;
end;

if SHOCKSTROKE&M. = 0 then do;
	if STROKE&M. = . or STROKE&N. = . then SHOCKSTROKE&M. = .C;
	if STROKE&M. = 0 and (STROKE&N. = 0 OR EXIT_STROKE&N. =5) then SHOCKSTROKE&M. = 0;
	if STROKE&M. = 0 and (STROKE&N. = 1 OR EXIT_STROKE&N. =1) then SHOCKSTROKE&M. = 1;
	if STROKE&M. = 1 and (STROKE&N. in (0,1) OR EXIT_STROKE&N. in (1,5)) then SHOCKSTROKE&M. = 0;
end;

SHOCK&M. = SUM(SHOCKSTROKE&M., SHOCKHEARTD&M., SHOCKCANCER&M., shockhospitalized&M.); 

run;
%mend;

* %healthshock(J,7,T,6); * run but not useful b/c 2004 is the first entry year;
%healthshock(K,8,U,7);
%healthshock(L,9,V,8);
%healthshock(M,10,W,9); 
%healthshock(N,11,X,10); 
%healthshock(O,12,Y,11);
%healthshock(P,13,X,12);


** data check;
/*
proc freq;
tables SHOCKCANCER11 * OWHY0RWT / missing;
tables SHOCKHEARTD11 * OWHY0RWT / missing;
tables SHOCKSTROKE11 * OWHY0RWT / missing;
where eli11 = 1;
run;

proc freq;
tables EXIT_CANCER8 * KWHY0RWT / missing;
tables EXIT_HEARTD8 * KWHY0RWT / missing;
tables EXIT_STROKE8 * KWHY0RWT / missing;
where eli7 = 1;
run;
*/

** data check;
/*
* how many are not in the follow up wave;
proc freq;
tables KWHY0RWT / missing;
where eli7 = 1;
run; * 314 are not in the follow up wave;

proc freq;
tables shockhospitalized7/ missing;
where eli7 = 1;
run; ** 372-314 = 58 are missing values;
** where 58 missing coming from?;

proc freq;
tables shockhospitalized7 * KWHY0RWT/ missing;
where eli7 = 1 ;
run; * of missing 15 are alive, 37 are dead, 2 in NH
*/

ods trace on;
proc contents data = wide order = varnum;
run;

ods trace off;

proc freq;
tables OUT_: / missing;
run;

data wide2; set wide;
	*OUT_ADLhelp_12=.;
	*OUT_deceased12=.;
	*OUT_nursinghome_12=.;

WAVE6=6;
WAVE7=7;
WAVE8=8;
WAVE9=9;
WAVE10=10;
WAVE11=11;
WAVE12=12;
WAVE13=13;


%macro renameJ7(A,N,X);
rename &A.AGE = AGE&N.;
*rename &A.G015 = G015_&N.;
*rename &A.G020 = G020_&N.;
*rename &A.G022 = G022_&N.;
*rename &A.G024 = G024_&N.;
*rename &A.G029 = G029_&N.;
*rename &A.G031 = G031_&N.;
rename &A.G097 = G097_&N.;
rename &A.N099 = N099_&N.;
rename &A.N101 = N101_&N.;
rename &A.N114 = N114_&N.;
rename &A.N116 = N116_&N.;
rename &A.N117 = N117_&N.;
rename &X.N099 = N099_exit_&N.;
rename &X.N101 = N101_exit_&N.;
rename &X.N114 = N114_exit_&N.;
rename &X.N116 = N116_exit_&N.;
rename &X.N117 = N117_exit_&N.;
rename &A.WHY0RWT = WHY0RWT_&N.;
rename &A._ADLhelp = ADLhelp_&N.;
*rename &A._disability_persist = disability_persist1_&N.;
*rename &A._disability_persist2 = disability_persist2_&N.;
*rename &A._disability_persist3 = disability_persist3_&N.;
*rename &A._disability_total = disability_total_&N.;
rename H&N.HHRES = HHRES&N.;
rename R&N.ADLA = ADLA&N.;
rename R&N.IWSTAT = IWSTAT&N.;
rename W&N.LA = LA&N.;
* rename R&N.WTRESP = WEIGHTS&N.; ** not using weights in analysis;
rename R&N.PROXY = PROXY&N.;
%mend;
%renameJ7(J,7,T);%renameJ7(K,8,U);%renameJ7(L,9,V);
%renameJ7(M,10,W);%renameJ7(N,11,X);%renameJ7(O,12,Y);
%renameJ7(P,13,Z);
run;


** join in LW cognition data;
data lw
(rename = (cogfunction2004 = cog7
			cogfunction2006 = cog8
			cogfunction2008 = cog9
			cogfunction2010 = cog10
			cogfunction2012 = cog11
			cogfunction2014 = cog12
			cogfunction2016 = cog13	));

set lw.cogfinalimp_9516wide;
keep hhid pn cogfunction2004-cogfunction2016 ;
run;

proc sql;
create table wide3 as 
select * from wide2
left join lw 
on wide2.hhid = lw.hhid and wide2.pn = lw.pn;
quit;

proc contents data=wide3;
run;

options nonotes;


data long (keep=HHID PN HHIDPN HHRES INW AGE WHY0RWT PROXY WAVE
	IWSTAT LA eli RAFEMALE RARACEM RAHISPAN RAVETRN HSGRAD INC_ABV_MED 
	married poorhealth HIGHBP DIABETES CANCER LUNGD ARTHRITIS HEARTD 
	STROKE DEPRESSION SMOKING cog BADVISION BADHEARING PAIN 
	G097 /*HELP with future needs*/
	G015 G020 G022 G024 G029 G031 /*HELP with ADLS*/
	ADLA 
	ADLhelp disability_total ADLdiff
	WALKDIFF DRESSDIFF BATHDIFF	EATDIFF TOILTDIFF BEDDIFF
	N099 N101 N099_exit N101_exit /*HOSPITALIZATIONS*/
	N114 N116 N117  N114_exit N116_exit N117_exit /*NURSING HOME*/
	WEIGHTS 
	OUT_ADLhelp OUT_deceased OUT_nursinghome 
	SHOCKCANCER SHOCKHEARTD SHOCKSTROKE SHOCK shockhospitalized);

set wide3;
array aHHRES (7:13) HHRES7-HHRES13;
array aINW (7:13) INW7-INW13;
array aAGE(7:13) AGE7-AGE13;
array aWHY0RWT (7:13) WHY0RWT_7-WHY0RWT_13;
array aPROXY (7:13) PROXY7-PROXY13;
array aWAVE (7:13) WAVE7-WAVE13;
array aIWSTAT (7:13) IWSTAT7-IWSTAT13;
array aLA (7:13) LA7-LA13;
array aeli (7:13) eli7-eli13;
array aINC_ABV_MED (7:13) INC_ABV_MED7-INC_ABV_MED13;
array amarried (7:13) married7-married13;
array apoorhealth (7:13) poorhealth7-poorhealth13;
array aHIGHBP (7:13) HIGHBP7-HIGHBP13;
array aDIABETES (7:13) DIABETES7-DIABETES13;
array aCANCER (7:13) CANCER7-CANCER13;
array aLUNGD (7:13) LUNGD7-LUNGD13;
array aARTHRITIS (7:13) ARTHRITIS7-ARTHRITIS13;
array aHEARTD (7:13) HEARTD7-HEARTD13;
array aSTROKE (7:13) STROKE7-STROKE13;
array aDEPRESSION (7:13) DEPRESSION7-DEPRESSION13;
array aSMOKING (7:13) SMOKING7-SMOKING13;
array acog(7:13) cog7-cog13;
array aBADVISION (7:13) BADVISION7-BADVISION13;
array aBADHEARING (7:13) BADHEARING7-BADHEARING13;
array aPAIN (7:13) PAIN7-PAIN13;
array aG097(7:13) G097_7-G097_13;
array aG015(7:13) G015_7-G015_13;
array aG020(7:13) G020_7-G020_13;
array aG022(7:13) G022_7-G022_13;
array aG024(7:13) G024_7-G024_13;
array aG029(7:13) G029_7-G029_13;
array aG031(7:13) G031_7-G031_13;
array aADLA (7:13) ADLA7-ADLA13;
array aADLhelp(7:13) ADLhelp_7-ADLhelp_13;
array adisability_total(7:13) disability_total_7-disability_total_13;
array aADLdiff (7:13) ADLdiff7-ADLdiff13;
array aWALKDIFF (7:13) WALKDIFF7-WALKDIFF13;
array aDRESSDIFF (7:13) DRESSDIFF7-DRESSDIFF13;
array aBATHDIFF (7:13) BATHDIFF7-BATHDIFF13;
array aEATDIFF (7:13) EATDIFF7-EATDIFF13;
array aTOILTDIFF (7:13) TOILTDIFF7-TOILTDIFF13;
array aBEDDIFF (7:13) BEDDIFF7-BEDDIFF13;
array aN099(7:13) N099_7-N099_13;
array aN101(7:13) N101_7-N101_13;
array aN099_exit(7:13) N099_exit_7-N099_exit_13;
array aN101_exit(7:13) N101_exit_7-N101_exit_13;
array aN114(7:13) N114_7-N114_13;
array aN116(7:13) N116_7-N116_13;
array aN117(7:13) N117_7-N117_13;
array aN114_exit(7:13) N114_exit_7-N114_exit_13;
array aN116_exit(7:13) N116_exit_7-N116_exit_13;
array aN117_exit(7:13) N117_exit_7-N117_exit_13;
array aWEIGHTS (7:13) WEIGHTS7-WEIGHTS13;
array aOUT_ADLhelp(7:13) OUT_ADLhelp_7-OUT_ADLhelp_13;
array aOUT_deceased(7:13) OUT_deceased7-OUT_deceased13;
array aOUT_nursinghome(7:13) OUT_nursinghome_7-OUT_nursinghome_13;
array aSHOCKCANCER (7:13) SHOCKCANCER7-SHOCKCANCER13;
array aSHOCKHEARTD (7:13) SHOCKHEARTD7-SHOCKHEARTD13;
array aSHOCKSTROKE (7:13) SHOCKSTROKE7-SHOCKSTROKE13;
array aSHOCK (7:13) SHOCK7-SHOCK13;
array ashockhospitalized (7:13) shockhospitalized7-shockhospitalized13;

do wave= 7 to 13;
	HHRES=aHHRES[wave];
	INW=aINW[wave];
	AGE = aAGE[wave];	
	WHY0RWT=aWHY0RWT[wave];
	PROXY=aPROXY[wave];
	WAVE=aWAVE[wave];
	IWSTAT=aIWSTAT[wave];
	LA=aLA[wave];
	ELI=aeli[wave];
	INC_ABV_MED=aINC_ABV_MED[wave];
 	married = amarried[wave];
	poorhealth= apoorhealth[wave];
	HIGHBP= aHIGHBP[wave];
 	DIABETES= aDIABETES[wave];
 	CANCER= aCANCER[wave];
 	LUNGD = aLUNGD[wave];
 	ARTHRITIS= aARTHRITIS[wave];
 	HEARTD = aHEARTD[wave];
 	STROKE = aSTROKE[wave];
 	DEPRESSION = aDEPRESSION[wave];
 	SMOKING = aSMOKING[wave];
	COG = acog[wave];
	BADVISION = aBADVISION[wave];
 	BADHEARING = aBADHEARING[wave];
 	PAIN = aPAIN[wave];
	G097=aG097[wave];
	G015=aG015[wave];
	G020=aG020[wave];
	G022=aG022[wave];
	G024=aG024[wave];
	G029=aG029[wave];
	G031=aG031[wave];
	ADLA=aADLA[wave];
	ADLhelp=aADLhelp[wave];
	disability_total=adisability_total[wave];
	ADLDIFF = aADLDIFF[wave];
	WALKDIFF = aWALKDIFF[wave];
 	DRESSDIFF = aDRESSDIFF[wave];
 	BATHDIFF = aBATHDIFF[wave];
 	EATDIFF = aEATDIFF[wave];
 	TOILTDIFF = aTOILTDIFF[wave];
	BEDDIFF = aBEDDIFF[wave];
	N099=aN099[wave];
	N101=aN101[wave];
	N099_exit=aN099_exit[wave];
	N101_exit=aN101_exit[wave];	
	N114=aN114[wave];
	N116=aN116[wave];
	N117=aN117[wave];
	N114_exit=aN114_exit[wave];
	N116_exit=aN116_exit[wave];
	N117_exit=aN117_exit[wave];
	WEIGHTS= aWEIGHTS[wave];
	OUT_ADLhelp=aOUT_ADLhelp[wave];
	OUT_deceased=aOUT_deceased[wave];
	OUT_nursinghome=aOUT_nursinghome[wave];
	SHOCKCANCER = aSHOCKCANCER[wave];
 	SHOCKHEARTD = aSHOCKHEARTD[wave];
 	SHOCKSTROKE= aSHOCKSTROKE[wave];
	SHOCK = aSHOCK[wave];
	shockhospitalized= ashockhospitalized[wave];
	output;
end;
run; 


proc contents data=long order = varnum;
run;

data out.analytic_long_2021_05_13; set work.long;
run;

data out.analytic_wide_2021_05_13; set work.wide3;
run;

proc datasets lib=work nolist;
	delete wide wide2 lw;
run;
quit;
