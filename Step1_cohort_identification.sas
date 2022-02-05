
/** Social Vulnerability Index (SVI) mortality prediction model using HRS (linked to Medicare claims) **/

/** cohort chart flow*/
/*(1) Completion of 2010 LB (baseline) survey */
/*(2) 65+ at 2010 interview */
/*(3) Mortality in 2012 (by interview date) and 2014 interview date*/
/*(4) ADL impairement status 2010/2012/2014*/
/*(5) Whether they have Medicare linkage until the next interview (consecutive enrollment to 2012 interview and also to 2014 interview)*/

/** Then we are constructing a dataset that contains ALL psychosocial variables. This will be fitted to LASSO for variable selection */
/* (i) Extracting psychosocial variables extraction from core interview 2010 */
/* (ii) Extracting sychosocial variables from RAND 2010 */

libname mdcr 'V:\Health and Retirement Study\DATAinSAS2018\HRS_Provider_Distribution HRS018 - NCIRE 51424';
libname trk 'V:\Health and Retirement Study\Grisell\HRS_data\trk2018earlyv2a'; 
libname doi 'V:\Health and Retirement Study\DATAinSAS\doi_gdr';
libname dod 'V:\Health and Retirement Study\DATAinSAS\dod_gdr';
libname randp 'V:\Health and Retirement Study\Grisell\HRS_data\rand\randhrs1992_2016v2';
proc format cntlin=randp.sasfmts;run;
libname fat 'V:\Health and Retirement Study\Grisell\HRS_data\rand\fatfiles';
libname sv 'V:\Health and Retirement Study\Sun\Sachin\psychosocial\data_interim'; 
libname xwalk 'V:\Health and Retirement Study\DATAinSAS2020\xref\Data\Medicare\Xref2015\sas\'; 
libname lb "V:\Health and Retirement Study\Grisell\HRS_data\rand\fatfiles"; 

/*************************************************************************/
/*(0) Any leave behind survey between 2006 and 2014; interivew at age>=65*/
/*************************************************************************/

***** Prepare tracker file;
data trk2016 (drop=HHID PN HHIDPNC);
	set trk.trk2016tr_r;
	HHIDPNC=HHID||PN; /*concatenates HHI (char: 6) with PN (char: 3)*/
  	hhidpn=INPUT(HHIDPNC,9.0); /*converts HHIDPNC from character to numeric*/
 proc sort; by hhidpn; run; /*43216*/


/****************************/
/*(1) Completion of 2010 LB */
/****************************/

proc sql;
 	create table wave_2010 as 
	select HHIDPN, MLBCOMP as complete, 2010 as LB_year /*self completion by mail, phone, or someone other than designated person*/
	from fat.hd10f5e
	where 1<=MLBCOMP<5;
quit; /*8332*/


/*******************/
/* (2) 65+ in 2010 */
/*******************/

data cohort_1; 
merge wave_2010 (in=A) trk2016 (keep=HHIDPN MAGE) /*randp.Randhrs1992_2016v1 (keep=HHIDPN R10AGEY_B RABMONTH RABYEAR RABDATE)*/; 
by HHIDPN; 
if A; 

if MAGE>65; 
run; /*4302 */


/*************************************************************************************/
/* (3) quickly checking Mortality in 2012 and 2014 using NALIVE and OALIVE in tracker*/
/*************************************************************************************/

data cohort_2; 
merge cohort_1 (in=A) trk2016 (keep=HHIDPN NALIVE OALIVE MIWTYPE NIWTYPE OIWTYPE); 
by HHIDPN; 
if A; 
run; 
proc freq data=cohort_2; tables NALIVE OALIVE; run;
proc freq data=cohort_2; tables NIWTYPE*NALIVE; run;
proc freq data=cohort_2; tables OIWTYPE*OALIVE; run;


/***********************************************/
/* (4) ADL impairement in 2010, 2012, and 2014 */
/***********************************************/

/** (i) ADL_IADL RECODING -- combining ADL/IADL difficulty and dependence variables and make one categorical variable with 3 levels (0=indep, 1=diff, 2=dep)**/
data adl_iadl; set randp.Derived_adl_iadl_gdr_20190702; run;
data adl_iadl_cat(keep=HHIDPN  R1BATH_cat 	R1BED_cat  		R1DRESS_cat 	R1EAT_cat 	R1TOILT_cat 	R1WALKR_cat 
				   R2BATH_cat 	R2BED_cat  		R2DRESS_cat 	R2EAT_cat 	R2TOILT_cat 	R2WALKR_cat 
				   R3BATH_cat 	R3BED_cat  		R3DRESS_cat 	R3EAT_cat 	R3TOILT_cat 	R3WALKR_cat  
				   R4BATH_cat 	R4BED_cat  		R4DRESS_cat 	R4EAT_cat 	R4TOILT_cat 	R4WALKR_cat 
				   R5BATH_cat 	R5BED_cat  		R5DRESS_cat 	R5EAT_cat 	R5TOILT_cat 	R5WALKR_cat
				   R6BATH_cat 	R6BED_cat  		R6DRESS_cat 	R6EAT_cat 	R6TOILT_cat 	R6WALKR_cat
				   R7BATH_cat 	R7BED_cat  		R7DRESS_cat 	R7EAT_cat 	R7TOILT_cat 	R7WALKR_cat
				   R8BATH_cat 	R8BED_cat  		R8DRESS_cat 	R8EAT_cat 	R8TOILT_cat 	R8WALKR_cat 
				   R9BATH_cat 	R9BED_cat  		R9DRESS_cat 	R9EAT_cat 	R9TOILT_cat 	R9WALKR_cat  
				   R10BATH_cat R10BED_cat  		R10DRESS_cat 	R10EAT_cat R10TOILT_cat 	R10WALKR_cat 
				   R11BATH_cat R11BED_cat  		R11DRESS_cat 	R11EAT_cat R11TOILT_cat 	R11WALKR_cat 
				   R12BATH_cat	R12BED_cat		R12DRESS_cat	R12EAT_cat	R12TOILT_cat	R12WALKR_cat
				   R13BATH_cat	R13BED_cat		R13DRESS_cat	R13EAT_cat	R13TOILT_cat	R13WALKR_cat
				   ); 
set adl_iadl ; 

ARRAY ADL_DIFF [*] R1BATHW 	R1BEDW  	R1DRESSW 	R1EATW 	R1TOILTW 	R1WALKRW 
				   R2BATHA 	R2BEDA  	R2DRESSA 	R2EATA 	R2TOILTA 	R2WALKRA 
				   R3BATHA 	R3BEDA  	R3DRESSA 	R3EATA 	R3TOILTA 	R3WALKRA  
				   R4BATHA 	R4BEDA  	R4DRESSA 	R4EATA 	R4TOILTA 	R4WALKRA 
				   R5BATHA 	R5BEDA  	R5DRESSA 	R5EATA 	R5TOILTA 	R5WALKRA
				   R6BATHA 	R6BEDA  	R6DRESSA 	R6EATA 	R6TOILTA 	R6WALKRA 
				   R7BATHA 	R7BEDA  	R7DRESSA 	R7EATA 	R7TOILTA 	R7WALKRA
				   R8BATHA 	R8BEDA  	R8DRESSA 	R8EATA 	R8TOILTA 	R8WALKRA 
				   R9BATHA 	R9BEDA  	R9DRESSA 	R9EATA 	R9TOILTA 	R9WALKRA  
				   R10BATHA R10BEDA  	R10DRESSA 	R10EATA R10TOILTA 	R10WALKRA 
				   R11BATHA R11BEDA  	R11DRESSA 	R11EATA R11TOILTA 	R11WALKRA 
				   R12BATHA	R12BEDA		R12DRESSA	R12EATA	R12TOILTA	R12WALKRA
				   R13BATHA	R13BEDA		R13DRESSA	R13EATA	R13TOILTA	R13WALKRA;

ARRAY ADL_DEP[*] R1BATDE  	R1BEDDE   	R1DRESSDE 	R1EATDE   	R1TOILTDE 	R1WALKRDE 
				 R2BATDE  	R2BEDDE   	R2DRESSDE 	R2EATDE   	R2TOILTDE 	R2WALKRDE 
				 R3BATDE  	R3BEDDE   	R3DRESSDE 	R3EATDE   	R3TOILTDE 	R3WALKRDE 
				 R4BATDE  	R4BEDDE   	R4DRESSDE 	R4EATDE   	R4TOILTDE 	R4WALKRDE
				 R5BATDE 	R5BEDDE  	R5DRESSDE 	R5EATDE   	R5TOILTDE 	R5WALKRDE
				 R6BATDE   	R6BEDDE   	R6DRESSDE 	R6EATDE   	R6TOILTDE 	R6WALKRDE
				 R7BATDE   	R7BEDDE   	R7DRESSDE 	R7EATDE   	R7TOILTDE 	R7WALKRDE
				 R8BATDE   	R8BEDDE   	R8DRESSDE 	R8EATDE  	R8TOILTDE 	R8WALKRDE 
				 R9BATDE   	R9BEDDE   	R9DRESSDE 	R9EATDE   	R9TOILTDE 	R9WALKRDE 
				 R10BATDE   R10BEDDE   	R10DRESSDE 	R10EATDE   	R10TOILTDE 	R10WALKRDE 
				 R11BATDE	R11BEDDE	R11DRESSDE	R11EATDE	R11TOILTDE	R11WALKRDE
				 R12BATDE	R12BEDDE	R12DRESSDE	R12EATDE	R12TOILTDE	R12WALKRDE
				 R13BATDE	R13BEDDE	R13DRESSDE	R13EATDE	R13TOILTDE	R13WALKRDE;

ARRAY ADL[*] 	   R1BATH_cat 	R1BED_cat  		R1DRESS_cat 	R1EAT_cat 	R1TOILT_cat 	R1WALKR_cat 
				   R2BATH_cat 	R2BED_cat  		R2DRESS_cat 	R2EAT_cat 	R2TOILT_cat 	R2WALKR_cat 
				   R3BATH_cat 	R3BED_cat  		R3DRESS_cat 	R3EAT_cat 	R3TOILT_cat 	R3WALKR_cat  
				   R4BATH_cat 	R4BED_cat  		R4DRESS_cat 	R4EAT_cat 	R4TOILT_cat 	R4WALKR_cat 
				   R5BATH_cat 	R5BED_cat  		R5DRESS_cat 	R5EAT_cat 	R5TOILT_cat 	R5WALKR_cat
				   R6BATH_cat 	R6BED_cat  		R6DRESS_cat 	R6EAT_cat 	R6TOILT_cat 	R6WALKR_cat
				   R7BATH_cat 	R7BED_cat  		R7DRESS_cat 	R7EAT_cat 	R7TOILT_cat 	R7WALKR_cat
				   R8BATH_cat 	R8BED_cat  		R8DRESS_cat 	R8EAT_cat 	R8TOILT_cat 	R8WALKR_cat 
				   R9BATH_cat 	R9BED_cat  		R9DRESS_cat 	R9EAT_cat 	R9TOILT_cat 	R9WALKR_cat  
				   R10BATH_cat R10BED_cat  		R10DRESS_cat 	R10EAT_cat R10TOILT_cat 	R10WALKR_cat 
				   R11BATH_cat R11BED_cat  		R11DRESS_cat 	R11EAT_cat R11TOILT_cat 	R11WALKR_cat 
				   R12BATH_cat	R12BED_cat		R12DRESS_cat	R12EAT_cat	R12TOILT_cat	R12WALKR_cat
				   R13BATH_cat	R13BED_cat		R13DRESS_cat	R13EAT_cat	R13TOILT_cat	R13WALKR_cat;
Do I =1 to 78; 
if ADL_DIFF[I] = 0 then ADL[I]=0; 
if ADL_DIFF[I] = 1 & ADL_DEP[I] ~=1 then ADL[I] =1; 
if ADL_DEP[I] = 1 then ADL[I] = 2; 
END; DROP I; 

run; 

/* (ii) create overall ADL status variable (R#ADL) that combines 6 ADL items and indicates the worst functional impairment across the items */
data adl (keep=HHIDPN  R10ADL		R11ADL		R12ADL); 
set adl_iadl_cat; 

ARRAY BATH[*] 	R1BATH_cat	R2BATH_cat	R3BATH_cat	R4BATH_cat	R5BATH_cat	R6BATH_cat	R7BATH_cat	R8BATH_cat	R9BATH_cat	R10BATH_cat		R11BATH_cat		R12BATH_cat		R13BATH_cat;
ARRAY BED[*]	R1BED_cat 	R2BED_cat 	R3BED_cat 	R4BED_cat 	R5BED_cat 	R6BED_cat 	R7BED_cat 	R8BED_cat 	R9BED_cat 	R10BED_cat 		R11BED_cat 		R12BED_cat 		R13BED_cat ;
ARRAY DRES[*]	R1DRESS_cat	R2DRESS_cat	R3DRESS_cat	R4DRESS_cat	R5DRESS_cat	R6DRESS_cat	R7DRESS_cat	R8DRESS_cat	R9DRESS_cat	R10DRESS_cat	R11DRESS_cat	R12DRESS_cat	R13DRESS_cat;
ARRAY EAT[*] 	R1EAT_cat	R2EAT_cat	R3EAT_cat	R4EAT_cat	R5EAT_cat	R6EAT_cat	R7EAT_cat	R8EAT_cat	R9EAT_cat	R10EAT_cat		R11EAT_cat		R12EAT_cat		R13EAT_cat	;
ARRAY TOIL[*]	R1TOILT_cat	R2TOILT_cat	R3TOILT_cat	R4TOILT_cat	R5TOILT_cat	R6TOILT_cat	R7TOILT_cat	R8TOILT_cat	R9TOILT_cat	R10TOILT_cat	R11TOILT_cat	R12TOILT_cat	R13TOILT_cat;
ARRAY WALK[*] 	R1WALKR_cat	R2WALKR_cat	R3WALKR_cat	R4WALKR_cat	R5WALKR_cat	R6WALKR_cat	R7WALKR_cat	R8WALKR_cat	R9WALKR_cat	R10WALKR_cat	R11WALKR_cat	R12WALKR_cat	R13WALKR_cat;

ARRAY ADL[*]	R1ADL		R2ADL		R3ADL		R4ADL		R5ADL		R6ADL		R7ADL		R8ADL		R9ADL		R10ADL		R11ADL		R12ADL	R13ADL;

DO I=1 to 13; 
ADL[I] = MAX(BATH[I], BED[I], DRES[I], EAT[I], TOIL[I], WALK[I]) ;
END; DROP I; 

run; 
data cohort_3; 
merge cohort_2 (in=A) adl; 
by HHIDPN; 
if A; 
run; 

proc freq data=cohort_3; tables R10ADL; run; 
proc freq data=cohort_3; tables R11ADL; where NALIVE in (1,2); run; 
proc freq data=cohort_3; tables R12ADL; where OALIVE in (1,2); run; 



/****************************************************************************/
/** (5) Medicare Linkage until 2012 and 2014 interivews, consecutive months */
/****************************************************************************/

/* Here we are trying to look at whether they have Medicare linkage until the next interview 
(consecutive enrollment to 2012 interview and also to 2014 interview) from the 2010 baseline*/

/*(i) First, bringing in the valid 2012 & 2014 interview dates */
data cohort_4 ;
merge cohort_3(in=A)
	 doi.doi2017_gdr (keep=HHIDPN MIWDATE NIWDATE OIWDATE); 
by HHIDPN; 
if A; 
if NALIVE in (5, 6) then NIWDATE = . ; 
if OALIVE in (5, 6) then OIWDATE = . ; 

ARRAY IWDATE [*] MIWDATE NIWDATE OIWDATE; 
ARRAY MO [*] 	 MO_2010 MO_2012 MO_2014; 
ARRAY YR [*]  	 YR_2010 YR_2012 YR_2014; 

DO I=1 to dim(IWDATE); 
	MO[I] = month(IWDATE[I]); 
	YR[I] = year (IWDATE[I]); 
END; DROP I; 

run; 	

/*(ii) How many "months" from 2010 baseline to 2012 and 2014 interviews?*/
proc freq data=cohort_4; tables NIWDATE; where NALIVE <5 & NIWTYPE=1; run; /*3892*/
data cohort_2012_0; 
set cohort_4; 
if NALIVE <5 & NIWTYPE=1; /*valid core interview and alive in 2012) */

MO_to_2012 = (2011-YR_2010)*12 + (12-MO_2010+1) + (YR_2012 -2012)*12 + MO_2012; 
MO_to_2014 = ((YR_2014-2010)*12 + MO_2014) - ((YR_2010-2010)*12 + MO_2010)+1; 
run; /*3892*/

/* (iii) Monthly medicare enrollment in 2010, 2011, 2012, and 2013 from medicare data*/

	%MACRO HMOIND(INDATA,YR);
	DATA A (KEEP=BID_HRS_22 HMOIND12 BUYIN12 rename=(BUYIN12=BUYIN12_&YR HMOIND12=HMOIND12_&YR)); SET &INDATA ;
	PROC SORT; BY BID_HRS_22 HMOIND12_&YR; RUN;
	PROC SORT DATA=A OUT=DN&YR nodupkey; BY BID_HRS_22; RUN;
	%MEND HMOIND;
	%HMOIND(MDCR.DN_2010,10)
	%HMOIND(MDCR.DN_2011,11)
	%HMOIND(MDCR.DN_2012,12)
	
	/*### UPDATED MBSF 2013-2015 */	
	%MACRO HMOIND1(INDATA,YR);
		DATA A (keep=BID_HRS_22 HMOIND12_&YR BUYIN12_&YR); 
		length HMOIND12_&YR  $12	BUYIN12_&YR $12;
		SET &INDATA;
		HMOIND12_&YR = catt(HMO_IND_01,HMO_IND_02,HMO_IND_03,HMO_IND_04,HMO_IND_05,HMO_IND_06,HMO_IND_07,HMO_IND_08,HMO_IND_09,HMO_IND_10,HMO_IND_11,HMO_IND_12);
		BUYIN12_&YR =catt(MDCR_ENTLMT_BUYIN_IND_01,MDCR_ENTLMT_BUYIN_IND_02,MDCR_ENTLMT_BUYIN_IND_03,MDCR_ENTLMT_BUYIN_IND_04,MDCR_ENTLMT_BUYIN_IND_05,
						MDCR_ENTLMT_BUYIN_IND_06,MDCR_ENTLMT_BUYIN_IND_07,MDCR_ENTLMT_BUYIN_IND_08,MDCR_ENTLMT_BUYIN_IND_09,MDCR_ENTLMT_BUYIN_IND_10,
						MDCR_ENTLMT_BUYIN_IND_11,MDCR_ENTLMT_BUYIN_IND_12);
		PROC SORT; BY BID_HRS_22 HMOIND12_&YR; RUN;
		PROC SORT DATA=A OUT=DN&YR nodupkey; BY BID_HRS_22; RUN;
		%MEND HMOIND1;
	%HMOIND1(mdcr.mbsf_2013,13)
	%HMOIND1(mdcr.mbsf_2014,14)
	%HMOIND1(mdcr.mbsf_2014,15)

	DATA cmsxref (keep=BID_HRS_22 HHIDPN); /*xwalk to HRS */
	set mdcr.XREF2015Medicare (keep=BID_HRS_22  HHIDPN rename=(HHIDPN=HHIDPN_C));
	HHIDPN=input(HHIDPN_C,9.0);
	proc sort; by BID_HRS_22; run;

	DATA DN1015; MERGE DN10-DN15; BY BID_HRS_22; PROC SORT; BY BID_HRS_22;  run;
	PROC SORT data=cmsxref; by BID_HRS_22; run;
	DATA DN1015_1; MERGE DN1015 (in=A) cmsxref (in=B); by BID_HRS_22; if A & B; run;
	PROC SORT DATA=DN1015_1; by HHIDPN; run;

	DATA FFS_01; 
	length FFS_48M  $48 FFS_72M $72; 
	set DN1015_1; 
	FFS_48M = catt(HMOIND12_10, HMOIND12_11, HMOIND12_12, HMOIND12_13);
	FFS_72M = catt(HMOIND12_10, HMOIND12_11, HMOIND12_12, HMOIND12_13, HMOIND12_14, HMOIND12_15);
	run; 
	proc sort data=FFS_01; by HHIDPn; run; 

		proc freq data=cohort_2012_0; tables Mo_to_2012; run; 
		proc freq data=cohort_2012_0; tables Mo_to_2014; run; 

	data cohort_2012_1; 
	length FFS_to_12 $48 FFS_to_14 $90; 
	merge cohort_2012_0 (in=A) FFS_01 (keep=HHIDPN FFS_48M FFS_72M); 
	by HHIDPN; 
	if A; 

	FFS_to_12 = substr(FFS_48M, (YR_2010-2010)*12 + MO_2010, Mo_to_2012); 
	if Mo_to_2014~=. then FFS_to_14 = substr(FFS_72M, (YR_2010-2010)*12 + MO_2010, Mo_to_2014); 
	run;
	data cohort_2012_2; 
	set cohort_2012_1; 
	FFS_12 = verify(FFS_to_12, "0"); 
	FFS_14 = verify(FFS_to_14, "0"); 
	
	if MO_2014~=. & YR_2014~=. & FFS_14>Mo_to_2014; /* Those who have FFS all the way*/

	run; 



/***************************************/
/*06/03/2020 Looking at SVI variables. */
/***************************************/

/*We'd like to see the descriptive first to see (1) missing values (2) distribution of the categorical variables */

proc contents data=lb.HD10f5e; run;

/* (i) Extracting psychosocial variables extraction from core interview 2010 */
proc import datafile="V:\Health and Retirement Study\Sun\Sachin\psychosocial\spreadsheet\LBS RAND core variables 2010_2012.csv" 
dbms=csv out=sv.var_list replace; guessingrows=12341; run;
proc sql;
  select (strip(var10)) into: lbs_var_2010 separated by ' ' 
  from sv.var_list replace
  where source="LBS" and var10~= "   " ;
quit;

%put &lbs_var_2010;
data lb_2010; set lb.HD10f5e (keep=HHIDPN &lbs_var_2010); run;
proc sort data=lb_2010; by HHIDPN; run; 

/* (ii) Extracting sychosocial variables from RAND 2010 */
proc sql;
  select (strip(var10)) into: rand_var_2010 separated by ' ' 
  from sv.var_list replace
  where source="RAND" and var10~= "   " ;
quit;

%put &rand_var_2010;
data rand_2010; set randp.Randhrs1992_2016v1 (keep=HHIDPN &rand_var_2010); run;
proc sort data=rand_2010; by HHIDPN; run;

/* (iii) Merging all variables to the data fiel */
data cohort_2010 (drop= complete LB_year HHIDPN); 
merge cohort_1 (in=A) lb_2010  rand_2010; 
by HHIDPN; 
if A; 
run; 

/* (iv) Simple descriptive stats*/
ods excel file="V:\Health and Retirement Study\Sun\Sachin\psychosocial\spreadsheet\desc_2010.xlsx"
options(Sheet_Interval='none');
proc freq data=cohort_2010; tables _all_/missing out=desc_2010 ;run; 
ods excel close; 



