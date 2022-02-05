

/**  This code develop Charlson index using Medicare claims (inpatient, outpatient, carrier)*/

libname mdcr 'V:\Health and Retirement Study\DATAinSAS2018\HRS_Provider_Distribution HRS018 - NCIRE 51424';
libname trk 'V:\Health and Retirement Study\Grisell\HRS_data\trk2018earlyv2a'; 
libname doi 'V:\Health and Retirement Study\DATAinSAS\doi_gdr';
libname dod 'V:\Health and Retirement Study\DATAinSAS\dod_gdr';
libname randp 'V:\Health and Retirement Study\Grisell\HRS_data\rand\randhrs1992_2016v2';
proc format cntlin=randp.sasfmts;run;
libname fat 'V:\Health and Retirement Study\Grisell\HRS_data\rand\fatfiles';
libname sv 'V:\Health and Retirement Study\Sun\Sachin\psychosocial\data_interim'; 
libname xwalk 'V:\Health and Retirement Study\DATAinSAS2020\xref\Data\Medicare\Xref2015\sas\'; 

data ip  (keep=BID_HRS_22 Claim_ID_HRS_22 DGNSCD01-DGNSCD25 ADMSN_DT DSCHRGDT);
	set mdcr.IP_2011-mdcr.IP_2013;
	run; 
data IP_charlson(keep=bid_hrs_22 adm_dt discharge_dt DGNSCD01-DGNSCD25);
	set ip;
	adm_dt=ADMSN_DT; 
	discharge_dt = DSCHRGDT; 
	format adm_dt discharge_dt mmddyy10.;
run;

data op  (keep=BID_HRS_22 DGNSCD01-DGNSCD25 from_dt thru_dt);
	set mdcr.OP_2011-mdcr.OP_2013;
	run; 
data OP_charlson(keep=bid_hrs_22 adm_dt discharge_dt DGNSCD01-DGNSCD25);
	set op;
	adm_dt=from_dt;
	discharge_dt=thru_dt;
	format adm_dt discharge_dt mmddyy10.;
run; 
data cr  (keep=BID_HRS_22 DGNSCD01-DGNSCD13 from_dt thru_dt);
	set mdcr.PB_2011-mdcr.PB_2013;
	run; 

data Car_charlson(keep=bid_hrs_22 adm_dt discharge_dt DGNSCD01-DGNSCD13);
	set cr;	
	adm_dt=from_dt;
	discharge_dt=thru_dt;
	format adm_dt discharge_dt mmddyy10.;
run;

data data_charlson;
	set IP_charlson
		OP_charlson
		Car_charlson;
run;

proc sql;
	create table data_charlson1 as 
	select A.hhidpn, B.*
	from sub2012 A inner join data_charlson B on A.bid_hrs_22=B.bid_hrs_22 and (A.NIWDATE-365)<=B.adm_dt<=(A.NIWDATE)
	order by a.hhidpn, b.adm_dt;
quit;


data data_charlson2;
	set data_charlson1;
	by hhidpn;
	array CONDITIONS (17) ANYMI CHF VASCUL1 CVD PULMON1 DEMENTIA PARALYS DIABET1 DIABET3 RENAL1 LIVER1 LIVER2 ANYULCER RHEUM AIDS MALIGNANCY METASTATIC;
	do i= 1 to 17;
	if FIRST.hhidpn then CONDITIONS(i)=0;
	retain CONDITIONS;
	end;
	array dxcode DGNSCD01-DGNSCD25;
	do over dxcode;
	dx_3=substr(dxcode,1,3);
	dx_4=substr(dxcode,1,4);
	/********** MYOCARDIAL INFARCTION WEIGHT=1 ****************/
	if dx_3='410' then do; ACUTEMI=1; ANYMI=1;end;
	if dx_3='412' then do; OLDMI=1; ANYMI=1; end;
	if ACUTEMI=. then ACUTEMI=0;
	if ANYMI=. then ANYMI=0;
	if OLDMI=. then OLDMI=0;
	/********** CHF ***** WEIGHT=1 ****************************/
	if dx_3='428' then CHF=1;
	if CHF=. then CHF=0;
	/*********** PERIPHERAL VASCULAR DISEASE ******* WEIGHT=1**/
	if dx_3='441'|dx_4 in('4439','7854','V434','v434') then VASCUL1=1;
	if VASCUL1=. then VASCUL1=0;
	/********* CEREBROVASCULAR DISEASE ******* WEIGHT=1 *******/
	if '430'<=dx_3<='437'|dxcode='438 ' then CVD=1;
	if CVD=. then CVD=0;
	/*********** COPD *********************** WEIGHT=1 ********/
	if '490'<=dx_3<='496'|'500'<=dx_3<='505'|dx_4='5064' then PULMON1=1;
	if PULMON1=. then PULMON1=0;
	/******** DEMENTIA ****** WEIGHT=1 ***********************/
	if dx_3='290' then DEMENTIA=1;
	if DEMENTIA=. then DEMENTIA=0;
	/********* PARALYSIS **************** WEIGHT=2 ************/
	if dx_3='342'|dx_4='3441' then PARALYS=1;
	if PARALYS=. then PARALYS=0;
	/******** DIABETES ************* WEIGHT=1 *****************/
	if dxcode='250 '|dx_4='2507'|'2500'<=dx_4<='2503' then DIABET1=1;
	if DIABET1=. then DIABET1=0;
	/********* DIABETES WITH SEQUELAE ****** WEIGHT=2 *********/
	if ('2504'<=dx_4<='2506') then DIABET3=1;
	if DIABET3=. then DIABET3=0;
	/********* CHRONIC RENAL FAILURE ******* WEIGHT=2 *********/
	if dx_3 in('582','585','586','588')| ('5830'<=dx_4<='5837') then RENAL1=1;
	if RENAL1=. then RENAL1=0;
	/************** VARIOUS CIRRHODITES ******** WEIGHT=1 *****/
	if dx_4 in('5712','5714','5715','5716') then LIVER1=1;
	if LIVER1=. then LIVER1=0;
	/************** MODERATE-SEVERE LIVER DISEASE *** WEIGHT=3*/
	if ('5722'<=dx_4<='5728')|('4560'<=dx_4<='4561')|dxcode in('4562 ','45620','45621') then LIVER2=1;
	if LIVER2=. then LIVER2=0;
	/*************** ULCERS ********** WEIGHT=1 ***************/
	if '5310'<=dx_4<='5313'|'5320'<=dx_4<='5323'|'5330'<=dx_4<='5333'|'5340'<=dx_4<='5343'|
	dx_4 in('531 ','5319','532 ','5329','533 ','5339','534 ','5349') then do; ULCER1=1; ANYULCER=1; end;
	if '5314'<=dx_4<='5317'|'5324'<=dx_4<='5327'|'5334'<=dx_4<='5337'|'5344'<=dx_4<='5347' then do;
	ULCER2=1; ANYULCER=1; end;
	if ULCER1=. then ULCER1=0;
	if ANYULCER=. then ANYULCER=0;
	if ULCER2=. then ULCER2=0;
	if ANYULCER=. then ANYULCER=0;
	/*************** RHEUM ********** WEIGHT=1 ***************/
	if dxcode in('71481','725 ','7100 ','7101 ','7104 ')|'7140'<=dx_4<='7142' then RHEUM=1;
	if RHEUM=. then RHEUM=0;
	/*************** AIDS ********** WEIGHT=6 ***************/
	if '042'<=dx_3<='044' then AIDS=1;
	if AIDS=. then AIDS=0;
	/*************** ANY MALIGNANCY ** WEIGHT=2 **************/
	if ('140'<=dxcode<='1729')| ('174'<=dxcode<='1958')|('200'<=dxcode<='2089') then MALIGNANCY=1;
	if MALIGNANCY=. then MALIGNANCY=0;
	/*************** METASTATIC SOLID TUMOR ********** WEIGHT=6 *****/
	if '196'<=dxcode<='1991' then METASTATIC=1; 
	end;
	if METASTATIC=. then METASTATIC=0;
	if LAST.hhidpn;
	drop i dx_3 dx_4;
proc sort ;
by hhidpn;
run;

data data_charlson3;
	set data_charlson2;
	Charlson=(ANYMI * 1) +
	(CHF * 1) +
	(VASCUL1 * 1) +
	(CVD * 1) +
	(PULMON1 * 1) +
/*	(DEMENTIA * 1) +*/
	(PARALYS * 2) +
	(DIABET1 * 1) +
	(DIABET3 * 2) +
	(RENAL1 * 2) +
	(LIVER1 * 1) +
	(LIVER2 * 3) +
	(ANYULCER * 1) +
	(RHEUM * 1) +
	(AIDS * 6) +
	(MALIGNANCY * 2) +
	(METASTATIC * 6);
run;

