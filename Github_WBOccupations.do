***********************************************************************************************************************************
*************************************************What Works Centre for Wellbeing ***************************************************
*******Analysis of wellbeing by occupation using Annual Population Survey(APS)and Annual Survey of Hours and Earnings(ASHE)********
***************************************************Simona Tenaglia - February  2023***************************************************
************************************************************************************************************************************
**Version 1.0  - Stata 14

**This do file run an analysis of wellbeing by occuaption over the period April 2012- April 2022. It looks at wellbeing by occupation, 
**its changes over time and how it has been affected by the pandemic, exploiting the APS  and ASHE data
**It looks also for determinants of wellbeing within occupations, by running OLS regression by using several socio demographic explanatory variables,
** an occupation variable and the gross week pay. 
**It produces the outputs and analysis used in the report "Wellbeing by occupations in UK during  2012-2022" Published by WWC for Wellbeing in February 2023.

**********************************************************DATA********************************************

**APS data can be downladed from UK Data Service (https://ukdataservice.ac.uk/). The following datasets are used for the analysis:

** 7364	Annual Population Survey: Subjective Well-Being, April 2012 - March 2013
** 7565	Annual Population Survey: Subjective Well-Being, April 2013 - March 2014
** 7806	Annual Population Survey: Personal Well-Being, April 2014 - March 2015
** 8003	Annual Population Survey, April 2015 - March 2016	
** 8197	Annual Population Survey, April 2016 - March 2017	
** 8356	Annual Population Survey, April 2017 - March 2018	
** 8510	Annual Population Survey, April 2018 - March 2019	
** 8647	Annual Population Survey, April 2019 - March 2020	
** 8833	Annual Population Survey, April 2020 - March 2021
** 8991 Annual Population Survey, April 2021 - March 2022
**ASHE data for 2020 are the "2020 revised edition of the dataset" downloaded from the ONS website: 
**(https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/occupation4digitsoc2010ashetable14)


**Set working directory
 cd 
set more off, perm     /// not to pause or display a -more- message. The option perm means the -more- setting is remembered and become the default setting.

** For all years we upload the datasets with the command use. After creating an indicator for each year and put the variables'name in lower case, we save the datasets with different names ( APS2012-2013, APS22013-2014 etc.) 

**Generate an Indicator for each year, put variables' name in lower case and save**
**2012-2013 Data**
use "a12m13_wellbeing_eul.dta"
gen period = 2012
rename *, lower
save "APS2012-2013.dta", replace

**2013-2014 Data**
use "a13m14_wellbeing_eul.dta"
gen year =2013
rename *, lower
**Save
save "APS2013-2014.dta", replace
 
**2014-2015 data**
use "a14m15_wellbeing_eul.dta"
gen year =2014
rename *, lower
**Save
save "APS2014-2015.dta", replace
 
**2015-2016 data**
use "apsp_a15m_eul.dta"
gen year =2015
rename *, lower
**Save
save "APS2015-2016.dta", replace

**2016-2017 data**
use "apsp_a16m17_eul.dta"
gen year =2016
rename *, lower
**Save
save "APS2016-2017.dta", replace

**2017-2018 data**
use "apsp_apr17mar18_eul.dta"
gen year =2017
rename *, lower
**Save
save "APS2017-2018.dta", replace 

**2018-2019 data**
use "apsp_a18m19_eul_pwta18.dta"
gen year =2018
**Save
save "APS2018-2019.dta", replace

**2019-2020 data**
use "apsp_a19m20_eul_pwta20.dta"
gen year =2019
rename *, lower
**Save
save "APS2019-2020.dta", replace

**2020-2021 data**
use "apsp_a20m21_eul_pwta20.dta"
gen year =2020
rename *, lower
**Save
save "APS2020-2021.dta", replace

**2021-2022 data**
use "apsp_a21m22_eul_pwta22.dta"
gen year =2021
rename *, lower
**Save
save "APS2021-2022.dta", replace
      
 
**Appending
**This code creates one dataset with all years 2012-2022 appending each dataset in turn 
**Append all data together 
clear
use "APS2012-2013.dta", clear
append using "APS2013-2014.dta"
append using "APS2014-2015.dta"
append using "APS2015-2016.dta"
append using  "APS2016-2017.dta"
append using "APS2017-2018.dta"
append using  "APS2018-2019.dta"
append using "APS2019-2020.dta"
append using  "APS2020-2021.dta"
append using  "APS2021-2022.dta"

****************************Structuring variables for analysis***********************************
**Rename wellebing variables
ren satis Satisfaction
ren worth Worthwhileness
ren happy Happiness
ren anxious Anxiety

**Create unique variable for  wellbeing weights
replace np122r11=np132r11 if year==2013
replace np122r11=np142r14 if year==2014
replace np122r11=npwt18 if year==2015
replace np122r11=npwt18 if year==2016
replace np122r11=npwt18 if year==2017
replace np122r11=npwt18 if year==2018
replace np122r11=npwt20 if year==2019
replace np122r11=npwt20 if year==2020
replace np122r11=npwt22 if year==2021

**Keeping positive values for the dependent variables. 
replace Satisfaction =. if Satisfaction<0
replace Happiness =. if Happiness<0
replace Worthwhileness =. if Worthwhileness<0
replace Anxiety =. if Anxiety<0

**Replace negative values to run regressions  
replace sc10mmj = . if sc10mmj ==-8
replace sc10mmj = 10 if sc10mmj==-9      ///we are keeping values corresponding to the category "Does not apply" to preserve observations on unemployed
replace sc20mmj = . if sc20mmj ==-8
replace sc20mmj = 10 if sc20mmj == -9    ///we are keeping values corresponding to the category "Does not apply" to preserve observations on unemployed

**Create unique variable for job classification
replace sc10mmj=sc20mmj if year==2021

**Create new label for occupations
#delimit ;
label define sc10mmj_label
	1 "Managers and  Directors " 
	2 "ProfessionalOccupations" 
	3 "Associate Professional,Technical Occupations" 
	4 "Administrative,Secretarial Occupations" 
	5 "Skilled Trades Occupations"
	6 "Caring, Leisure,other Service Occupations" 
	7 "Sales And Customer Service Occupations" 
	8 "Process, Plant And Machine Operatives" 
	9 "Elementary Occupations"
	10 "Does not apply";
#delimit cr
label values sc10mmj sc10mmj_label 

**Create unique variable for age, create variable age squared
replace dvage = age if dvage ==.
ren dvage aget
gen agesq=aget^2

**Generate dummy and label for sex where  male=0 female =1 
gen sex_d = 0
replace sex_d = 1 if sex==2
#delimit ;
label define sex_d_label
		0		"Male"
		1		"Female";
#delimit cr
label values sex_d sex_d_label

**Generate variable and label for ethnicity
 gen ethnicity=ethukeul
 replace ethnicity = . if ethukeul<0
 #delimit ;
 label define Ethnicity_label 
			1	"White" 
			2 	"Mixed" 
			3	"Indian" 
			4	"Pakistani" 
			5 	"Bangladeshi" 
			6 	"Chinese" 
			7 	"Other Asian" 
			8 	"Black/African/Caribbean/Black British "	
			9 	"Other ethnic groups";
#delimit cr
label values ethnicity Ethnicity_label

**Generate unique variable and labels for level of education and replace negative values with .
replace hiqul15d = . if hiqul15d==-8
replace hiqul15d = . if hiqul15d==-9
replace hiqul11d = . if hiqul11d==-8
replace hiqul11d = . if hiqul11d==-9 
replace hiqul11d = hiqul15d if hiqul11d ==.

#delimit ;
label define hiqul11d_label 
		1 	"Degree or equivalent" 
		2 	"Higher education" 
		3 	"GCE, A-level or equivalent" 
		4 	"GCSE grades A*-C or equivalent" 
		5 	"Other qualifications" 
		6 	"No qualification"
		7 	"Did not know"
		8 	"No answer"
		9   "Does not apply", replace; 
#delimit cr
label values hiqul11d hiqul11d_label

**Rename qualification level educ
ren hiqul11d educ

		
**Label for marital status
#delimit ;
label define marsta_label 
	-9 	"Does not apply" 
	-8 	"No answer" 
	1 	"Single, never married" 
	2 	"Married living toget" 
	3 	"Married separated " 
	4 	"Divorced" 
	5 	"Widowed" 
	6 	"Civil partnership now or before" 
	7 	"Separated Civil Partner" 
	8 	"Former Civil Partner, legally dissolved" 
	9 	"Surviving Civil Partner, partner died", replace; 
#delimit cr
label values marsta marsta_label

**Generate variable and labels for employment, unemployment, inactivity.   
gen empl=inecac05
replace empl=. if inecac05<0
replace empl=3 if inecac05>=6     ///We sum all types of inactivities
replace empl=4 if inecac05==5     /// We keepunemployed separate
replace empl=5 if inecac05==3
replace empl=6 if inecac05==4
#delimit ;
label define empl_label 
	1 	"employed" 
	2 	"self employed" 
	3 	"all inactive" 
	4	"unemployed" 
	5 	"gov empl and training progr" 
	6 	"unpaid family worker";
#delimit cr
label values empl empl_label

** create labels for year
#delimit ;
label define year_label 
	2012 	"2012-2013" 
	2013 	"2013-2014" 
	2014	"2014-2015" 
	2015	"2015-2016" 
	2016	"2016-2017" 
	2017 	"2017-2018"
	2018    "2018-2019"
	2019    "2019-2020"
	2020    "2020-2021"
	2021    "2021-2022";
#delimit cr
label values year year_label

**Replace negative values for grossweek pay. 
replace grsswk=. if grsswk<0              /// we consider as missing values -8 = "no answer" an -9 = "Does not apply"


**Create variables for High and Low wellbeing
foreach x of varlist Satisfaction Happiness Worthwhileness {
gen High_`x' = `x'>=9
gen Low_`x' = `x'<=4
replace High_`x' = . if `x'==.
replace Low_`x' = . if `x'==.
}

foreach x of varlist Anxiety{
gen High_`x' = `x'>=6
gen Low_`x' = `x'<=1
replace High_`x' = . if `x'==.
replace Low_`x' = . if `x'==.
}

**********************************Analysis*************************************
**************************Descriptives and densities***************************

** N. observations by ocupations - raw number
tab sc10mmj year if sc10mmj!=10					/// raw numbers here do not considers the category "Does not apply"

**Plot densities for wellbeing variables
 twoway (histogram Satisfaction if sc10mmj!=10), xlabel(, labsize(small)) by(, subtitle(, fcolor(white) lcolor(white))) by(, graphregion(fcolor(white) ifcolor(white))) by(sc10mmj)
 twoway (histogram Anxiety if sc10mmj!=10), xlabel(, labsize(small)) by(, subtitle(, fcolor(white) lcolor(white))) by(, graphregion(fcolor(white) ifcolor(white))) by(sc10mmj)
 twoway (histogram Happiness if sc10mmj!=10), xlabel(, labsize(small)) by(, subtitle(, fcolor(white) lcolor(white))) by(, graphregion(fcolor(white) ifcolor(white))) by(sc10mmj)
 twoway (histogram Worthwhileness if sc10mmj!=10), xlabel(, labsize(small)) by(, subtitle(, fcolor(white) lcolor(white))) by(, graphregion(fcolor(white) ifcolor(white))) by(sc10mmj)

 **** Create set of binary variables for occupations (sc10mmj) to run ksmirnov test  to check equalities of distribution 
tabulate (sc10mmj), gen (occ_dummy)

ksmirnov Satisfaction, by(occ_dummy3) 
ksmirnov Satisfaction, by(occ_dummy4)
ksmirnov Satisfaction, by(occ_dummy5)
ksmirnov Satisfaction, by(occ_dummy6) 
ksmirnov Satisfaction, by(occ_dummy7)
ksmirnov Satisfaction, by(occ_dummy8)
ksmirnov Satisfaction, by(occ_dummy9) 
 

** mean wellbeing by year and place where work is mainly carried out
preserve
collapse  Satisfaction Happiness Worthwhileness Anxiety [aweight = np122r11], by(year home)

**graph mean life satisfaction by places where work is mainly carried out over 2012-2022
#delimit ;
twoway (line Satisfaction year if home==1) (line Satisfaction year if home ==2) (line Satisfaction year if home==3) 
(line Satisfaction year if home==4), title(Mean Life Satisfaction by places where interviewed mainly work) 
legend(order(1 "Own home" 2 "Same grounds or building as home" 3 "In different places using home as a base" 4 "Somewhere quite separate from home" ))graphregion(color(white))
save "Satisfaction_workplace.png", replace;
#delimit cr
restore

 **frequencies type of job, by occupation
tab  sc10mmj jobtyp if jobtyp>0 & sc10mmj!=10, row

**graph Life satisfaction and Anxiety by type of contract and occupations
graph set window fontface "Arial"
graph hbar (mean) Satisfaction [aweight = np122r11] if jobtyp>0 & sc10mmj!=10, over(jobtyp) over(sc10mmj) graphregion(color(white)) asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))
save "Satisfaction_typeoccupation.png", replace
graph hbar (mean) Anxiety [aweight = np122r11] if  jobtyp>0 & sc10mmj!=10, over(jobtyp) over(sc10mmj) graphregion(color(white)) asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66"))
save "Anxiety_typeoccupation.png", replace

****************************Analysis  across occupations****************************	
**Calculate mean for wellbeing variables
foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety {
mean `x' [aweight = np122r11], over(sc10mmj)
}

**Graph for mean values Wellbeing variables
graph hbar (mean) Satisfaction Happiness Worthwhileness Anxiety[aweight = np122r11] if sc10mmj!=10, graphregion(color(white)) by(, legend(on)) legend(order(1 "Satisfaction"  2 " Happiness"  3 "Worthwhileness"  4 "Anxiety")) by(sc10mmj) subtitle(, size(small))asyvars bar (1, color("58 183 185")) bar(2, color("0 52 66")) bar (3, color("247 147 33"))
save "meanONS4_byoccupations.png", replace

**Calculate proportions of High and Low wellbeing
preserve
collapse High_Satisfaction High_Happiness High_Worthwhileness Low_Anxiety Low_Satisfaction Low_Happiness Low_Worthwhileness High_Anxiety, by(sc10mmj)

**Graphs of proportions
foreach x of varlist High_Satisfaction High_Happiness High_Worthwhileness High_Anxiety{
 graph hbar (mean) `x' if sc10mmj!=10, over(sc10mmj) graphregion(color(white)) ytitle(Proportion with `x') bar(1, color("058 183 185")) blabel(total, position(inside) format(%9.2g))
save "`x'.gph", replace
 }
 
foreach x of varlist Low_Satisfaction Low_Happiness Low_Worthwhileness Low_Anxiety{
 graph hbar (mean) `x' if sc10mmj!=10, over(sc10mmj) graphregion(color(white)) ytitle(Proportion with `x') ///
 bar(1, color("058 183 185")) blabel(total, position(inside) format(%9.2g))
 save "`x'.gph", replace
}

restore

*****************************Analysis over time************************************

**Mean values for wellbeing by occupations over 2012-2022
foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
mean `x' [aweight = np122r11], over( sc10mmj year)
 }

**Graphs wellbeing variables by occupations over 2012-2022
preserve
collapse  Satisfaction Happiness Worthwhileness Anxiety [aweight = np122r11], by(year sc10mmj) 

#delimit ;
twoway (line Satisfaction year if sc10mmj==1) (line Satisfaction year if sc10mmj==2) (line Satisfaction year if sc10mmj==3)
 (line Satisfaction year if sc10mmj==4) (line Satisfaction year if sc10mmj==5) (line Satisfaction year if sc10mmj==6) 
 (line Satisfaction year if sc10mmj==7) (line Satisfaction year if sc10mmj==8) (line Satisfaction year if sc10mmj==9), ytitle(Mean Satisfaction)
 ytitle(, size(small)) title(Mean Satisfaction by occupation over 2012-2022, size(medsmall)) graphregion(color(white))
 legend(order(1 "Managers, Directors" 2 "Professional Occupations" 3 "Associate Professional/Technical Occup" 4 "Administrative,Secretarial Occupations" 5 "Skilled Trades Occupations" 6 "Caring, Leisure,Other Service Occup" 7 "Sales, Customer Service Occupations" 8 "Process,Plan, Machine Operatives" 9 "Elementary Occupations")
 size(small) span) xlabel(#10, labsize(vsmall) valuelabel);

 #delimit ;
twoway (line Happiness year if sc10mmj==1) (line Happiness year if sc10mmj==2) (line Happiness year if sc10mmj==3)
 (line Happiness year if sc10mmj==4) (line Happiness year if sc10mmj==5) (line Happiness year if sc10mmj==6) 
 (line Happiness year if sc10mmj==7) (line Happiness year if sc10mmj==8) (line Happiness year if sc10mmj==9), ytitle(Mean Happiness)
 ytitle(, size(small)) title(Mean Happiness by occupation over 2012-2022, size(medsmall)) graphregion(color(white))
 legend(order(1 "Managers, Directors" 2 "Professional Occupations" 3 "Associate Professional,Technical Occ" 4 "Administrative,Secretarial Occ" 5 "Skilled Trades Occupations" 6 "Caring, Leisure,Other Service Occ" 7 "Sales, Customer Service Occupations" 8 "Process,Plan, Machine Operatives" 9 "Elementary Occupations")
 size(small) span) xlabel(#10, labsize(vsmall) valuelabel);

 #delimit ;
twoway (line Worthwhile year if sc10mmj==1) (line Worthwhileness year if sc10mmj==2) (line Worthwhileness year if sc10mmj==3)
 (line Worthwhileness year if sc10mmj==4) (line Worthwhileness year if sc10mmj==5) (line Worthwhileness year if sc10mmj==6) 
 (line Worthwhileness year if sc10mmj==7) (line Worthwhileness year if sc10mmj==8) (line Worthwhileness year if sc10mmj==9), ytitle(Mean Worthwhileness)
 ytitle(, size(small)) title(Mean Worthwhileness by occupation over 2012-2022, size(medsmall)) graphregion(color(white))
 legend(order(1 "Managers, Directors" 2 "Professional Occupations" 3 "Associate Professional/Technical Occup" 4 "Administrative,Secretarial Occupations" 5 "Skilled Trades Occupations" 6 "Caring, Leisure,Other Service Occup" 7 "Sales, Customer Service Occupations" 8 "Process,Plan, Machine Operatives" 9 "Elementary Occupations")
 size(small) span) xlabel(#10, labsize(vsmall) valuelabel);
 
 #delimit ;
twoway (line Anxiety year if sc10mmj==1) (line Anxiety year if sc10mmj==2) (line Anxiety year if sc10mmj==3)
 (line Anxiety year if sc10mmj==4) (line Anxiety year if sc10mmj==5) (line Anxiety year if sc10mmj==6) 
 (line Anxiety year if sc10mmj==7) (line Anxiety year if sc10mmj==8) (line Anxiety year if sc10mmj==9), ytitle(Mean Anxiety)
 ytitle(, size(small)) title(Mean Anxiety by occupation over 2012-2022, size(medsmall)) graphregion(color(white))
 legend(order(1 "Managers, Directors" 2 "Professional Occupations" 3 "Associate Professional/Technical Occup" 4 "Administrative,Secretarial Occupations" 5 "Skilled Trades Occupations" 6 "Caring, Leisure,Other Service Occup" 7 "Sales, Customer Service Occupations" 8 "Process,Plan, Machine Operatives" 9 "Elementary Occupations")
 size(small) span) xlabel(#10, labsize(vsmall) valuelabel);

restore

****High/low proportions in 2019, 2020 and 2021
**Calculate proportions for high/low scores answers for Life Satisfaction, Happiness, Worthwhileness and  Anxiety
preserve
collapse High_Satisfaction High_Happiness High_Worthwhileness Low_Anxiety Low_Satisfaction Low_Happiness Low_Worthwhileness High_Anxiety , by(sc10mmj year)

**Graphs of high/low scores  for 2019, 2020 and 2021
foreach x of varlist High_Satisfaction High_Happiness High_Worthwhileness Low_Anxiety {
graph hbar (mean) `x' if year>=2019 & sc10mmj!=10, over(year) graphregion(color(white)) ytitle(Proportion with `x') bar(1, color("058 183 185")) blabel(total, position(inside) format(%9.2g)) by(sc10mmj) subtitle(, size(small))
save "proportions `x' 2019-2021.gph", replace
 }

  foreach x of varlist Low_Satisfaction Low_Happiness Low_Worthwhileness High_Anxiety{
graph hbar (mean) `x' if year>=2019 & sc10mmj!=10, over(year) graphregion(color(white)) ytitle(Proportion with `x')  bar(1, color("058 183 185")) blabel(total, position(inside) format(%9.2g)) by(sc10mmj) subtitle(, size(small))
save "proportions `x' 2019-2021.gph", replace
}

**regression analysis with trend and 2020 as binary indicators (to take into account the pandemic)

**Generate binary period indicators for 2020 and 2021
gen t20 = year==2020
gen t21 = year==2021

**Create a trendline 
gen trend = year-2012 

**Run regressions in a loop, regression each of wellbeing questions on the trendline and the 2020 dummmy
foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
reg `x' trend t20 t21 , vce(robust)
 }
 
 foreach x of varlist High_Happiness High_Satisfaction High_Worthwhileness Low_Anxiety{
 reg `x' trend t20 t21, vce(robust)
 }
 
 foreach x of varlist Low_Happiness Low_Satisfaction Low_Worthwhileness High_Anxiety{
 reg `x' trend t20 t21, vce(robust)
 }
 
 
*******************************Regression analysis*********************************
**Regression analysis for each ONS4 wellbeing question on a loop. in particular, regression of outcome on sociodemographic variables, occupations and gross week pay

 ***regressions introducing explanatory variables***

foreach x of varlist  Satisfaction Happiness Worthwhileness Anxiety{
reg `x'  aget agesq, vce(robust)
}

 foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
reg `x'  aget agesq sex_d, vce(robust)
}

foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
reg `x' aget agesq sex_d i.ethnicity, vce(robust) 
}

foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
reg `x' aget agesq sex_d i.ethnicity ib6.educ, vce(robust) 
}
foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
reg `x'  aget agesq sex_d i.ethnicity ib6.educ i.marsta,  vce(robust)
}

foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
reg `x'  aget agesq sex_d i.ethnicity ib6.educ i.marsta i.empl,  vce(robust)
}

foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
reg `x'  aget agesq sex_d i.ethnicity ib6.educ i.marsta i.empl i.sc10mmj, vce(robust)
outreg2 using tables_mregr7.docx, excel  label 
}
** Regression keeping only employed respondents
preserve
keep if empl == 1 | empl == 5
 foreach x of varlist Satisfaction Happiness Worthwhileness Anxiety{
reg `x'  aget agesq sex_d i.ethnicity ib6.educ i.marsta i.empl i.sc10mmj grsswk, vce(robust)
}
restore
 
 