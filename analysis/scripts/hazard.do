version 14
clear all

* general code:
* stset time, failure(event)
* here: time = duration, event = detected
* or event = death. death = 0 if enddate = 1000 (last date in time). else, death = 1 
 
****************************** Directory path *******************************************
* SET DIRECTORY HERE
cd ~/switchdrive/Research_gues_bell/Code/simulate_collusion/analysis

* pwd // show working directory
*****************************************************************************************
* Model I, No detection, event = death *
* Only Population *
clear all
insheet using "data/duration_no_enforcement.csv", clear delimiter(;)
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
* Estimate the hazard rate
streg n_firms, distribution(weibull)


* Model II, Constant detection probability (structured=0), event = death *
* Population *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
browse
keep if structured == 0
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
* Estimate the hazard rate
streg n_firms len_reduction, distribution(weibull) // fine reduction if applying for leniency: 1 - no fines. 0 - full fines. 
// leniency = 1 makes leaving and cheating more attractive than staying in cartel
// large n_firms increase ICC_entry and ICC_exit. 
// Large len_reduction increase ICC_exit and shortens duration.
//streg n_firms theta_len, distribution(weibull)

* Estimate not the hazard rate, but the coefficients. e^coeff = hazard ratio
streg n_firms len_reduction, distribution(weibull) nohr
* predict mean time = predict mean survival time = mean duration = 1/lambda
predict lambda_min_1, mean time  // this prediction is not reliable. it varies with different sorting of rows. do not use!
dis lambda_min_1



* Sample
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 0 & detected == 1
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)
predict lambda_min_1, mean time  
dis lambda_min_1

* undetected cases (population - sample)
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 0 & detected == 0
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)




* Model III, Increasing detection probability (structured=1), event = death *
* Population *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)
streg n_firms len_reduction, distribution(weibull) nohr
predict lambda_min_1, mean time  
dis lambda_min_1


* Sample
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1 & detected == 1
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)
predict lambda_min_1, mean time  
dis lambda_min_1

* undetected cases (population - sample)
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1 & detected == 0
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)



*******************************************************************************
*** Separate into Small and Large Number of Firms ***

* Model I, No detection, event = death *
* Only Population *
clear all
insheet using "data/duration_no_enforcement.csv", clear delimiter(;)
keep if n_firms < 4
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms, distribution(weibull)

clear all
insheet using "data/duration_no_enforcement.csv", clear delimiter(;)
keep if n_firms > 3
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms, distribution(weibull)


* Model II, Constant detection probability (structured=0), event = death *
* Sample
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 0 & detected == 1 & n_firms < 4
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 0 & detected == 1 & n_firms > 3
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

* undetected cases (population - sample)
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 0 & detected == 0 & n_firms < 4
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 0 & detected == 0 & n_firms > 3
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

* Population *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 0 & n_firms < 4
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 0 & n_firms > 3
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)


* Model III, Increasing detection probability (structured=1), event = death *
* Sample
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1 & detected == 1 & n_firms < 4
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1 & detected == 1 & n_firms > 3
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

* undetected cases (population - sample)
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1 & detected == 0 & n_firms < 4
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1 & detected == 0 & n_firms > 3
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

* Population *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1 & n_firms < 4
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)

clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if structured == 1 & n_firms > 3
gen death = 1
replace death = 0 if end == 1000
stset duration, failure(death)
streg n_firms len_reduction, distribution(weibull)






*** DEPRECATED ***

* Population *
* event = detected *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
browse
//keep if theta_len == 1 & structured == 0
keep if theta_len == 1 & structured == 1
stset duration, failure(detected)
sts graph, na

* Variable n_firms is significant if Pr>chi2 <= 0.2
sts test n_firms, logrank

* Are the survival curves separate for different number of firms in industry?
sts graph, by(n_firms) 

* Estimate the hazard rate
streg n_firms, distribution(weibull)

* Estimate not the hazard rate, but the coefficients
streg n_firms, distribution(weibull) nohr

predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1


* Sample, all theta *
* event = detected. This is equal to event = death, because all cartels die if they get detected, and the sample only contains detected cartels. *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
//keep if structured == 0 & detected == 1
keep if structured == 1 & detected == 1
stset duration, failure(detected)
//sts graph, na
* number of firms is significant:
//sts test n_firms, logrank
//sts graph, by(n_firms) 

* leniency is not significant:
sts test theta_len, logrank 
//sts graph, by(theta_len) 


* structured = 0: only include number of firms in regression
streg n_firms, distribution(weibull)
streg n_firms, distribution(weibull) nohr

* structured = 1: include number of firms and leniency in regression
streg n_firms theta_len, distribution(weibull)
streg n_firms theta_len, distribution(weibull) nohr

predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1
* structured = 0: HR: 1.06, mean duration: 173,6
* structured = 1: HR: n_firms 0.91, theta 1.03, mean duration: 119.75


* Population, all theta *
* event = detected *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
//browse
//keep if structured == 0
keep if structured == 1
stset duration, failure(detected)
//sts graph, na
sts test theta_len, logrank
//sts graph, by(theta_len) 
* structured = 0: leniency is not significant
streg n_firms, distribution(weibull)
streg n_firms, distribution(weibull) nohr
predict lambda_min_1, mean time  
dis lambda_min_1
* HR: 0.91, mean duration: 218,5

* structured = 1: leniency is significant Pr>chi2 = 0.0001
streg n_firms theta_len, distribution(weibull)
streg n_firms theta_len, distribution(weibull) nohr
predict lambda_min_1, mean time  
dis lambda_min_1
* HR: n_firms 0.79, theta 1.07, mean duration: 133.79


* Population *
* event = death *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
//keep if structured == 0
keep if structured == 1
gen death = 1
//replace death = 0 if end == 1000
stset duration, failure(death)
//sts graph, na
//sts test theta_len, logrank
//sts graph, by(theta_len) 
* leniency is significant
streg n_firms, distribution(weibull)
//streg n_firms theta_len, distribution(weibull)
//streg n_firms theta_len, distribution(weibull) nohr
predict lambda_min_1, mean time  
dis lambda_min_1

* HR n-firms: 1.42, HR theta: 0.696, mean duration: 483.8

* Population *
* event = detection *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
browse
keep if theta_len == 1 & structured == 0
stset duration, failure(detected)
sts graph, na

* Variable n_firms is significant if Pr>chi2 <= 0.2
sts test n_firms, logrank

sts graph, by(n_firms) 


* Undetected Cases *
* event = death *
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
//keep if structured == 0 & detected == 0
keep if structured == 1 & detected == 0
gen death = 1
//replace death = 0 if end == 1000
//keep if structured == 0 & detected == 0 & death == 0
stset duration, failure(death)
//sts graph, na
//sts test theta_len, logrank
//sts graph, by(theta_len) 
* leniency is significant

streg n_firms, distribution(weibull)
//streg n_firms theta_len, distribution(weibull)
//streg n_firms theta_len, distribution(weibull) nohr
predict lambda_min_1, mean time  
dis lambda_min_1
browse
egen maxdur = max(duration)


//** theta = probability of being born | not being alive
//** 1/theta = meanBS = Mean Births = mean interarrival time in days
clear all
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
keep if theta_len == 1
keep if detected == 1 & structured == 0
stset start
sts gen hazard_c = h
streg n_firms, distribution(weibull), if detected==1
predict theta_min_1, mean time 
estpost su theta_min_1
dis theta_min_1



*****************************************************************************************
* DEPRECATED *
*****************************************************************************************

//*******************************************************************************
//** unstructured
//** lambda = probability of dying | being alive on a give day
//** 1/lambda = mean duration in days

// replace exit with detected

//** SAMPLE
insheet using "data/duration_enforcement_all.csv", clear delimiter(;)
gen rightCensor = 0
replace rightCensor = 1 if (detected == 0)  // not found
gen exit = 1 - rightCensor

browse

stset duration, failure(exit)
//sts graph, survival
sts gen hazard_a = h
//sts graph, hazard_d title("Smoothed Hazard estimate, Unstructured") saving(112_hazard_unstruc, replace)
streg n_firms, distribution(weibull), if detected==1
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
estpost summarize lambda_min_1
esttab using "sample_hazard_unstruc_death.csv", replace plain cells("mean sd count") noobs

stset duration_year, failure(exit)
sts gen hazard_b = h
streg n_firms, distribution(weibull), if detected==1
predict lambda_min_1_years, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
estpost summarize lambda_min_1_years
esttab using "sample_hazard_unstruc_death_years.csv", replace plain cells("mean sd count") noobs

//** theta = probability of being born | not being alive
//** 1/theta = meanBS = Mean Births = mean interarrival time in days
stset start
sts gen hazard_c = h
streg n_firms, distribution(weibull), if detected==1
predict theta_min_1, mean time 
estpost su theta_min_1
esttab using "sample_hazard_unstruc_birth.csv", replace plain cells("mean sd count") noobs


//** theta = probability of being born | not being alive
//** 1/theta = meanBS = Mean Births = mean interarrival time in years
stset startyear
sts gen hazard_d = h
streg n_firms, distribution(weibull), if detected==1
predict theta_min_1_years, mean time 
estpost su theta_min_1_years
esttab using "sample_hazard_unstruc_birth_years.csv", replace plain cells("mean sd count") noobs


//** SAMPLE
clear all
insheet using "pop_duration.csv", clear delimiter(;)
browse
keep if theta_len == 1
keep if detected == 1 & structured == 0
stset duration, failure(detected)
streg n_firms, distribution(weibull)
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1
estpost summarize lambda_min_1
esttab using "firms_sample_hazard_unstruc_death.csv", replace plain cells("mean sd count") noobs

//** UNDETECTED
clear all
insheet using "pop_duration.csv", clear delimiter(;)
browse
keep if theta_len == 1
keep if detected == 0 & structured == 0
gen exit = 1
stset duration, failure(exit)
streg n_firms, distribution(weibull)
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1

//** POPULATION
clear all
insheet using "pop_duration.csv", clear delimiter(;)
browse
keep if theta_len == 1
keep if structured == 0
gen exit = 1
stset duration, failure(exit)
streg n_firms, distribution(weibull)
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1


cor detected n_firms


clear all
insheet using "pop_duration.csv", clear delimiter(;)
browse
keep if theta_len == 1
keep if structured == 0
gen exit = 1
stset duration, failure(exit)
streg n_firms, distribution(weibull)
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1
estpost summarize lambda_min_1
esttab using "firms_pop_hazard_unstruc_death.csv", replace plain cells("mean sd count") noobs

clear all
insheet using "pop_duration.csv", clear delimiter(;)
browse
keep if theta_len == 1
keep if detected == 1 & structured == 1
stset duration, failure(detected)
streg n_firms, distribution(weibull)
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1
estpost summarize lambda_min_1
esttab using "firms_sample_hazard_struc_death.csv", replace plain cells("mean sd count") noobs

clear all
insheet using "pop_duration.csv", clear delimiter(;)
browse
keep if theta_len == 1
keep if structured == 1
gen exit = 1
stset duration, failure(exit)
streg n_firms, distribution(exponential)
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1
estpost summarize lambda_min_1
esttab using "firms_pop_hazard_struc_death.csv", replace plain cells("mean sd count") noobs


stset start
streg n_firms, distribution(exponential)
predict theta_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis theta_min_1
estpost summarize theta_min_1
esttab using "firms_pop_hazard_struc_birth.csv", replace plain cells("mean sd count") noobs


stset duration, failure(detected)
//sts graph, survival
//sts gen hazard_e = h
//sts graph, h
//sts graph, haz
//sts graph, cumhaz
//sts graph, hazard_e title("Smoothed Hazard estimate, Unstructured") saving(112_hazard_unstruc, replace)
//streg cartel industry, distribution(exponential)
streg n_firms, distribution(weibull)
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
dis lambda_min_1
estpost summarize lambda_min_1
esttab using "sample_hazard_struc_death.csv", replace plain cells("mean sd count") noobs
esttab using "pop_hazard_struc_death.csv", replace plain cells("mean sd count") noobs

stset duration_year, failure(detected)
//sts gen hazard_f = h
streg n_firms, distribution(exponential)
predict lambda_min_1_years, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
estpost summarize lambda_min_1_years
esttab using "pop_hazard_unstruc_death_years.csv", replace plain cells("mean sd count") noobs

//** theta = probability of being born | not being alive
//** 1/theta = meanBS = Mean Births = mean interarrival time in days
stset start
sts gen hazard_g = h
streg n_firms, distribution(weibull)
predict theta_min_1, mean time 
estpost su theta_min_1
esttab using "pop_hazard_unstruc_birth.csv", replace plain cells("mean sd count") noobs


//** theta = probability of being born | not being alive
//** 1/theta = meanBS = Mean Births = mean interarrival time in years
stset startyear
sts gen hazard_h = h
streg n_firms, distribution(weibull)
predict theta_min_1_years, mean time 
estpost su theta_min_1_years
esttab using "pop_hazard_unstruc_birth_years.csv", replace plain cells("mean sd count") noobs
