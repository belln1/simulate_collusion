version 14
clear all
// to do: replace 112_1 with variable seed + timefactor


 
****************************** Directory path *******************************************
* SET DIRECTORY HERE
cd ~/switchdrive/Research_gues_bell/Code/simulate_collusion

* pwd // show working directory
*****************************************************************************************
//** structured
//** lambda = probability of dying | being alive on a give day
//** 1/lambda = mean duration in days
//** (meanDS = Mean Deaths)
insheet using "112_12_duration_cartel_struc.csv", clear delimiter(;)
gen rightCensor = 0
replace rightCensor = 1 if (detected == 0)  // not found
gen exit = 1 - rightCensor

stset duration, failure(exit)
//sts graph, survival
sts gen hazard_d = h
//sts graph, hazard_d title("Smoothed Hazard estimate, Structured") saving(112_hazard_struc, replace)
streg n_firms, distribution(weibull), if detected==1
//predict meandS, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
predict lambda_min_1, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
estpost summarize lambda_min_1
esttab using "112_12_hazard_struc_death.csv", replace plain cells("mean sd count") noobs

stset duration_years, failure(exit)
//sts graph, survival
sts gen hazard_y = h
//sts graph, hazard_d title("Smoothed Hazard estimate, Structured") saving(112_hazard_struc, replace)
streg n_firms, distribution(weibull), if detected==1
//predict meandS, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
predict lambda_min_1_years, mean time  // predict mean time = predict mean survival time = mean duration = 1/lambda
estpost summarize lambda_min_1_years
esttab using "112_12_hazard_struc_death_years.csv", replace plain cells("mean sd count") noobs

//** theta = probability of being born | not being alive
//** 1/theta = meanBS = Mean Births = mean interarrival time in days
stset startdate, failure(exit)
sts gen hazard_b = h
streg n_firms, distribution(weibull), if detected==1
predict theta_min_1, mean time 
estpost su theta_min_1
esttab using "112_12_hazard_struc_birth.csv", replace plain cells("mean sd count") noobs


//*******************************************************************************
//** unstructured
//** lambda = probability of dying | being alive on a give day
//** 1/lambda = mean duration in days

// replace exit with detected

//** SAMPLE
clear all
insheet using "sample_duration.csv", clear delimiter(;)
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
