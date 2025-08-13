******************************************************************************
 * Moderating Effect of Board Independence on the Relationship Between CEO Overconfidence and Corporate Financing Policy: Empirical Evidence from US Listed Companies
 * Sample Period: 2014-2024
 * Author: Shi Qiu
 * Student ID: 11537704
******************************************************************************
clear all
set more off
capture log close

******************************************************************************
*** Step 0: Install Required External Packages ***
******************************************************************************

display "Checking and installing required external packages..."

// Install winsor2 command
capture which winsor2
if _rc != 0 {
    display "Installing winsor2 package..."
    ssc install winsor2
    display "winsor2 package installed successfully"
}
else {
    display "winsor2 package already exists"
}

// Install estout command
capture which estout
if _rc != 0 {
    display "Installing estout package..."
    ssc install estout
    display "estout package installed successfully"
}
else {
    display "estout package already exists"
}

// Install reghdfe command
capture which reghdfe
if _rc != 0 {
    display "Installing reghdfe package..."
    ssc install reghdfe
    display "reghdfe package installed successfully"
}
else {
    display "reghdfe package already exists"
}

// Install ftools command
capture which ftools
if _rc != 0 {
    display "Installing ftools package..."
    ssc install ftools
    display "ftools package installed successfully"
}
else {
    display "ftools package already exists"
}

display "All required packages checked"
display "=========================================="

// Set working directory
cd "C:\Users\13158\Desktop\Final\Code12"
display "Working directory set to: " c(pwd)

// Create output directories
cap mkdir "out"
display "Output directory prepared"

******************************************************************************
*** Step 1: Data Import and Initial Cleaning ***
******************************************************************************

// Import board data
display "Importing board data..."
import delimited "Board.csv", clear
ds
display "Board data imported, number of variables: " c(k)
display "Number of observations: " _N

// Check ned variable distribution
display "Checking ned variable distribution..."
tab ned, missing

// Check CEO-related variables
display "Checking CEO observations in rolename..."
tab rolename if strpos(upper(rolename), "CEO") > 0

// Analyze board composition by company
display "Analyzing board composition by company..."
bysort companyid: egen total_directors = count(directorid)
bysort companyid: egen indep_directors = total(ned == "Yes")

// Generate board independence ratio
gen board_indep = indep_directors / total_directors if total_directors > 0
label variable board_indep "Board Independence Ratio"

display "Board independence distribution:"
summarize board_indep, detail

// Save board data
preserve
keep companyid board_indep indep_directors total_directors
duplicates drop
display "Company-level board data observations: " _N
display "Company-level board independence distribution:"
summarize board_indep, detail
save "board_temp.dta", replace
restore

// Import CEO data
display "Importing CEO data..."
import delimited "CEO.csv", clear
ds
display "CEO data imported, number of variables: " c(k)
display "Number of observations: " _N

// Check CEO identifier variable
display "Checking ceoann variable..."
tab ceoann

// Keep only CEO observations
display "Keeping only CEO observations..."
keep if ceoann == "CEO"
display "CEO data observations after cleaning: " _N

// Check duplicates
display "Checking duplicates of gvkey-year in CEO data..."
duplicates report gvkey year

// Handle duplicate records
duplicates drop gvkey year, force
display "CEO data observations after handling duplicates: " _N

// Keep necessary variables
keep gvkey year opt_unex_exer_est_val tdc1
// Remove observations with missing key variables
drop if missing(gvkey) | missing(year)
save "ceo_temp.dta", replace

// Import firm data
display "Importing firm data..."
import delimited "Firm.csv", clear
ds
display "Firm data imported, number of variables: " c(k)
display "Number of observations: " _N

// Rename year variable
rename fyear year

// Check duplicates
display "Checking duplicates of gvkey-year in firm data..."
duplicates report gvkey year

// Keep needed variables
keep gvkey year sic at che dlc dltt ppent ni capx csho prcc_f ceq
// Remove observations with missing key variables
drop if missing(gvkey) | missing(year) | missing(at) | at <= 0
display "Firm data observations after cleaning: " _N
save "firm_temp.dta", replace

// Import linking data
display "Importing linking data..."
import delimited "Linking.csv", clear
ds
display "Linking data imported, number of variables: " c(k)
display "Number of observations: " _N

// Check duplicates
display "Checking duplicates of gvkey in linking data..."
duplicates report gvkey

// Keep necessary variables
keep gvkey companyid
duplicates drop gvkey, force
display "Linking data observations after handling duplicates: " _N
save "linking_temp.dta", replace

******************************************************************************
*** Step 2: Data Merging ***
******************************************************************************

display "Merging datasets..."

// Merge firm and CEO data
use "firm_temp.dta", clear
display "Firm data observations: " _N

merge m:1 gvkey year using "ceo_temp.dta"
display "After merging with CEO data:"
tab _merge
drop if _merge == 2
drop _merge

// Merge with linking table
merge m:1 gvkey using "linking_temp.dta"
display "After merging with linking table:"
tab _merge
keep if _merge == 3
drop _merge

// Merge with board data
merge m:1 companyid using "board_temp.dta"
display "After merging with board data:"
tab _merge

// Check board independence distribution after merging
display "Board independence distribution after merging:"
summarize board_indep if _merge == 3, detail
count if board_indep == 0 & _merge == 3
display "Observations with board independence = 0: " r(N)
count if board_indep > 0 & _merge == 3
display "Observations with board independence > 0: " r(N)

keep if _merge == 3
drop _merge

display "Data merging completed"
ds
display "Variables after merging: " c(k)
display "Observations after merging: " _N

******************************************************************************
*** Step 3: Sample Selection and Period Setting ***
******************************************************************************

display "Executing sample selection..."

// Sample period filtering: keep 2014-2024
display "Filtering sample period: 2014-2024"
count
display "Observations before filtering: " r(N)
keep if inrange(year, 2014, 2024)
count
display "Observations after keeping 2014-2024: " r(N)

// Exclude financial and utility industries
display "Excluding financial and utility industries"
count
display "Observations before industry filtering: " r(N)
drop if inrange(sic, 4900, 4999) | inrange(sic, 6000, 6999)
count
display "Observations after excluding financial and utilities: " r(N)

// Set panel data structure
display "Setting panel data structure..."
xtset gvkey year
display "Panel setup completed, panel unit: gvkey, time: year"

******************************************************************************
*** Step 4: Basic Variable Construction ***
******************************************************************************

display "Constructing basic variables..."

// Drop missing values for key financial variables
foreach var of varlist che dlc dltt ppent ni capx csho prcc_f ceq {
    drop if missing(`var')
}

// Check SIC codes and generate industry classification
display "Checking SIC code distribution..."
summarize sic, detail
count if missing(sic)
display "Observations with missing SIC codes: " r(N)

// Drop observations with missing SIC codes
drop if missing(sic)
display "Observations after dropping missing SIC codes: " _N

// Generate 2-digit SIC industry codes
gen sic2 = int(sic/100)
label variable sic2 "2-digit SIC Industry Code"

// Check industry distribution
display "Industry distribution:"
tab sic2, sort

// Check number of observations by industry
bysort sic2: gen industry_count = _N
summarize industry_count, detail
display "Minimum observations per industry: " r(min)
display "Maximum observations per industry: " r(max)

display "Observations after cleaning missing values: " _N

// Construct dependent variables
gen leverage = (dlc + dltt) / at
gen cash_hold = che / at
label variable leverage "Financial Leverage"
label variable cash_hold "Cash Holdings"

// Construct control variables
gen ln_assets = ln(at)
gen roa = ni / at
gen tobinsq = (at + abs(prcc_f) * csho - ceq) / at
gen tangibility = ppent / at

// Handle CEO compensation variable
gen ln_ceopay = ln(tdc1) if !missing(tdc1) & tdc1 > 0

label variable ln_assets "Firm Size (ln)"
label variable roa "ROA"
label variable tobinsq "Tobin's Q"
label variable tangibility "Tangibility"
label variable ln_ceopay "CEO Pay (ln)"

******************************************************************************
*** Step 5: Overconfidence Variable Construction ***
******************************************************************************

display "Constructing overconfidence variables..."

// Investment-based overconfidence: industry-year adjustment
display "Constructing investment rate variable..."
sort gvkey year
by gvkey: gen lag_at = at[_n-1] if year == year[_n-1] + 1
gen inv_rate = capx / lag_at if lag_at > 0 & !missing(lag_at)
label variable inv_rate "Investment Rate"

display "Observations after investment rate calculation: " _N

// Industry-year median adjustment
display "Calculating industry-year adjusted investment rate..."
bysort year sic2: egen inv_med = median(inv_rate) if !missing(inv_rate)
gen inv_adj = inv_rate - inv_med if !missing(inv_rate) & !missing(inv_med)
label variable inv_adj "Industry-Year Adjusted Investment Rate"

// Calculate overconfidence based on adjusted investment rate
display "Calculating overconfidence indicator..."
egen inv_adj_p80 = pctile(inv_adj) if !missing(inv_adj), p(80)
gen overconf = (inv_adj >= inv_adj_p80) if !missing(inv_adj) & !missing(inv_adj_p80)
label variable overconf "Managerial Overconfidence"

// Keep original version as comparison
display "Keeping original version overconfidence calculation as comparison..."
egen year_ind_id = group(year sic2)
gen inv_quintile = .
label variable inv_quintile "Investment Rate Quintile"

bysort year_ind_id: egen group_count = count(inv_rate) if !missing(inv_rate)
bysort year_ind_id: egen inv_rank = rank(inv_rate) if !missing(inv_rate) & group_count >= 5
bysort year_ind_id: egen max_rank = max(inv_rank) if !missing(inv_rank)
gen temp_quintile = ceil(5 * inv_rank / max_rank) if !missing(inv_rank) & !missing(max_rank)

replace temp_quintile = 5 if temp_quintile > 5 & !missing(temp_quintile)
replace temp_quintile = 1 if temp_quintile < 1 & !missing(temp_quintile)
replace inv_quintile = temp_quintile if !missing(temp_quintile)

gen overconf_alt = (inv_quintile == 5) if !missing(inv_quintile)
label variable overconf_alt "Managerial Overconfidence (Original Version)"

// Clean up temporary variables
drop group_count inv_rank max_rank temp_quintile

display "Investment-based overconfidence calculation completed"
tab overconf
tab overconf_alt

// Option-based overconfidence
display "Calculating option-based overconfidence..."

// Construct option value ratio
gen option_value_ratio = opt_unex_exer_est_val / tdc1 if !missing(opt_unex_exer_est_val) & !missing(tdc1) & tdc1 > 0

// Use absolute threshold
gen alt_overconf = (option_value_ratio >= 0.8) if !missing(option_value_ratio)
label variable alt_overconf "Alternative Overconfidence Indicator (Option-based)"

// Keep original version as comparison
bysort year: egen p80_options = pctile(opt_unex_exer_est_val) if !missing(opt_unex_exer_est_val), p(80)
bysort year: egen temp_p80 = max(p80_options)
gen alt_overconf_pctl = (opt_unex_exer_est_val >= temp_p80) if !missing(opt_unex_exer_est_val) & !missing(temp_p80)
label variable alt_overconf_pctl "Alternative Overconfidence Indicator (Original Version)"
drop p80_options temp_p80

display "Option-based overconfidence calculation completed"
tab alt_overconf
tab alt_overconf_pctl

******************************************************************************
*** Step 6: Board Independence Variables and Interaction Terms ***
******************************************************************************

display "Constructing board-related variables..."

// Construct moderating variables
gen board_indep2 = board_indep^2
label variable board_indep2 "Board Independence Squared"

// Construct interaction terms
gen oc_bi = overconf * board_indep
gen oc_bi2 = overconf * board_indep2
label variable oc_bi "OC × Board Independence"
label variable oc_bi2 "OC × Board Independence²"

// Construct alternative version interaction terms
gen alt_oc_bi = alt_overconf * board_indep
gen alt_oc_bi2 = alt_overconf * board_indep2
label variable alt_oc_bi "Alt OC × Board Independence"
label variable alt_oc_bi2 "Alt OC × Board Independence²"

******************************************************************************
*** Step 7: Simultaneity Control Variables ***
******************************************************************************

display "Constructing simultaneity control variables..."

// Generate lagged dependent variables to address simultaneity
sort gvkey year
by gvkey: gen L_leverage = leverage[_n-1] if year == year[_n-1] + 1
by gvkey: gen L_cash_hold = cash_hold[_n-1] if year == year[_n-1] + 1

label variable L_leverage "Lagged Financial Leverage"
label variable L_cash_hold "Lagged Cash Holdings"

// Generate lagged explanatory variables for reverse causality test
by gvkey: gen L_overconf = overconf[_n-1] if year == year[_n-1] + 1
by gvkey: gen L_board_indep = board_indep[_n-1] if year == year[_n-1] + 1
by gvkey: gen L_board_indep2 = board_indep2[_n-1] if year == year[_n-1] + 1
by gvkey: gen L_oc_bi = oc_bi[_n-1] if year == year[_n-1] + 1
by gvkey: gen L_oc_bi2 = oc_bi2[_n-1] if year == year[_n-1] + 1

label variable L_overconf "Lagged Overconfidence"
label variable L_board_indep "Lagged Board Independence"
label variable L_board_indep2 "Lagged Board Independence²"
label variable L_oc_bi "Lagged Interaction: OC × Board Independence"
label variable L_oc_bi2 "Lagged Interaction: OC × Board Independence²"

******************************************************************************
*** Step 8: COVID and Firm Size Variables ***
******************************************************************************

display "Constructing COVID and firm size variables..."

// COVID period variable
gen COVID = (year >= 2020)
label variable COVID "COVID Period Indicator"

// Triple interaction terms
gen oc_bi_covid = overconf * board_indep * COVID
gen oc_bi2_covid = overconf * board_indep2 * COVID
label variable oc_bi_covid "OC × Board Independence × COVID"
label variable oc_bi2_covid "OC × Board Independence² × COVID"

// Firm size grouping
bysort year: egen at_median = median(at)
gen large = (at >= at_median)
label variable large "Large Firm Indicator"

display "COVID and size variables construction completed"
tab COVID
tab large

******************************************************************************
*** Step 9: Data Cleaning and Winsorization ***
******************************************************************************

display "Executing data cleaning and winsorization..."

// Keep observations with complete data
keep if !missing(leverage, cash_hold, overconf, board_indep, ln_assets, roa, tobinsq, tangibility)

display "Observations with complete data: " _N

// Check variable distributions
display "Board independence distribution in regression sample:"
summarize board_indep, detail
count if board_indep == 0
display "Observations with board independence = 0: " r(N)
count if board_indep > 0
display "Observations with board independence > 0: " r(N)

// Perform winsorization
display "Performing variable winsorization..."

foreach var of varlist leverage cash_hold ln_assets roa tobinsq tangibility board_indep {
    winsor2 `var', replace cuts(1 99)
    display "Variable `var' winsorized at 1% and 99%"
}

// Winsorize CEO pay
winsor2 ln_ceopay if !missing(ln_ceopay), replace cuts(1 99)
display "CEO pay variable winsorized at 1% and 99%"

display "Final sample size: " _N

// Check industry distribution
display "Industry distribution in final regression sample:"
tab sic2, sort
bysort sic2: gen final_industry_count = _N
summarize final_industry_count, detail
display "Minimum observations per industry in final sample: " r(min)
display "Maximum observations per industry in final sample: " r(max)

******************************************************************************
*** Step 10: Descriptive Statistics ***
******************************************************************************

display "Generating descriptive statistics..."

estpost tabstat leverage cash_hold overconf alt_overconf board_indep board_indep2 ///
    ln_assets roa tobinsq tangibility ln_ceopay, ///
    statistics(count mean sd min max p25 p50 p75) columns(statistics)

esttab using "out/desc_stats.rtf", replace ///
    title("Descriptive Statistics") ///
    cells("count(fmt(%9.0fc)) mean(fmt(%9.3f)) sd(fmt(%9.3f)) min(fmt(%9.3f)) max(fmt(%9.3f)) p25(fmt(%9.3f)) p50(fmt(%9.3f)) p75(fmt(%9.3f))") ///
    nonumbers nomtitles ///
    addnote("Note: Continuous variables are winsorized at 1% and 99% percentiles")

display "Descriptive statistics exported to out/desc_stats.rtf"

******************************************************************************
*** Step 11: Main Regression Analysis ***
******************************************************************************

display "=========================================="
display "Starting Main Regression Analysis"
display "Model Specification:"
display "- Dependent variables: Financial leverage and cash holdings"
display "- Independent variable: Managerial overconfidence"
display "- Moderating variable: Board independence"
display "- Control variables: Firm size, ROA, Tobin's Q, tangibility, CEO pay"
display "- Simultaneity control: Include lagged dependent variable"
display "- Fixed effects: Firm and time fixed effects"
display "- Standard errors: Clustered at firm level"
display "=========================================="

// Create regression sample
preserve
drop if missing(ln_ceopay) | missing(L_cash_hold) | missing(L_leverage)
display "Regression sample size: " _N

// Check sample distributions
display "Board independence distribution in regression sample:"
summarize board_indep, detail

display "Year distribution in regression sample:"
tab year, sort

// Leverage regressions
display "Conducting financial leverage regression analysis..."
eststo clear

// Model 1: Leverage - Basic model
display "Estimating leverage basic model..."
eststo lev1_sim: areg leverage overconf board_indep L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
display "Completed leverage model 1"

// Model 2: Leverage - Add interaction
display "Estimating leverage interaction model..."
eststo lev2_sim: areg leverage overconf board_indep oc_bi L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
display "Completed leverage model 2"

// Model 3: Leverage - Full model
display "Estimating leverage full model..."
eststo lev3_sim: areg leverage overconf board_indep oc_bi board_indep2 oc_bi2 L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
display "Completed leverage model 3"

// Export leverage regression results
esttab lev1_sim lev2_sim lev3_sim using "out/leverage_regressions_simultaneity.rtf", replace ///
    title("Financial Leverage Regression Results") ///
    mtitles("Basic" "Interaction" "Full") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: All models include firm and time fixed effects" ///
           "Time fixed effects coefficients omitted" ///
           "Standard errors clustered at firm level" ///
           "Robust standard errors in parentheses" ///
           "Include lagged cash holdings to control simultaneity" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

display "Financial leverage regression results exported to out/leverage_regressions_simultaneity.rtf"

// Cash holdings regressions
display "Conducting cash holdings regression analysis..."
eststo clear

// Model 1: Cash Holdings - Basic model
display "Estimating cash holdings basic model..."
eststo cash1_sim: areg cash_hold overconf board_indep L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
display "Completed cash holdings model 1"

// Model 2: Cash Holdings - Add interaction
display "Estimating cash holdings interaction model..."
eststo cash2_sim: areg cash_hold overconf board_indep oc_bi L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
display "Completed cash holdings model 2"

// Model 3: Cash Holdings - Full model
display "Estimating cash holdings full model..."
eststo cash3_sim: areg cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
display "Completed cash holdings model 3"

// Export cash holdings regression results
esttab cash1_sim cash2_sim cash3_sim using "out/cash_holdings_regressions_simultaneity.rtf", replace ///
    title("Cash Holdings Regression Results") ///
    mtitles("Basic" "Interaction" "Full") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: All models include firm and time fixed effects" ///
           "Time fixed effects coefficients omitted" ///
           "Standard errors clustered at firm level" ///
           "Robust standard errors in parentheses" ///
           "Include lagged financial leverage to control simultaneity" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

display "Cash holdings regression results exported to out/cash_holdings_regressions_simultaneity.rtf"

restore

******************************************************************************
*** Step 12: Simultaneity Diagnostics (SUR Analysis) ***
******************************************************************************

display "Conducting simultaneity diagnostics (SUR analysis)..."

// Create sample for SUR analysis
preserve
drop if missing(ln_ceopay, L_cash_hold, L_leverage, leverage, cash_hold, overconf, board_indep, oc_bi, board_indep2, oc_bi2, ln_assets, roa, tobinsq, tangibility)

display "SUR analysis sample size: " _N

// Step A: Remove fixed effects (partialling out)
display "Executing partialling out procedure to remove fixed effects..."

// Double demeaning for all variables (equivalent to two-way fixed effects)
local varlist "leverage cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 L_leverage L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay"

foreach var of local varlist {
    // Step 1: Remove firm fixed effects
    areg `var', absorb(gvkey)
    predict `var'_resid1, resid
    
    // Step 2: Remove time fixed effects
    reg `var'_resid1 i.year
    predict `var'_twoway_resid, resid
    
    label variable `var'_twoway_resid "`var' residuals after removing two-way FE"
    
    // Clean intermediate variables
    drop `var'_resid1
}

display "Partialling out completed, all variables with two-way fixed effects removed"

// Step B: SUR estimation on residualized variables
display "Conducting SUR estimation on residualized variables..."

// SUR regression: leverage equation and cash holdings equation
sureg (leverage_twoway_resid overconf_twoway_resid board_indep_twoway_resid oc_bi_twoway_resid board_indep2_twoway_resid oc_bi2_twoway_resid L_cash_hold_twoway_resid ln_assets_twoway_resid roa_twoway_resid tobinsq_twoway_resid tangibility_twoway_resid ln_ceopay_twoway_resid) ///
      (cash_hold_twoway_resid overconf_twoway_resid board_indep_twoway_resid oc_bi_twoway_resid board_indep2_twoway_resid oc_bi2_twoway_resid L_leverage_twoway_resid ln_assets_twoway_resid roa_twoway_resid tobinsq_twoway_resid tangibility_twoway_resid ln_ceopay_twoway_resid)

// Save SUR results
estimates store sur_results

// Export SUR results
esttab sur_results using "out/sur_analysis.rtf", replace ///
    title("Simultaneity Diagnostics: SUR Estimation Results") ///
    mtitles("Leverage Equation" "Cash Holdings Equation") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N, fmt(%9.0fc) ///
          labels("Observations")) ///
    addnote("Note: This table shows SUR estimation results after removing two-way fixed effects" ///
           "Used to diagnose simultaneity between the two financing policy equations" ///
           "All variables are residuals after removing firm and time fixed effects" ///
           "SUR is for diagnostic and efficiency reference only, not replacing fixed effects main specification" ///
           "Error correlation coefficient ρ and Breusch-Pagan test see statistical output" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

display "SUR analysis results exported to out/sur_analysis.rtf"

// Extract and display key diagnostic information
display "=========================================="
display "Key Simultaneity Diagnostics Results:"
display "1. Error correlation coefficient (ρ) and Breusch-Pagan independence test results included in SUR output"
display "2. If ρ is significantly different from 0, indicates simultaneity between two financing policy equations"
display "3. This diagnosis supports the approach of including lagged dependent variables in main regressions"
display "=========================================="

restore

******************************************************************************
*** Step 13: Reverse Causality Test ***
******************************************************************************

display "Conducting reverse causality test..."

// Use lagged explanatory variables to estimate models
preserve
drop if missing(ln_ceopay, L_overconf, L_board_indep, L_oc_bi, L_board_indep2, L_oc_bi2)
display "Reverse causality test sample size: " _N

// Reverse causality test - Leverage regressions
display "Conducting reverse causality test - Financial leverage regression..."
eststo clear

eststo rev_lev1: areg leverage L_overconf L_board_indep ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo rev_lev2: areg leverage L_overconf L_board_indep L_oc_bi ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo rev_lev3: areg leverage L_overconf L_board_indep L_oc_bi L_board_indep2 L_oc_bi2 ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

// Export reverse causality test - Leverage results
esttab rev_lev1 rev_lev2 rev_lev3 using "out/reverse_causality_leverage.rtf", replace ///
    title("Reverse Causality Test: Financial Leverage Regression") ///
    mtitles("Basic Model" "Interaction Model" "Full Model") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: All explanatory variables use one-period lag to test reverse causality" ///
           "All models include firm and time fixed effects" ///
           "Time fixed effects coefficients omitted" ///
           "Standard errors clustered at firm level" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

// Reverse causality test - Cash holdings regressions
display "Conducting reverse causality test - Cash holdings regression..."
eststo clear

eststo rev_cash1: areg cash_hold L_overconf L_board_indep ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo rev_cash2: areg cash_hold L_overconf L_board_indep L_oc_bi ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo rev_cash3: areg cash_hold L_overconf L_board_indep L_oc_bi L_board_indep2 L_oc_bi2 ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

// Export reverse causality test - Cash holdings results
esttab rev_cash1 rev_cash2 rev_cash3 using "out/reverse_causality_cash.rtf", replace ///
    title("Reverse Causality Test: Cash Holdings Regression") ///
    mtitles("Basic Model" "Interaction Model" "Full Model") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: All explanatory variables use one-period lag to test reverse causality" ///
           "All models include firm and time fixed effects" ///
           "Time fixed effects coefficients omitted" ///
           "Standard errors clustered at firm level" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

display "Reverse causality test results exported to out/reverse_causality_leverage.rtf and out/reverse_causality_cash.rtf"

restore

******************************************************************************
*** Step 14: COVID Period Analysis and Triple Interaction ***
******************************************************************************

display "Conducting COVID period analysis..."

// COVID period triple interaction analysis
preserve
drop if missing(ln_ceopay, L_cash_hold, L_leverage)
display "COVID analysis sample size: " _N

// COVID triple interaction - Leverage regression
display "Conducting COVID triple interaction - Financial leverage regression..."
eststo clear

eststo covid_lev: areg leverage overconf board_indep oc_bi board_indep2 oc_bi2 COVID oc_bi_covid oc_bi2_covid L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

// COVID triple interaction - Cash holdings regression
display "Conducting COVID triple interaction - Cash holdings regression..."
eststo covid_cash: areg cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 COVID oc_bi_covid oc_bi2_covid L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

// Export COVID triple interaction results
esttab covid_lev covid_cash using "out/covid_triple_interaction.rtf", replace ///
    title("COVID Period Triple Interaction Effects Analysis") ///
    mtitles("Leverage" "Cash Holdings") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: COVID=1 indicates 2020 and after" ///
           "Triple interaction terms measure changes in board independence moderating effects during COVID" ///
           "All models include firm and time fixed effects and simultaneity controls" ///
           "Standard errors clustered at firm level" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

restore

// Subsample regression analysis: Pre-COVID vs COVID comparison
display "Conducting subsample regression analysis: Pre-COVID vs COVID comparison..."

// Pre-COVID sample (2014-2019)
preserve
keep if year <= 2019
drop if missing(ln_ceopay, L_cash_hold, L_leverage)
display "Pre-COVID sample size (2014-2019): " _N

eststo clear
eststo pre_lev: areg leverage overconf board_indep oc_bi board_indep2 oc_bi2 L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo pre_cash: areg cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

esttab pre_lev pre_cash using "out/pre_covid_analysis.rtf", replace ///
    title("Pre-COVID Period Analysis (2014-2019)") ///
    mtitles("Leverage" "Cash Holdings") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: Sample period is 2014-2019 (pre-COVID)" ///
           "All models include firm and time fixed effects and simultaneity controls" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

restore

// COVID period sample (2020-2024)
preserve
keep if year >= 2020
drop if missing(ln_ceopay, L_cash_hold, L_leverage)
display "COVID period sample size (2020-2024): " _N

eststo clear
eststo post_lev: areg leverage overconf board_indep oc_bi board_indep2 oc_bi2 L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo post_cash: areg cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

esttab post_lev post_cash using "out/post_covid_analysis.rtf", replace ///
    title("COVID Period Analysis (2020-2024)") ///
    mtitles("Leverage" "Cash Holdings") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: Sample period is 2020-2024 (COVID period)" ///
           "All models include firm and time fixed effects and simultaneity controls" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

restore

display "COVID period analysis completed"

******************************************************************************
*** Step 15: Firm Size Heterogeneity Analysis ***
******************************************************************************

display "Conducting firm size heterogeneity analysis..."

// Large firms subsample analysis
preserve
keep if large == 1
drop if missing(ln_ceopay, L_cash_hold, L_leverage)
display "Large firms subsample size: " _N

eststo clear
eststo large_lev: areg leverage overconf board_indep oc_bi board_indep2 oc_bi2 L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo large_cash: areg cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

esttab large_lev large_cash using "out/large_firms_analysis.rtf", replace ///
    title("Large Firms Subsample Analysis") ///
    mtitles("Leverage" "Cash Holdings") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: Large firms defined as total assets above annual median" ///
           "All models include firm and time fixed effects and simultaneity controls" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

restore

// Small firms subsample analysis
preserve
keep if large == 0
drop if missing(ln_ceopay, L_cash_hold, L_leverage)
display "Small firms subsample size: " _N

eststo clear
eststo small_lev: areg leverage overconf board_indep oc_bi board_indep2 oc_bi2 L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo small_cash: areg cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

esttab small_lev small_cash using "out/small_firms_analysis.rtf", replace ///
    title("Small Firms Subsample Analysis") ///
    mtitles("Leverage" "Cash Holdings") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: Small firms defined as total assets below annual median" ///
           "All models include firm and time fixed effects and simultaneity controls" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

restore

display "Firm size heterogeneity analysis completed"

******************************************************************************
*** Step 16: Hausman Test and Estimation Choice ***
******************************************************************************

display "Conducting Hausman test..."

// Create sample for Hausman test
preserve
drop if missing(ln_ceopay, L_cash_hold, L_leverage)
display "Hausman test sample size: " _N

// Use standard standard errors for Hausman test
display "=========================================="
display "Note: Due to incompatibility between clustered standard errors and Hausman test,"
display "standard standard errors are used here for diagnostic purposes only"
display "Main regression results still use clustered robust standard errors"
display "=========================================="

// Estimate random effects model (leverage)
display "Estimating leverage random effects model..."
xtreg leverage overconf board_indep oc_bi board_indep2 oc_bi2 L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, re
estimates store leverage_re_std

// Estimate fixed effects model (leverage)
display "Estimating leverage fixed effects model..."
xtreg leverage overconf board_indep oc_bi board_indep2 oc_bi2 L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, fe
estimates store leverage_fe_std

// Conduct Hausman test (leverage)
display "Conducting leverage Hausman test..."
hausman leverage_fe_std leverage_re_std, sigmamore
display "Leverage Hausman test completed"

// Estimate random effects model (cash holdings)
display "Estimating cash holdings random effects model..."
xtreg cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, re
estimates store cash_re_std

// Estimate fixed effects model (cash holdings)
display "Estimating cash holdings fixed effects model..."
xtreg cash_hold overconf board_indep oc_bi board_indep2 oc_bi2 L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, fe
estimates store cash_fe_std

// Conduct Hausman test (cash holdings)
display "Conducting cash holdings Hausman test..."
hausman cash_fe_std cash_re_std, sigmamore
display "Cash holdings Hausman test completed"

// Export Hausman test results comparison
esttab leverage_fe_std leverage_re_std cash_fe_std cash_re_std using "out/hausman_test_results.rtf", replace ///
    title("Hausman Test: Fixed Effects vs Random Effects") ///
    mtitles("Leverage-FE" "Leverage-RE" "Cash Holdings-FE" "Cash Holdings-RE") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: FE=Fixed Effects, RE=Random Effects" ///
           "This table uses standard standard errors for Hausman test compatibility" ///
           "Main regression analysis uses clustered robust standard errors" ///
           "Hausman test statistics and p-values see Stata output" ///
           "If test rejects null hypothesis, supports using fixed effects model" ///
           "All models include simultaneity control variables" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

display "Hausman test results exported to out/hausman_test_results.rtf"

restore

******************************************************************************
*** Step 17: Robustness Tests ***
******************************************************************************

display "Conducting robustness tests..."
display "Robustness tests: Using alternative overconfidence measure"

// Robustness tests using alternative overconfidence indicator
preserve
drop if missing(alt_overconf, ln_ceopay, L_cash_hold, L_leverage)
display "Alternative indicator robustness test sample size: " _N

// Construct interaction terms for alternative indicator
gen alt_oc_bi_new = alt_overconf * board_indep
gen alt_oc_bi2_new = alt_overconf * board_indep2

// Robustness tests - Leverage regressions
display "Conducting robustness tests - Financial leverage regression..."
eststo clear

eststo rob_lev1: areg leverage alt_overconf board_indep L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo rob_lev2: areg leverage alt_overconf board_indep alt_oc_bi_new L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo rob_lev3: areg leverage alt_overconf board_indep alt_oc_bi_new board_indep2 alt_oc_bi2_new L_cash_hold ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

// Export robustness test leverage results
esttab rob_lev1 rob_lev2 rob_lev3 using "out/robustness_leverage_alt.rtf", replace ///
    title("Robustness Tests: Financial Leverage Regression (Alternative Overconfidence Indicator)") ///
    mtitles("Basic Model" "Interaction Model" "Full Model") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: Using alternative overconfidence indicator based on option value ratio" ///
           "All models include firm and time fixed effects and simultaneity controls" ///
           "Time fixed effects coefficients omitted" ///
           "Standard errors clustered at firm level" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

// Robustness tests - Cash holdings regressions
display "Conducting robustness tests - Cash holdings regression..."
eststo clear

eststo rob_cash1: areg cash_hold alt_overconf board_indep L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo rob_cash2: areg cash_hold alt_overconf board_indep alt_oc_bi_new L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)
eststo rob_cash3: areg cash_hold alt_overconf board_indep alt_oc_bi_new board_indep2 alt_oc_bi2_new L_leverage ln_assets roa tobinsq tangibility ln_ceopay i.year, absorb(gvkey) cluster(gvkey)

// Export robustness test cash holdings results
esttab rob_cash1 rob_cash2 rob_cash3 using "out/robustness_cash_alt.rtf", replace ///
    title("Robustness Tests: Cash Holdings Regression (Alternative Overconfidence Indicator)") ///
    mtitles("Basic Model" "Interaction Model" "Full Model") ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    drop(*year*) ///
    stats(N r2, fmt(%9.0fc %9.3f) ///
          labels("Observations" "R²")) ///
    addnote("Note: Using alternative overconfidence indicator based on option value ratio" ///
           "All models include firm and time fixed effects and simultaneity controls" ///
           "Time fixed effects coefficients omitted" ///
           "Standard errors clustered at firm level" ///
           "*, **, *** indicate significance at 10%, 5%, 1% levels")

display "Robustness test results exported to out/robustness_leverage_alt.rtf and out/robustness_cash_alt.rtf"

restore

******************************************************************************
*** Step 18: Clean up temporary files ***
******************************************************************************

display "Cleaning up temporary files..."
capture erase "board_temp.dta"
capture erase "ceo_temp.dta"
capture erase "firm_temp.dta"
capture erase "linking_temp.dta"

******************************************************************************
*** Analysis Completed Summary ***
******************************************************************************

display "=========================================="
display "All analyses completed!"
display "Output files:"
display "1. out/desc_stats.rtf - Descriptive Statistics"
display "2. out/leverage_regressions_simultaneity.rtf - Financial Leverage Regression Results"
display "3. out/cash_holdings_regressions_simultaneity.rtf - Cash Holdings Regression Results"
display "4. out/sur_analysis.rtf - Simultaneity Diagnostics: SUR Estimation Results"
display "5. out/reverse_causality_leverage.rtf - Reverse Causality Test: Leverage Regression"
display "6. out/reverse_causality_cash.rtf - Reverse Causality Test: Cash Holdings Regression"
display "7. out/covid_triple_interaction.rtf - COVID Period Triple Interaction Effects Analysis"
display "8. out/pre_covid_analysis.rtf - Pre-COVID Period Analysis (2014-2019)"
display "9. out/post_covid_analysis.rtf - COVID Period Analysis (2020-2024)"
display "10. out/large_firms_analysis.rtf - Large Firms Subsample Analysis"
display "11. out/small_firms_analysis.rtf - Small Firms Subsample Analysis"
display "12. out/hausman_test_results.rtf - Hausman Test Results"
display "13. out/robustness_leverage_alt.rtf - Robustness Tests: Leverage Regression (Alternative Indicator)"
display "14. out/robustness_cash_alt.rtf - Robustness Tests: Cash Holdings Regression (Alternative Indicator)"
display "=========================================="

display "Regression Method Summary:"
display "- Estimation method: Fixed effects regression"
display "- Fixed effects: Firm and time fixed effects"
display "- Standard errors: Firm-level clustered robust standard errors"
display "- Control variables: Firm size, ROA, Tobin's Q, tangibility, CEO pay"

display "Final sample statistics:"
display "Number of observations: " _N
count if !missing(leverage, cash_hold, overconf, board_indep)
display "Observations with basic variables: " r(N)

summarize leverage cash_hold overconf alt_overconf board_indep ln_ceopay roa, detail