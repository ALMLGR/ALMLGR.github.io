// This code should be run in Stata 16 or 15

*cd "/Path/To/1918_npi_effects/"

version 16.1

clear all
set more off, permanently
set emptycells drop

use "data/output/all_city.dta" 

gen CityState = City + ", " + State

encode CityState, gen(City_f)
rename (City City_f) (City_s City)

rename totalPop State_totalPop
 
local tablenumber 1 
 
** Controls

gen AgricultureEmpShare1910 = State_agricultureOccupied / State_totalPersonsOccupied
gen ManufacturingEmpShare1914 = CityManuEmp1914 / Pop1910
gen UrbanPopShare1910 = State_urbanPopAbove2500/ State_totalPop
gen IncomePerCapita1910 = State_personalIncomeLindert1910
gen Pop1910_ln = ln(Pop1910) 

local controls_npi c.AgricultureEmpShare1910 c.ManufacturingEmpShare1914 c.UrbanPopShare1910 c.IncomePerCapita1910 c.Pop1910_ln c.Mortality1917


*** Regressions

reshape long CityManuEmp CityManuOutput, i(City) j(Year)

gen PostSpanishFlu = Year > 1918

gen CityManuEmp_ln = ln(CityManuEmp)
gen CityManuOutput_ln = ln(CityManuOutput)

gen PopGrowth1910_ln = ln(Pop1910/Pop1900)
gen PopGrowth1917_ln = ln(Pop1917/Pop1910)

xtset City Year
local panelvar = r(panelvar)
local panelvar_name "City FE"

gen CorreiaYears = Year >= 1914 & Year <= 1923

label variable SpeedofNPI "NPI Speed"
label variable DaysofNPI "NPI Days"
label variable PostSpanishFlu "Post"


*** NPI Intensity

local outcomelist CityManuEmp_ln CityManuOutput_ln
local labellist `" "Manufacturing Employment" "Manufacturing Output" "'
local labelshortlist Emp Output

local n: word count `outcomelist'
disp `n'

forvalues i = 1/`n' {

	local var: word `i' of `outcomelist'
	local lab: word `i' of `labellist'
	local short: word `i' of `labelshortlist'
	
	label variable `var' "`lab'" 

	
	** Pooled DiD

	* No Controls - Replication
	qui xtreg `var' c.PostSpanishFlu#c.DaysofNPI i.Year if CorreiaYears == 1, fe vce(cluster City)
	est store `short'_pool_no_ctrls_rep
	estadd local absorbed_fe = cond(e(absvar) == "`panelvar'","Yes","No",""), replace
	
	* Basic Controls - Replication
	qui xtreg `var' c.PostSpanishFlu#c.DaysofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') i.Year if CorreiaYears == 1, fe vce(cluster City)
	est store `short'_pool_ctrls_rep
	estadd local absorbed_fe = cond(e(absvar) == "`panelvar'","Yes","No",""), replace
	
	* City Specific Growth Rates
	qui xtreg `var' c.PostSpanishFlu#c.DaysofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') i.City#c.Year i.Year, fe vce(cluster City)
	est store `short'_pool_city_g
	estadd local absorbed_fe = cond(e(absvar) == "`panelvar'","Yes","No",""), replace
	
	* Linear Time Trend for Treatment (Pretrend)
	qui xtreg `var' c.PostSpanishFlu#c.DaysofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') c.DaysofNPI#c.Year i.Year, fe vce(cluster City)
	est store `short'_pool_pretrend
	estadd local absorbed_fe = cond(e(absvar) == "`panelvar'","Yes","No",""), replace
	test c.DaysofNPI#c.Year
	estadd scalar pretrend_pval = r(p)
	
	est table `short'_pool_*, stats(r2 N_g N) b(%9.6f) se(%9.6f) varlabel keep(c.PostSpanishFlu#c.DaysofNPI c.DaysofNPI#c.Year) modelwidth(18)

	esttab `short'_pool_* using "./results/table`tablenumber'.tex", cells(b(star fmt(%9.5f)) se(par fmt(%9.5f))) ///
	noomitted nobaselevels keep(c.PostSpanishFlu#c.DaysofNPI) indicate("City Time Trends = *.City#c.Year" "Treatment by Year Trend = c.DaysofNPI#c.Year" "Time FE = *.Year") ///
	stats(absorbed_fe r2 N_g N pretrend_pval, fmt(%s %9.4f %9.0g %9.0g %05.4f) labels("`panelvar_name'" R-squared Cities Observations "Pretrend P-value")) legend label ///
	style(tex) starlevels(* 0.10 ** 0.05 *** 0.01) mlabels(, depvars) collabels(none) title("Manufacturing Growth and Days of NPIs") replace

	local tablenumber = `tablenumber' + 1

	
	** Event Study

	* No Controls - Replication
	qui xtreg `var' i(1899 1904 1909 1919 1921 1923 1925 1927).Year#c.DaysofNPI i.Year, fe vce(cluster City)
	est store `short'_event_no_ctrls_rep

	* Basic Controls - Replication
	qui xtreg `var' i(1899 1904 1909 1919 1921 1923 1925 1927).Year#c.DaysofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') i.Year, fe vce(cluster City)
	est store `short'_event_ctrls_rep
	estout `short'_event_ctrls_rep using "./results/correiaEventStudy`short'NPI.txt", cells("b se ci_l ci_u") keep(*.Year#c.DaysofNPI) replace 
	
	* Linear Time Trend for Treatment (Pretrend)
	qui xtreg `var' i(1919 1921 1923 1925 1927).Year#c.DaysofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') c.DaysofNPI#c.Year i.Year, fe vce(cluster City)
	est store `short'_event_pretrend
	estout `short'_event_pretrend using "./results/controlEventStudy`short'NPI.txt", cells("b se ci_l ci_u") keep(*.Year#c.DaysofNPI) replace 
    
	est table `short'_event_*, stats(r2 N_g N) b(%9.6f) se(%9.6f) varlabel keep(i.Year#c.DaysofNPI c.DaysofNPI#c.Year) modelwidth(18)	
	
	estimates clear
	
}


*** NPI Speed

local outcomelist CityManuEmp_ln CityManuOutput_ln
local labellist `" "Manufacturing Employment" "Manufacturing Output" "'
local labelshortlist Emp Output

local n: word count `outcomelist'
disp `n'

forvalues i = 1/`n' {

	local var: word `i' of `outcomelist'
	local lab: word `i' of `labellist'
	local short: word `i' of `labelshortlist'
	
	label variable `var' "`lab'" 

	
	** Pooled DiD

	* No Controls - Replication
	qui xtreg `var' c.PostSpanishFlu#c.SpeedofNPI i.Year if CorreiaYears == 1, fe vce(cluster City)
	est store `short'_pool_no_ctrls_rep
	estadd local absorbed_fe = cond(e(absvar) == "`panelvar'","Yes","No",""), replace
	
	* Basic Controls - Replication
	qui xtreg `var' c.PostSpanishFlu#c.SpeedofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') i.Year if CorreiaYears == 1, fe vce(cluster City)
	est store `short'_pool_ctrls_rep
	estadd local absorbed_fe = cond(e(absvar) == "`panelvar'","Yes","No",""), replace
	
	* City Specific Growth Rates
	qui xtreg `var' c.PostSpanishFlu#c.SpeedofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') i.City#c.Year i.Year, fe vce(cluster City)
	est store `short'_pool_city_g
	estadd local absorbed_fe = cond(e(absvar) == "`panelvar'","Yes","No",""), replace
	
	* Linear Time Trend for Treatment (Pretrend)
	qui xtreg `var' c.PostSpanishFlu#c.SpeedofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') c.SpeedofNPI#c.Year i.Year, fe vce(cluster City)
	est store `short'_pool_pretrend
	estadd local absorbed_fe = cond(e(absvar) == "`panelvar'","Yes","No",""), replace
	test c.SpeedofNPI#c.Year
	estadd scalar pretrend_pval = r(p)
	
	est table `short'_pool_*, stats(r2 N_g N) b(%9.6f) se(%9.6f) varlabel keep(c.PostSpanishFlu#c.SpeedofNPI c.SpeedofNPI#c.Year) modelwidth(18)

	esttab `short'_pool_* using "./results/table`tablenumber'.tex", cells(b(star fmt(%9.5f)) se(par fmt(%9.5f))) ///
	noomitted nobaselevels keep(c.PostSpanishFlu#c.SpeedofNPI) indicate("City Time Trends = *.City#c.Year" "Treatment by Year Trend = c.SpeedofNPI#c.Year" "Time FE = *.Year") ///
	stats(absorbed_fe r2 N_g N pretrend_pval, fmt(%s %9.4f %9.0g %9.0g %05.4f) labels("`panelvar_name'" R-squared Cities Observations "Pretrend P-value")) legend label ///
	style(tex) starlevels(* 0.10 ** 0.05 *** 0.01) mlabels(, depvars) collabels(none) title("Manufacturing Growth and Speed of NPIs") replace

	local tablenumber = `tablenumber' + 1

	
	** Event Study

	* No Controls - Replication
	qui xtreg `var' i(1899 1904 1909 1919 1921 1923 1925 1927).Year#c.SpeedofNPI i.Year, fe vce(cluster City)
	est store `short'_event_no_ctrls_rep

	* Basic Controls - Replication
	qui xtreg `var' i(1899 1904 1909 1919 1921 1923 1925 1927).Year#c.SpeedofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') i.Year, fe vce(cluster City)
	est store `short'_event_ctrls_rep
	estout `short'_event_ctrls_rep using "./results/correiaEventStudy`short'NPISpeed.txt", cells("b se ci_l ci_u") keep(*.Year#c.SpeedofNPI) replace 
	
	* Linear Time Trend for Treatment (Pretrend)
	qui xtreg `var' i(1919 1921 1923 1925 1927).Year#c.SpeedofNPI i(1899 1904 1909 1919 1921 1923 1925 1927).Year#(`controls_npi') c.SpeedofNPI#c.Year i.Year, fe vce(cluster City)
	est store `short'_event_pretrend
	estout `short'_event_pretrend using "./results/controlEventStudy`short'NPISpeed.txt", cells("b se ci_l ci_u") keep(*.Year#c.SpeedofNPI) replace 
    
	est table `short'_event_*, stats(r2 N_g N) b(%9.6f) se(%9.6f) varlabel keep(i.Year#c.SpeedofNPI c.SpeedofNPI#c.Year) modelwidth(18)	
	
	estimates clear
	
}
