################################################
# Prepare state level data and export to Stata
################################################
state.popns <- fread(paste0(root.path,"/data/input/controls/state_populations.csv"))
state.popns$State <- c(state.name, 'District of Columbia')[match(state.popns$State, c(state.abb, 'DC'))]
state.flu.deaths <- fread(paste0(root.path,"/data/input/state_deaths_influenza.csv"))
state.wageEarners.output <- fread(paste0(root.path,"/data/input/state_wage_output.csv"))
state.wageEarners.output[, valueProduct := as.integer(valueProduct)]
state.wageEarners.output.wide <- dcast(state.wageEarners.output, State ~ Year, value.var=c("wageEarners","valueProduct"))
setDT(state.wageEarners.output.wide)
names(state.wageEarners.output.wide) <- gsub("_", "", names(state.wageEarners.output.wide))

## State controls
# Per capita income (note that when it is missing is replaced with USA value)
state.controls.perCapitaIncome <- fread(paste0(root.path, "/data/input/controls/personalIncomeLindert1910.csv"))
# Manufacturing, agriculture, and total employment
state.controls.sectorEmployment <- fread(paste0(root.path, "/data/input/controls/stateEmplShares1910.csv"))
# Urban population
state.controls.urbanRuralShares <- fread(paste0(root.path, "/data/input/controls/statePopUrbanRuralShares1910.csv"))
state.controls.all <- merge(state.controls.urbanRuralShares, state.controls.sectorEmployment,by = "State", all.x = T)
state.controls.all <- merge(state.controls.all, state.flu.deaths,by = "State", all.x = T)
state.controls.all <- merge(state.controls.all, state.controls.perCapitaIncome, by = "State", all.x = T)
# note that when income per capita is missing replace with USA value
state.controls.all[is.na(personalIncomeLindert1910), personalIncomeLindert1910 := state.controls.perCapitaIncome[State == "USA", personalIncomeLindert1910]]
all.state <- merge(state.popns, state.wageEarners.output.wide, by='State')
all.state <- merge(all.state, state.controls.all, by = "State")

##############################################
# Prepare city level data and export to Stata
##############################################
npis.mortality <- fread(paste0(root.path,"/data/input/npis_mortality.csv"))
city.wageEarners <- fread(paste0(root.path,"/data/input/city_employment_manu.csv"))
city.output <- fread(paste0(root.path,"/data/input/city_output_manu.csv"))
city.popns <- fread(paste0(root.path,"/data/input/controls/city_populations.csv"))

all.city <- merge(npis.mortality, city.wageEarners, by = c("City", "State"))
all.city <- merge(all.city, city.output, by = c("City", "State"))
all.city <- merge(all.city, city.popns, by = c("City", "State"))

## City level controls from state data
city.controls.stateLevel <- all.state[, c("State", "urbanPopAbove2500", "urbanPopAbove25000", "personalIncomeLindert1910", "totalPersonsOccupied", "agricultureOccupied", "manufacturingOccupied")]
names(city.controls.stateLevel)[-1] <- paste0("State_", names(city.controls.stateLevel)[-1]) #add state at the start of the name to avoid confusion
all.city <- merge(all.city, city.controls.stateLevel, by = "State", all.x = T)

#Bring state controls to city
all.city <- merge(all.city, all.state[,c('State','StatePop1910','totalPop')], by='State')
all.city.stata <- all.city # This is to keep spaces in variable names in R file
names(all.city.stata) <- gsub("\\.", "", gsub(" ", "", names(all.city)))

# Save as .dta
save.dta13(all.city.stata, file=paste0(root.path,'/data/output/all_city.dta'))
