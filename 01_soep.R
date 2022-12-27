#### ##################################################################### ####
####                       Load the SOEP data                              ####
#### ##################################################################### ####
#### _____________________________________________________________________ ####
#### Load the initial data on PEQUIV variables ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load the relevant SOEP 37 data sets ####
codebook = read_excel("01_RawData/03_codebooks/codebook.xlsx", sheet = "pequiv")
data = list.files("01_RawData/02_Soep37", pattern = "pequiv", full.names = T) %>% lapply(., read_dta)

#### Bind the all the list elements ####
data = rbindlist(data)

#### Only keep the variables in the Code Book ####
data = data %>% select(., one_of(codebook$soep_name))

#### Rename the variables to the code book ####
data = data %>% rename_at(colnames(data), ~codebook$study_name)

#### Only keep data from 2005 ####
data = filter(data, year >= 2005)

#### Review the data set ####
glimpse(data)

#### Save the data set ####
write_rds(data, file = "02_GenData/03_SoepFiles/pequiv.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Load the initial data on HGEN variables ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load the relevant SOEP 35 data sets ####
codebook = read_excel("01_RawData/03_codebooks/codebook.xlsx", sheet = "hgen")
data = list.files("01_RawData/02_Soep37", pattern = "hgen", full.names = T) %>% lapply(., read_dta)

#### Only keep the variables in the Code Book ####
data = rbindlist(data) %>% select(., one_of(codebook$soep_name))

#### Rename the variables to the code book ####
data = data %>% rename_at( colnames(data), ~codebook$study_name)

#### Only keep data from 2005 ####
data = filter(data, year >= 2005)

#### Review the data set ####
glimpse(data)

#### Save the data set ####
write_rds(data, file = "02_GenData/03_SoepFiles/hgen.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Load the initial data on HCONSUM variables ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load the relevant SOEP 37 data sets ####
pequiv = read_rds("02_GenData/03_SoepFiles/pequiv.rds")
hgen = read_rds("02_GenData/03_SoepFiles/hgen.rds")
codebook = read_excel("01_RawData/03_codebooks/codebook.xlsx", sheet = "hconsum2")
data = list.files("01_RawData/02_Soep37", pattern = "hconsum", full.names = T) %>% 
  lapply(., read_dta) %>% rbindlist(.)


#### Filter to keep the imputed and smoothed data ####
data = select(data, !contains(c("f","_a", paste0(seq(1, 17, 1), "a")))) |> 
  select(-cid, -syear)

#### Transform to long format ####
data = gather(data, var, value, -hid) |> arrange(hid, var)


#### Extract the variable name get the consumption concept ####
subs = paste(c("b","consum","c","d","e"), collapse = "|")
data = mutate(data, code = ifelse(grepl("consum", var), gsub(subs, "", var), var))
data = mutate(data, code = ifelse(!grepl("consum", var), gsub("_.*", "", var), code))
data = left_join(data, codebook |>  select(-group))

#### Transform the owner variables to monthly euros ####
data = mutate(data, value = ifelse(grepl("oheat|oelectr|outil", var), value/12, value))

#### Aggregate the individual data to the household level ####
pequiv = pequiv |> left_join(hgen) |>  ungroup() |> filter(year == 2010) |> 
  select(hid, income, owner) |> distinct() |> group_by(hid) |> 
  summarise_all(max, na.rm = T)

#### Add income  to the consumption data ####
data = left_join(data, pequiv)

#### Only keep one of the rent or own expenditure variables for utilities ####
data = split(data, f = data$hid) %>% 
  map(function(x) 
    if (unique(x$owner) == 1) 
      filter(x, !grepl("rheat|reletr", var)) 
    else filter(x, !grepl("oheat|oeletr", var))) %>%
  bind_rows()

#### Aggregate across expenditure concepts ####
data = data |> group_by(hid, income, owner, concept) |> summarise(value = mean(value))

#### Transform to yearly consumption and thousand euros ####
data = mutate(data, value = (value*12)/1000)

#### Spread the data set ####
data = spread(data, concept, value) |> ungroup()

#### Add to the aggregate consumption
data$total = rowSums(select(ungroup(data), -hid, -income, -owner))

#### Review the data set ####
glimpse(data)

#### Save the data set ####
write_rds(data, file = "02_GenData/03_SoepFiles/hconsum.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Load the initial data on HWEALTH variables ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load the relevant SOEP 35 data sets ####
pequiv = read_rds("02_GenData/03_SoepFiles/pequiv.rds")
codebook = read_excel("01_RawData/03_codebooks/codebook.xlsx", sheet = "hwealth")
data = list.files("01_RawData/02_Soep37", pattern = "hwealth", full.names = T) %>% 
  lapply(., read_dta) %>% rbindlist(.)

#### Extract the wealth from the primary residence ####
prop = select(data, hid, year = syear, contains(c("w010","w001", "w011","p010","p001", "p011"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T)) %>%
  left_join(., select(codebook, var, VarText))

#### Extract the wealth from other residences ####
OtherProp = select(data, hid, year = syear, contains(c("e010","e001", "e011"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))

#### Extract the value of financial assets ####
financial = select(data, hid, year = syear, contains(c("f010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))

#### insurance and Loan Contracts ####
LoanContracts = select(data, hid, year = syear, contains(c("i010", "h010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))

#### business ####
business = select(data, hid, year = syear, contains(c("b010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))

#### Tangible assets ####
tangible = select(data, hid, year = syear, contains(c("t010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))

#### Consumer Debts assets ####
ConDebt = select(data, hid, year = syear, contains(c("c010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))

#### Other ####
other = select(data, hid, year = syear, contains(c("s010", "v010", "n010", "n011", "n001"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))

#### Aggregate the different data sets ####
data = rbindlist(list(prop, OtherProp, financial, LoanContracts, tangible, business,
                      ConDebt, other)) 

#### Transform negative values to NA ####
data = data |> group_by(hid, year, var = VarText) |> summarise(value = sum(value, na.rm = T)) %>% 
  spread(., var, value)

#### Aggregate the individual data to the household level ####
pequiv = pequiv |>  ungroup() |> filter(year %in% c(2002,2007, 2012, 2017)) |> 
  select(hid, income, age) |> distinct() |> group_by(hid) |> 
  summarise_all(max, na.rm = T)

#### Add income and ownership to the consumption data ####
data = left_join(data, pequiv)

#### Arrange the data set ####
data = select(data, c(hid, year, income, age, GrossWealth, GrossWealth2, GrossDebts, 
                      GrossDebts2, NetWealth, NetWealth2, HouseRaw, 
                      HouseDebt, HouseNet, HouseRaw2, HouseNet2,HouseDebt2, 
                      financial, insurances, tangible, business, ConsumerDebt, 
                      StudentLoans, CarsValue))

#### Review the data set ####
glimpse(data)

#### Save the data set ####
write_rds(data, file = "02_GenData/03_SoepFiles/hwealth.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Join the data sets at the individual level ####
#### _____________________________________________________________________ ####

#### Clear the data set ####
rm(list = ls()); gc()

#### Load the data in a list format ####
wealth = read_rds("02_GenData/03_SoepFiles/hwealth.rds")
data = list(pequiv = read_rds("02_GenData/03_SoepFiles/pequiv.rds"), 
            hgen = read_rds("02_GenData/03_SoepFiles/hgen.rds"), 
            lodge = read_rds("02_GenData/03_SoepFiles/LodgeData.rds"))

#### Full join the data ####
data = reduce(data, left_join)

#### Only keep relevant covariates ####
data = select(data, -c(sample, MaritalStatus, region, AcqMeans, HouseCondition, 
                       HouseholdType, subsidies, year_moved))


#### Transform the numeric variables from haven labelled to numeric ####
data = mutate_at(data, vars(age, Npersons, Nkids, education, employment, income, GrossIncome, 
                            NewestConstDate, OldestConstDate, PowerCosts, ac, basement,  OtherUtilityCosts, WarmWaterCosts,
                            YearMoved, FreeRent, HouseSize, GasCosts), function(x)  x = ifelse(x < 0, NA, x))

#### Transform the indicator variables to binary ####
data = mutate_at(data, vars(ac, basement, FloorHeating, 
                            garden, CentralFloorHeating, ThermalInsulation, 
                            elevator, DoubleGlazing, AlternativeEnergy, pv, 
                            balcony, boiler), function(x) x = ifelse(x == 2, 0, x))

#### Transform SOEP negative values to NAs in labelled variables ####
data = mutate(data, sex = ifelse(sex < 0, NA, sex)) |> 
  mutate(sex = labelled(sex, c(female = 2, male = 1)))

data = mutate(data, head = ifelse(head < 0, NA, head)) |> 
  mutate(head = labelled(head, c(Head = 1, Parner = 2, Child = 3, Relative = 4, Nonrelative = 5)))

data = mutate(data, owner = ifelse(owner < 0, NA, owner)) |> 
  mutate(head = labelled(owner, c(Owner = 1, MainTenant = 2, SubTenant = 3, tenant = 4, other = 5)))

data = mutate(data, occupation = ifelse(occupation < 0, NA, occupation))

#### Transform income to Thousand of Euros ####
data = mutate(data, income = income/1000, GrossIncome = GrossIncome/1000)

#### Add the house type and ownership when a household did not moved and has an NA ####
data = data %>% group_by(pid, hid, YearMoved) %>% mutate(owner = na.omit(unique(owner))[1])

#### Transform the character NA in English lodge to character vector ####
data = mutate(data, LodgeEnglish = ifelse(LodgeEnglish == "NA", NA, LodgeEnglish))

#### Assume that if the lodge has a garden, it is a house ####
data = mutate(data, LodgeEnglish = ifelse(is.na(LodgeEnglish) == T & garden == 1, "House", LodgeEnglish))

#### Assume that if the lodge has an elevator is a flat ####
data = mutate(data, LodgeEnglish = ifelse(is.na(LodgeEnglish) == T & elevator == 1, "Flat", LodgeEnglish))

#### Transform the PV data from one when bought to one if owned ####
data = mutate(data, pv = as.numeric(pv)) %>% 
  arrange(pid, year) %>% group_by(pid, YearMoved) %>% mutate(pv = cumsum(pv)) %>%
  mutate(pv = ifelse(pv > 0, 1, 0))

#### Determine the age of the panel ####
data = data %>% ungroup() %>% group_by(pid, hid, YearMoved) %>% filter(pv == 1) %>% 
  filter(year == min(year)) %>% select(pid, hid, PanelYear = year, YearMoved) %>% left_join(data, .)

#### Check the completeness of PV panels ####
data = data %>% group_by(pid, hid, YearMoved) %>% mutate(pv = max(na.omit(unique(pv)))) %>%
  filter(is.infinite(pv) == F)

#### Simplify the House-flats dummy variable ####
data = mutate(data, lodge = ifelse(grepl("Flats|Flat", LodgeEnglish), "Flats", 
                                   ifelse(is.na(LodgeEnglish) == T, NA, "House")))

#### Add the lodge type when a household did not moved and has an NA ####
data = data %>% group_by(pid, hid, YearMoved) %>% mutate(lodge = na.omit(unique(lodge))[1])

#### Simplify the ownership variable ####
data = mutate(data, owner = ifelse(owner %in% c(2,5,3,4), 0, owner))

#### Take away not-necessary´columns ####
data = select(data, -NumLodge, -LodgeEnglish)

#### Glimpse the data ####
glimpse(data)

#### Save the data set ####
write_rds(data, file = "02_GenData/03_SoepFiles/RawSoep.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()


#### _____________________________________________________________________ ####
#### Calculate the income percentile of the "Bundesbank" ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")

#### Only select the income and consumption data ####
data = select(data, year, pid, hid, income) %>% filter(year == 2017) %>%
  group_by(year, hid) %>% summarise(income = mean(income, na.rm = T)) 

#### Transform income to Monthly in thousands ####
data = mutate(data, income = (income/12)*1000)

#### Compute the income and consumption deciles ####
data = data %>% ungroup() %>% filter(is.na(income) == F) %>%
  mutate(amt = cut2(income, c(0, 500, 900, 1300, 1500, 1700, 
                              2000, 2600, 3600, 5000, 7500, 10000)))

#### Compute the lower and higher bounds of the amt ####
data = mutate(data, LowAmt = gsub(",.*|\\(| |\\[", "", amt),
              HighAmt = gsub(".*,|\\)| |\\]", "", amt)) %>% 
  mutate_at(vars(LowAmt, HighAmt), function(x) x = as.numeric(x))

#### Calculate the percentile of each high and low limit ####
per = mutate(data, LowPer = ecdf(income)(LowAmt), HighPer = ecdf(income)(HighAmt)) %>%
  ungroup() %>% select(amt:HighPer) %>% distinct()

#### Round the percentile ####
per = mutate_at(ungroup(per), vars(HighPer, LowPer), function(x) x = round(x*100, 0))

#### Review the data set ####
per %>% head(.)

#### Save the data set ####
write_rds(per, file = "02_GenData/03_SoepFiles/AmtMap.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Estimate the wealth ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")
wealth = read_rds("02_GenData/03_SoepFiles/hwealth.rds")

#### Only select the income and consumption data ####
data = data %>% ungroup() %>% arrange(hid, pid, year, education, Npersons) %>% 
  select(hid, pid, year, income, age, education, Npersons) %>% group_by(hid, year) %>%
  summarise(education = max(education, na.rm = T), Npersons = mean(Npersons, na.rm = T)) %>% 
  mutate_at(vars(education, Npersons), function(x) ifelse(is.infinite(x), NA, x)) %>%
  filter_at(vars(education, Npersons), function(x) is.na(x) == F)

#### Include the wealth data ####
data = left_join(wealth, data)

#### Transform income to thousand ####
data = mutate(data, wealth)

#### Restrict data to the 95 percentile to avoid outliers driving the coefficients ####
data = filter(ungroup(data), GrossWealth < quantile(GrossWealth, p = 0.99, na.rm = T))

data = filter(data, is.na(income) == F, is.na(age) == F, is.na(education) == F)

data = select(data, c(hid:age, GrossWealth, education))
#### Estimate wealth across different models ####
est = list(
  feols(GrossWealth ~ income + age, data = data),
  
  feols(GrossWealth ~ income + age + income^2 + age^2, data = data), 
  
  feols(GrossWealth ~ income + age + income^2 + age^2 + education, data = data), 
  
  feols(GrossWealth ~ income + age + income^2 + age^2 + education  | year, data = data), 
  
  feols(GrossWealth ~ income + age + income^2 + age^2 + education | year + hid, data = data))


#### Extract the coefficients ####
coef = lapply(est, function(x) 
  summary(x) %>% coeftable(.) %>% as.data.frame(.) %>% 
    mutate(var = rownames(.))) %>% rbindlist(idcol = "spec")

#### Extract the fitted values ####
fitted = lapply(est, function(x) 
  data.frame(value = fitted(x, na.rm = T), id = seq(1, length(fitted(x)), 1))) %>% 
  rbindlist(., idcol = "spec") %>% mutate(variable = "Model") %>% 
  mutate(variable = paste0(variable, spec)) %>% select(-spec) %>% 
  spread(., variable, value)

#### Bind the fitted values with the main data frame ####
data = data.frame(data, select(fitted, -id))

#### Extract the coefficients for Severin #####
coef = filter(coef, spec == 1)

#### Transform from wide to long format ####
est = select(coef, spec, var, Estimate) %>%  mutate(var = gsub("\\(|\\)", "", var)) %>% 
  mutate(var = paste(var, "est", sep = "_")) %>% spread(., var, Estimate)

se = select(coef, spec, var, `Std. Error`) %>%  mutate(var = gsub("\\(|\\)", "", var)) %>% 
  mutate(var = paste(var, "se", sep = "_")) %>% spread(., var, `Std. Error`)

coef = cbind(est, se)

#### Save the point estimates####
write_rds(coef, file = "02_GenData/03_SoepFiles/WealthEst.rds", compress = "gz")
write_rds(data, file = "02_GenData/03_SoepFiles/WealthFitted.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Create a data set for the ABM ####
#### _____________________________________________________________________ ####
#### Clear the data set ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")
WealthEst = read_rds("02_GenData/03_SoepFiles/WealthEst.rds")
GallupEnv = read_rds("02_GenData/05_Gallup/probit.rds")
wealth = read_rds("02_GenData/03_SoepFiles/hwealth.rds")

#### Aggregate to the household level ####
sum = data %>% group_by(year, YearMoved, hid) %>% 
  summarise(income = mean(income, na.rm = T), age = max(age), HouseType = unique(lodge), 
            PowerCosts = mean(PowerCosts, na.rm = T), pv = max(pv, na.rm = T), 
            PanelAge = min(PanelYear, na.rm = T), GrossIncome  = mean(GrossIncome, na.rm = T)) |> 
  distinct()


#### Transform infinities in the panel's age to NAs ####
sum = mutate(sum, PanelAge = ifelse(is.infinite(PanelAge), NA, PanelAge)) 

#### Fill the house type and ownership for persons that did not moved and have NAs ####
sum = sum %>% group_by(hid, YearMoved) %>% mutate(HouseType = na.omit(unique(HouseType))[1]) |> 
  distinct()

#### Filter NAs in house type or ownership ####
sum = filter(sum, is.na(HouseType) == F)

#### When power costs are missing, fill them with the average ####
sum = sum %>% group_by(hid, YearMoved) %>% 
  filter(is.na(mean(PowerCosts, na.rm = T)) == F) %>%
  mutate(PowerCosts = ifelse(is.na(PowerCosts), 
                             round(mean(PowerCosts, na.rm = T), 0), PowerCosts))

#### Only keep data for 2007 and 2017 ####
#sum = filter(sum, year %in% c(2007, 2017)) |> distinct()

#### Transform Power costs to annual values ####
sum = mutate_at(sum, vars(PowerCosts), function(x) x = x*12)

#### Compute the age of the panel ####
sum = sum |> ungroup() |> mutate(PanelYear = PanelAge, PanelAge = ifelse(year == 2017, 2017 - PanelAge, NA)) %>% 
  
  group_by(hid, year, YearMoved) %>% 
  
  mutate(PanelAge = max(PanelAge, na.rm = T), pv = sum(pv, na.rm = T)) %>% 
  
  mutate(PanelAge = ifelse(is.infinite(PanelAge), NA, PanelAge))


#### If the age of the panel is negative change pv to zero and PanelAge to NA ####
sum = mutate(sum, pv = ifelse(PanelYear > 2017, 0, pv))
sum = mutate(sum, PanelAge = ifelse(PanelYear > 2017, NA, PanelAge))

#### Check for duplicates ####
sum %>% group_by(hid, year) %>% summarise(count = n()) %>% filter(count > 1)
sum = select(sum, hid, year, YearMoved, GrossIncome, income:PanelAge)

### Add the wealth data ####
wealth = wealth |> select(hid, year, GrossWealth, NetWealth, GrossDebts) 
sum = left_join(sum, wealth)

#### Add the coefficients of the regression ####
sum = data.frame(sum, age_wealth_est = WealthEst$age_est, age_wealth_se = WealthEst$age_se,
                 income_wealth_est = WealthEst$income_est, income_wealth_se = WealthEst$income_se, 
                 intercept_wealth_est = WealthEst$Intercept_est, intercept_wealth_se = WealthEst$Intercept_se)


#### Check the final data set ####
glimpse(sum)

#### Create 1000 different list elements with replacement ####
#set.seed(1243)
#data = lapply(vector("list", 1000), function(x) sum[sample(nrow(sum), nrow(sum), replace=T), ])
#data[[1]] = sum

#### Save the data set ####
write_rds(sum, file = "02_GenData/03_SoepFiles/AbmSoep.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Compute the distribution of consumption across income quintiles ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/hconsum.rds")

#### Only select the income and consumption data ####
data = select(data, hid, income, total) %>% 
  mutate(income = income/1000)

#### Compute the income and consumption deciles ####
data = data %>% ungroup() %>% 
  mutate(IncomeGroup = ntile(income, n = 10))


#### Compute the share of consumption ####
data = data |> filter(income > 0) |> mutate(ConShare = (total/income)*100)

head(data)
#### Aggregate average income and consumption ####
decile = data %>% group_by(IncomeGroup) %>% 
  summarise(IncAvg = mean(income, na.rm = T), 
            ConAvg = mean(total, na.rm = T), 
            ConSd = sd(total, na.rm = T), 
            ConShareAvg = mean(ConShare, na.rm = T), 
            ConShareSd = sd(ConShare, na.rm = T))

#### Plot the income and consumption (share) relationship per decile ####
ggplot(data) + geom_density(aes(x = log(ConShare), fill = IncomeGroup), alpha = 0.25) +
  grids()+ theme_economist() %+replace% 
  theme(legend.title = element_blank()) + labs(x = "Share of Consumption (Log)", y = "")

#### Plot the income and consumption relationship per decile ####
ggplot(data) + geom_density(aes(x = log(total), fill = IncomeGroup), alpha = 0.25) +
  grids()+ theme_economist() %+replace% 
  theme(legend.title = element_blank()) + labs(x = "Consumption (Log)", y = "")


#### Review the data set ####
decile %>% head(.)

#### Save the data set ####
write_rds(decile, file = "02_GenData/03_SoepFiles/ConsumptionDeciles.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Compute the standard deviation of income and power consumption across deciles ####
#### _____________________________________________________________________ ####

#### Clear the space ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/RawSoep.rds") 

#### Only select the income and consumption data ####
data = data |> ungroup() |> select(year, hid, income, PowerCosts) %>% filter(year %in% c(2010, 2017)) %>%
  group_by(year, hid) %>% summarise(income = max(income, na.rm = T), PowerCosts = max(PowerCosts, na.rm = T)) |>
  mutate_at(vars(income, PowerCosts), function(x) ifelse(is.infinite(x) == T, NA, x))

#### Compute the income and consumption deciles ####
data = data %>% ungroup() %>% 
  mutate(IncomeGroup = cut2(income, g = 10)) %>% 
  filter(is.na(IncomeGroup) == F) %>% 
  filter(income < quantile(income, p = .975), income > quantile(income, p = 0.025))

#### Aggregate average income and consumption ####
decile = data %>% group_by(IncomeGroup, year) %>% 
  summarise(IncAvg = mean(income, na.rm = T), 
            PowerCostAvg = mean(PowerCosts, na.rm = T), 
            IncSd = sd(income, na.rm = T), 
            PowerCostSd = sd(PowerCosts, na.rm = T)) %>% 
  filter(is.na(IncomeGroup) == F)

#### Transform the power costs to yearly values ####
decile = mutate_at(decile, vars(PowerCostAvg, PowerCostSd), function(x) x = x*12)

#### Plot the income and consumption relationship per decile ####
ggplot(data) + geom_density(aes(x = log(PowerCosts), fill = IncomeGroup), alpha = 0.25) +
  grids()+ theme_economist() %+replace% 
  theme(legend.title = element_blank()) + labs(x = "Power Costs (Log)", y = "") +
  facet_wrap(~year)


#### Review the data set ####
glimpse(decile)

#### Save the data set ####
ggsave("WebsiteABM/99_figures/PowerCostsDensity.png", width = 8.5, height = 4.5)
write_rds(decile, file = "02_GenData/03_SoepFiles/IncomePowerDeciles.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Calculate the probability of PV adoption ####
#### _____________________________________________________________________ ####

#### Clear the data set ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/AbmSoep.rds")

#### Aggregate to the household level ####
sum = data %>% select(year, hid, YearMoved, pv, income)

#### Compute the income decile per year ####
sum = sum %>% ungroup() %>% group_by(year) %>%
  mutate(IncomeGroup = ntile(income, 10)) %>% 
  filter(is.na(IncomeGroup) == F)

#### Summarize by all the combinations of house type and owner per year ####
sum = sum %>% group_by(year, IncomeGroup) %>% summarise(obs = n(), pv = sum(pv, na.rm = T))

#### Compute the probability of PV adoption ####
sum = mutate(sum, prob = (pv/obs)*100) %>% 
  mutate(IncomeGroup = as.factor(IncomeGroup))

#### Review the data set ####
sum %>% arrange(desc(prob)) %>% head(.)

# Plot the probability of adoption for houses and flats
ggplot(sum) + geom_line(aes(y = prob, x = year, color = IncomeGroup, group = IncomeGroup)) + 
  geom_point(aes(y = prob, x = year, color = IncomeGroup, group = IncomeGroup))  + theme_economist() %+replace%
  theme(strip.text = element_text(hjust = 0), legend.title = element_blank()) +
  ggtitle("Total PV systems per year") +labs(x = "", y = "")

# Plot the probability of adoption for houses and flats
plot = sum |> group_by(year) |> summarise(obs = sum(obs), prob = mean(prob))

ggplot(plot) + geom_line(aes(y = prob, x = year)) + 
  geom_point(aes(y = prob, x = year))  + theme_economist() %+replace%
  theme(strip.text = element_text(hjust = 0), legend.title = element_blank()) +
  ggtitle("Total PV systems per year") +labs(x = "", y = "")


#### Save the data set ####
write_rds(sum, file = "02_GenData/03_SoepFiles/PvProbability.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Calculate the probability of PV for the balanced panel ####
#### _____________________________________________________________________ ####

#### Clear the data set ####
rm(list = ls()); gc()

#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/AbmSoep.rds")

data = data |> filter(year > 2006 & year < 2018) |> group_by(hid) |> mutate(count = n())
data = filter(data, count == 11)

#### Aggregate to the household level ####
sum = data %>% select(year, hid, YearMoved, pv, income)

#### Compute the income decile per year ####
sum = sum %>% ungroup() %>% group_by(year) %>%
  mutate(IncomeGroup = ntile(income, 10)) %>% 
  filter(is.na(IncomeGroup) == F)

#### Summarize by all the combinations of house type and owner per year ####
sum = sum %>% group_by(year, IncomeGroup) %>% summarise(obs = n(), pv = sum(pv, na.rm = T))

#### Compute the probability of PV adoption ####
sum = mutate(sum, prob = (pv/obs)*100) %>% 
  mutate(IncomeGroup = as.factor(IncomeGroup))

#### Review the data set ####
sum %>% arrange(desc(prob)) %>% head(.)

# Plot the probability of adoption for houses and flats
ggplot(sum) + geom_line(aes(y = prob, x = year, color = IncomeGroup, group = IncomeGroup)) + 
  geom_point(aes(y = prob, x = year, color = IncomeGroup, group = IncomeGroup))  + theme_economist() %+replace%
  theme(strip.text = element_text(hjust = 0), legend.title = element_blank()) +
  ggtitle("Total PV systems per year") +labs(x = "", y = "")

# Plot the probability of adoption for houses and flats
plot = sum |> group_by(year) |> summarise(obs = sum(obs), prob = mean(prob))

ggplot(plot) + geom_line(aes(y = prob, x = year)) + 
  geom_point(aes(y = prob, x = year))  + theme_economist() %+replace%
  theme(strip.text = element_text(hjust = 0), legend.title = element_blank()) +
  ggtitle("Total PV systems per year") +labs(x = "", y = "")


#### Save the data set ####
write_rds(sum, file = "02_GenData/03_SoepFiles/PvProbability.rds", compress = "gz")

#### Clear the space ####
rm(list = ls()); gc()

