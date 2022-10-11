#### ##################################################################### ####
####                       Load the SOEP data                              ####
#### ##################################################################### ####
#### _____________________________________________________________________ ####
#### Load the initial data on PEQUIV variables ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()
#### Load the relevant SOEP 35 data sets ####
codebook = read_excel("01_RawData/02_codebooks/codebook.xlsx", sheet = "pequiv")
data = list.files("01_RawData/01_Soep35", pattern = "pequiv", full.names = T) %>% lapply(., read_dta)
#### Bind the all the list elements ####
data = rbindlist(data)
#### Only keep the variables in the Code Book ####
data = data %>% select(., one_of(codebook$soep_name))
#### Rename the variables to the code book ####
data = data %>% rename_at( colnames(data), ~codebook$study_name)
#### Only keep data from 2005 ####
data = filter(data, year >= 2005)
#### Review the data set ####
data %>% head(.)
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
codebook = read_excel("01_RawData/02_codebooks/codebook.xlsx", sheet = "hgen")
data = list.files("01_RawData/01_Soep35", pattern = "hgen", full.names = T) %>% lapply(., read_dta)
#### Only keep the variables in the Code Book ####
data = rbindlist(data) %>% select(., one_of(codebook$soep_name))
#### Rename the variables to the code book ####
data = data %>% rename_at( colnames(data), ~codebook$study_name)
#### Only keep data from 2005 ####
data = filter(data, year >= 2005)
#### Review the data set ####
data %>% head(.)
#### Save the data set ####
write_rds(data, file = "02_GenData/03_SoepFiles/hgen.rds", compress = "gz")
#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Load the initial data on HCONSUM variables ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()
#### Load the relevant SOEP 35 data sets ####
ind = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")
codebook = read_excel("01_RawData/02_codebooks/codebook.xlsx", sheet = "hconsum2")
data = list.files("01_RawData/01_Soep35", pattern = "hconsum", full.names = T) %>% 
  lapply(., read_dta) %>% rbindlist(.)
#### Filter to keep the imputed and smoothed data ####
data = select(data, !contains(c("f","_a", paste0(seq(1, 17, 1), "a")))) |> 
  select(-c(cid, hid, syear, hhnr)) |>  rename(hid = hhnrakt)
#### Transform to long format ####
data = gather(data, var, value, -hid)
data = arrange(data, hid, var)
#### Extract the variable name get the consumption concept ####
subs = paste(c("b","consum","c","d","e"), collapse = "|")
data = mutate(data, code = ifelse(grepl("consum", var), gsub(subs, "", var), var))
data = mutate(data, code = ifelse(!grepl("consum", var), gsub("_.*", "", var), code))
data = left_join(data, codebook |>  select(-group))
#### Transform the owner variables to monthly euros ####
data = mutate(data, value = ifelse(grepl("oheat|oelectr|outil", var), value/12, value))
#### Aggregate the individual data to the household level ####
ind = ind |>  ungroup() |> filter(year == 2010) |> 
  select(hid, income, owner) |> distinct() |> group_by(hid) |> 
  summarise_all(max, na.rm = T)
#### Add income and ownership to the consumption data ####
data = left_join(data, ind)
#### Only keep one of the rent or own expenditure variables for utilities ####
data = split(data, f = data$hid) %>% 
  map(function(x) 
    if (unique(x$owner) == 1) 
      filter(x, !grepl("rheat|reletr", var)) 
    else filter(x, !grepl("oheat|oeletr", var))) %>%
  bind_rows()
#### Aggregate across expenditure concepts ####
data = data |> group_by(hid, income, owner, concept) |> summarise(value = mean(value))
#### Transform to yearly average and thousand euros ####
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
ind = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")
codebook = read_excel("01_RawData/02_codebooks/codebook.xlsx", sheet = "hwealth")
data = list.files("01_RawData/01_Soep35", pattern = "hwealth", full.names = T) %>% 
  lapply(., read_dta) %>% rbindlist(.)
colnames(data)
#### Extract the wealth from the primary residence ####
prop = select(data, hid = hhnrakt, year = syear, contains(c("w010","w001", "w011","p010","p001", "p011"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T)) %>%
  left_join(., select(codebook, var, VarText))
#### Extract the wealth from other residences ####
OtherProp = select(data, hid = hhnrakt, year = syear, contains(c("e010","e001", "e011"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))
#### Extract the value of financial assets ####
financial = select(data, hid = hhnrakt, year = syear, contains(c("f010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))
#### insurance and Loan Contracts ####
LoanContracts = select(data, hid = hhnrakt, year = syear, contains(c("i010", "h010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))
#### business ####
business = select(data, hid = hhnrakt, year = syear, contains(c("b010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))
#### Tangible assets ####
tangible = select(data, hid = hhnrakt, year = syear, contains(c("t010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))
#### Consumer Debts assets ####
ConDebt = select(data, hid = hhnrakt, year = syear, contains(c("c010"))) %>% 
  gather(., var, value, -hid, -year) |> mutate(value = ifelse(value == -8, NA, value)) |> 
  mutate(imp = substr(var, 6,6)) |> 
  mutate(var = substr(var, 1,5))|> group_by(hid, year, var) |> 
  summarise(value = mean(value, na.rm = T))%>%
  left_join(., select(codebook, var, VarText))
#### Other ####
other = select(data, hid = hhnrakt, year = syear, contains(c("s010", "v010", "n010", "n011", "n001"))) %>% 
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
ind = ind |>  ungroup() |> filter(year %in% c(2002,2007, 2012, 2017)) |> 
  select(hid, income, owner, age) |> distinct() |> group_by(hid) |> 
  summarise_all(max, na.rm = T)
#### Add income and ownership to the consumption data ####
data = left_join(data, ind)
#### Arrange the data set ####
colnames(data)
data = select(data, c(hid, year, income, owner, age, GrossWealth, GrossWealth2, GrossDebts, 
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
#### Join the data sets ####
#### _____________________________________________________________________ ####
#### Clear the data set ####
rm(list = ls()); gc()
#### Load the data in a list format ####
data = list(pequiv = read_rds("02_GenData/03_SoepFiles/pequiv.rds"), 
            hgen = read_rds("02_GenData/03_SoepFiles/hgen.rds"), 
            lodge = read_rds("02_GenData/03_SoepFiles/LodgeData.rds"))
#### Full join the data ####
data = reduce(data, left_join)
#### Only keep relevant covariates ####
data = select(data, -c(sample, MaritalStatus, industry2, region, AcqMeans, HouseCondition, 
                       AlarmSystem, IndoorToilet, kitchen, IndoorShower, InterviewMonth, 
                       HouseType1, HouseType2, subsidies, year_moved))
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
#### Only keep the occupation of the HH head ####
data = data %>%  ungroup() %>% filter(head == 1) |> 
  group_by(pid, hid, year) |> mutate(count = n()) |> 
  select(hid, pid, year, industry = industry1) %>% 
  left_join(data, .)
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
#### See the data set ####
data = select(data, -NumLodge, -LodgeEnglish)
#### Glimpse the data ####
glimpse(data)
#### Save the data set ####
write_rds(data, file = "02_GenData/03_SoepFiles/RawSoep.rds", compress = "gz")
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
data = data |> ungroup() |> select(year, hid, income, PowerCosts, head) %>% filter(year == 2017, head == 1) %>%
  group_by(year, hid) %>% summarise(income = mean(income, na.rm = T), PowerCosts = mean(PowerCosts, na.rm = T)) 
#### Compute the income and consumption deciles ####
data = data %>% ungroup() %>% 
  mutate(IncomeGroup = cut2(income, g = 10)) %>% 
  filter(is.na(IncomeGroup) == F) %>% 
  filter(income < quantile(income, p = .975), income > quantile(income, p = 0.025))
#### Aggregate average income and consumption ####
decile = data %>% group_by(IncomeGroup) %>% 
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
  theme(legend.title = element_blank()) + labs(x = "Power Costs (Log)", y = "")
#### Review the data set ####
decile %>% head(.)
#### Save the data set ####
ggsave("WebsiteABM/99_figures/PowerCostsDensity.png")
write_rds(decile, file = "02_GenData/03_SoepFiles/IncomePowerDeciles.rds", compress = "gz")
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
#### Transform income to monhtly in thousands ####
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
#### Compute the distribution of consumption across income quintiles ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()
#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")
consum = read_rds("02_GenData/03_SoepFiles/FullConsum.rds")
#### Only select the income and consumption data ####
data = select(data, year, pid, hid, income) %>% filter(year == 2010) %>%
  group_by(year, hid) %>% summarise(income = mean(income, na.rm = T)) %>% 
  left_join(., consum)
#### Transform income to monthly in thousands ####
data = mutate(data, income = (income/12)*1000)
#### Compute the income and consumption deciles ####
data = data %>% ungroup() %>% 
  mutate(IncomeGroup = cut2(income, c(0, 500, 900, 1300, 1500, 1700, 2000, 2600, 3600, 5000, 7500, 10000)))
#### Compute the share of comsumption ####
data = data |> filter(income > 0) |> mutate(ConShare = (consumption/income)*100)
#### Aggregate average income and consumption ####
decile = data %>% group_by(IncomeGroup) %>% 
  summarise(IncAvg = mean(income, na.rm = T), 
            ConAvg = mean(consumption, na.rm = T), 
            ConSd = sd(consumption, na.rm = T), 
            ConShareAvg = mean(ConShare, na.rm = T), 
            ConShareSd = sd(ConShare, na.rm = T))
#### Plot the income and consumption (share) relationship per decile ####
ggplot(data) + geom_density(aes(x = log(ConShare), fill = IncomeGroup), alpha = 0.25) +
  grids()+ theme_economist() %+replace% 
  theme(legend.title = element_blank()) + labs(x = "Power Costs (Log)", y = "")
ggsave("WebsiteABM/99_figures/ConsumptionDensityShare.png")
#### Plot the income and consumption relationship per decile ####
ggplot(data) + geom_density(aes(x = log(consumption), fill = IncomeGroup), alpha = 0.25) +
  grids()+ theme_economist() %+replace% 
  theme(legend.title = element_blank()) + labs(x = "Power Costs (Log)", y = "")
ggsave("WebsiteABM/99_figures/ConsumptionDensity.png")
#### Review the data set ####
decile %>% head(.)
#### Save the data set ####
write_rds(decile, file = "02_GenData/03_SoepFiles/ConsumptionDeciles.rds", compress = "gz")
#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Compute the period-specific probability of power adoption conditional on ownership and house type ####
#### _____________________________________________________________________ ####
#### Clear the data set ####
rm(list = ls()); gc()
#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")
#### Select only Severin relevant variables ####
data = select(data, hid:YearMoved, state, age:employment, industry = industry1, 
              income, HealthSatisfaction, LifeSatisfaction, LodgeEnglish, lodge, AcqMeans:HouseSize,
              basement, garden, elevator, balcony, boiler:AlternativeEnergy, 
              FreeRent, owner, HseRawValue, mortgage, FinancialAssets, 
              GrossWealth:NetWealth, PowerRenter, PowerOwner, HeatingOwnerCosts:HeatingRenter)
#### Determine the age of the panel ####
data = data %>% ungroup() %>% group_by(pid, hid, YearMoved) %>% filter(pv == 1) %>% 
  filter(year == min(year)) %>% select(pid, hid, PanelYear = year, YearMoved) %>% left_join(data, .)
#### Aggregate to the household level ####
sum = data %>% group_by(year, hid, YearMoved) %>% 
  summarise(income = sum(income, na.rm = T), HouseType = unique(lodge), 
            owner = max(owner, na.rm = T), pv = max(pv, na.rm = T), 
            YearMoved = max(YearMoved, na.rm = T), PanelAge = min(PanelYear, na.rm = T)) %>% 
  mutate(pv = ifelse(is.infinite(pv), 0, pv))
#### Transform infinities in the panel's age to NAs ####
sum = mutate(sum, PanelAge = ifelse(is.infinite(PanelAge), NA, PanelAge))
#### Exclude households with full NAs in house type ####
sum = sum %>% group_by(hid, YearMoved) %>% mutate(HouseType = na.omit(unique(HouseType))[1])
#### Exclude households with full NAs in ownership ####
sum = sum %>% group_by(hid, YearMoved) %>% mutate(owner = na.omit(unique(owner))[1])
#### Only keep the year the pannel was acquired ####
sum = mutate(sum, pv = ifelse(PanelAge == year, 1, 0)) %>%
  mutate(pv = ifelse(is.na(pv) == T, 0, pv))
#### Filter NAs in house type or ownership ####
sum = filter(sum, is.na(HouseType) == F, is.na(owner) == F)
### Summarize by all the combinations of house type and owner per year ####
sum = sum %>% group_by(year, HouseType, owner) %>% summarise(obs = n(), pv = sum(pv))
#### Compute the probability of PV adoption ####
sum = mutate(sum, prob = (pv/obs)*100)
#### Review the data set ####
sum %>% arrange(desc(prob)) %>% head(.)
#### Save the data set ####
write_rds(sum, file = "02_GenData/03_SoepFiles/PvProbability.rds", compress = "gz")
#### Clear the space ####
rm(list = ls()); gc()
#### _____________________________________________________________________ ####
#### Estimate the wealth ####
#### _____________________________________________________________________ ####
#### Clear the space ####
rm(list = ls()); gc()
#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")
#### Only select the income and consumption data ####
data = arrange(data, hid, pid, year, education, Npersons) %>% 
  select(hid, pid, year, GrossWealth, income, age, education, Npersons) %>% group_by(hid, year) %>%
  summarise(income = mean(income, na.rm = T), wealth = mean(GrossWealth, na.rm = T), 
            age = max(age, na.rm = T), education = max(education, na.rm = T), Npersons = mean(Npersons, na.rm = T)) %>% 
  mutate_at(vars(income, wealth, age, education, Npersons), function(x) ifelse(is.infinite(x), NA, x)) %>%
  filter_at(vars(income, wealth, age, education, Npersons), function(x) is.na(x) == F)
#### Restrict data to the 95 percentile to avoid outliers driving the coefficients ####
data = filter(ungroup(data), wealth < quantile(wealth, p = 0.99, na.rm = T))
#### Estimate wealth across different models ####
spec = list(
  feols(wealth ~ income + age, data = data),
  
  feols(wealth ~ income + age + income^2 + age^2, data = data), 
  
  feols(wealth ~ income + age + income^2 + age^2 + education + Npersons, data = data), 
  
  feols(wealth ~ income + age + income^2 + age^2 + education + Npersons | year , data = data), 
  
  feols(wealth ~ income + age + income^2 + age^2 + education + Npersons | year + hid, data = data))
#### Run the regressions ####
est = lapply(spec, function(x) 
  summary(x))
#### Extract the coefficients ####
coef = lapply(spec, function(x) 
  summary(x) %>% coeftable(.) %>% as.data.frame(.) %>% 
    mutate(var = rownames(.))) %>% rbindlist(idcol = "spec")
#### Extract the fitted values ####
fitted = lapply(est, function(x) 
  data.frame(value = fitted(x), id = seq(1, length(fitted(x)), 1))) %>% 
  rbindlist(., idcol = "spec") %>% mutate(variable = "Model") %>% 
  mutate(variable = paste0(variable, spec)) %>% select(-spec) %>% 
  spread(., variable, value)
#### Bind the fitted values with the main data frame ####
data = data.frame(data, select(fitted, -id))
#### Plot the density funtion ####
plot = gather(data, var, value, -c(hid, year, income, age, education, Npersons))
#### Make the raw bar plot ####
plot %>% group_by(var) %>% filter(value < quantile(value, p = 0.99, na.rm = T)) %>% ggplot(.) + 
  geom_histogram(aes(value/1000, fill = var), alpha = 0.5, 
                 position =position_dodge2(width = 1.5), binwidth = 100) +
  theme(panel.background = element_blank(), legend.position = c(0.05, 0.85), 
        axis.line = element_line(), legend.background = element_blank(), 
        legend.title = element_blank()) +
  grids() + scale_fill_viridis_d(option = "D") +
  labs(y = "No. of Households", x = "Wealth in Ths.")
#### Make the restricted bar plot ####
plot %>% group_by(var) %>% filter(value < quantile(value, p = 0.99, na.rm = T)) %>% 
  mutate(value = ifelse(value == -8, 0, value)) %>% ggplot(.) + 
  geom_histogram(aes(value/1000, fill = var), alpha = 0.5, 
                 position =position_dodge2(width = 1.5), binwidth = 100) +
  theme(panel.background = element_blank(), legend.position = c(0.05, 0.85), 
        axis.line = element_line(), legend.background = element_blank(), 
        legend.title = element_blank()) +
  grids() + scale_fill_viridis_d(option = "D") +
  labs(y = "No. of Households", x = "Wealth in Ths.")
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
#### Clear the space ####
rm(list = ls()); gc()

#### _____________________________________________________________________ ####
#### Create a data set for the ABM ####
#### _____________________________________________________________________ ####
#### Clear the data set ####
rm(list = ls()); gc()
#### Load the data ####
data = read_rds("02_GenData/03_SoepFiles/RawSoep.rds")
wealth = read_rds("02_GenData/03_SoepFiles/WealthEst.rds")
GallupEnv = read_rds("02_GenData/05_Gallup/probit.rds")
#### Aggregate to the household level ####
head(data)
sum = data %>% group_by(year, hid, YearMoved, head, industry1, industry2) %>% 
  summarise(income = mean(income, na.rm = T), age = mean(age) %>% state = unique(state), HouseType = unique(lodge), 
            owner = unique(owner, na.rm = T), HouseSize = max(HouseSize, na.rm = T), 
            PowerCosts = mean(PowerCosts, na.rm = T), pv = max(pv, na.rm = T), 
            Npersons = unique(Npersons, na.rm = T),  Nkids = unique(Nkids, na.rm = T),
            PanelAge = min(PanelYear, na.rm = T), wealth = mean(GrossWealth, na.rm = T), 
            FinancialAssets = mean(FinancialAssets, na.rm = T))
#### Transform infinities in the panel's age to NAs ####
sum = mutate(sum, PanelAge = ifelse(is.infinite(PanelAge), NA, PanelAge))
#### Fill the house type and ownership for persons that did not moved and have NAs ####
sum = sum %>% group_by(hid, YearMoved) %>% mutate(HouseType = na.omit(unique(HouseType))[1])
sum = sum %>% group_by(hid, YearMoved) %>% mutate(owner = na.omit(unique(owner))[1])
#### Filter NAs in house type or ownership ####
sum = filter(sum, is.na(HouseType) == F, is.na(owner) == F)
#### When power costs are missing, fill them with the average ####
sum = sum %>% group_by(hid, YearMoved) %>% filter(is.na(mean(PowerCosts, na.rm = T)) == F) %>%
  mutate(PowerCosts = ifelse(is.na(PowerCosts), round(mean(PowerCosts, na.rm = T), 0), PowerCosts))
#### Aggregate all the data ####
sum = sum %>% group_by(hid, YearMoved) %>% summarise(IncomeSd = sd(income, na.rm = T), 
                                                     PowerCostsSd = sd(PowerCosts, na.rm = T)) %>% left_join(filter(sum, year == 2017), .)
#### Transform Power costs to annual values ####
sum = mutate_at(sum, vars(PowerCosts, PowerCostsSd), function(x) x = x*12)
#### Compute the age of the panel ####
sum = mutate(sum, PanelAge = 2018 - PanelAge) %>% 
  group_by(hid, YearMoved) %>% mutate(PanelAge = max(PanelAge, na.rm = T), pv = sum(pv, na.rm = T)) %>% 
  mutate(PanelAge = ifelse(is.infinite(PanelAge), NA, PanelAge))
#### Exclude the numbers from the state ids ####
sum = mutate(sum, state = gsub(".*\\] | .*", "", state))
#### Arrange the data ####
sum = select(sum, hid, YearMoved, state, industry1, industry2, income, IncomeSd, PowerCosts, PowerCostsSd,
             HouseType, owner, HouseSize, pv, PanelAge, Npersons, Nkids, wealth, FinancialAssets) %>% 
  distinct()
#### Check for duplicates ####
sum = sum %>% group_by(hid, YearMoved) %>% mutate(count = n())
#### Add the coefficients of the regression ####
sum = data.frame(sum, age_wealth_est = wealth$age_est, age_wealth_se = wealth$age_se,
                 income_wealth_est = wealth$income_est, income_wealth_se = wealth$income_se, 
                 intercept_wealth_est = wealth$Intercept_est, intercept_wealth_se = wealth$Intercept_se)
#### Create 1000 different list elements with replacement ####
set.seed(1243)
data = lapply(vector("list", 1000), function(x) sum[sample(nrow(sum), nrow(sum), replace=T), ])
data[[1]] = sum
#### Save the data set ####
write_rds(data, file = "02_GenData/03_SoepFiles/AbmSoepList.rds", compress = "gz")
#### Clear the space ####
rm(list = ls()); gc()
