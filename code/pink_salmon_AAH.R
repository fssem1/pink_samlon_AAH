#notes----
#author: Sara E Miller 
#contact: sara.miller@alaska.gov; 907-465-4245
#Last edited: July 31, 2018
#retrospective analysis for Taku sockeye inseason management 
  #standard model of 2 day sulk rate 
  #full model with no sulk rate
  #sulk model with data calculated sulk rates

# load ----
source("code/helper.R")
source("code/functions.R")

#data----
read.csv('data/BC_catch.csv') -> BC_catch
read.csv('data/AK_catch.csv') -> AK_catch
read.csv('data/strata_defs.csv') -> strata_defs 
read.csv('data/escape.csv') -> escapement

#data.clean----
escapement %>% rowwise() %>% 
  mutate(sum1 = sum(X101,X102,X103,X105,X106,X107,X108, na.rm=T),
                       sum2 = sum(X3,X1,X4,X5, na.rm=T)) %>% 
  mutate(expanded_by_2.5 = old_SEAK_index * 2.5,
              new_SEAK_index_expanded_by_2.5 = sum1 * 2.5,
              BC_index_expanded_by_1 = sum2 * 1) %>%
dplyr::select(year = year,
              old_SEAK_index = old_SEAK_index,
              old_SEAK_index_expanded_by_2.5 = expanded_by_2.5,
              X101 = X101,
              X102 = X102,
              X103 = X103,
              X105 = X105,
              X106 = X106,
              X107 = X107,
              X108 = X108,
              US_sum= sum1,
              new_SEAK_index_expanded_by_2.5 = new_SEAK_index_expanded_by_2.5,
              X3 = X3,
              X1 = X1,
              X4 = X4,
              X5 = X5,
              BC_sum = sum2,
              BC_index_expanded_by_1   = BC_index_expanded_by_1) -> escapement  
write.csv(escapement, "data/processed/escapement.csv")

replace(BC_catch, is.na(BC_catch), 0) -> BC_catch
  
BC_catch %>% melt(id=c("Area", "Period", "Gear", "District"), na.rm=TRUE) %>% 
  mutate (country = 'BC', 
          year = gsub("X", "", variable)) %>% 
  dplyr::select(country = country, 
                year = year,
                area = Area,
                period = Period,
                gear = Gear,
                district = District,
                catch  = value)  -> BCcatch

replace(AK_catch, is.na(AK_catch), 0) -> AK_catch
AK_catch %>% melt(id=c("Area", "Period", "Gear", "District"), na.rm=TRUE) %>% 
  mutate (country = 'AK', 
          year = gsub("X", "", variable)) %>% 
  dplyr::select(country = country, 
                year = year,
                area = Area,
                period = Period,
                gear = Gear,
                district = District,
                catch  = value)  -> AKcatch

#create 1982,1984,1985 sheet  
#median <- apply(strata_defs[,c("X1982","X1984", "X1985")],1,median)
strata_defs %>%  melt(id=c("Region", "Area", "Period", "Gear", "Median",  "Adjusted_Mean", "Comments"), na.rm=TRUE) %>% 
  dplyr::select(country = Region,
                area = Area, 
                gear = Gear,
                period = Period,
                year = variable,
                median = Median,
                adj_mean = Adjusted_Mean,
                prop_AK_fish  = value) %>% 
  mutate (year = gsub("X", "", year)) -> strata

escapement %>% filter (year %in% c(1982, 1984, 1985)) %>%
  dplyr::select(year = year,
                old_SEAK_index_expanded_by_2.5 = old_SEAK_index_expanded_by_2.5, 
                BC_index_expanded_by_1 =  BC_index_expanded_by_1) -> escape_filter
BCcatch %>% filter (year %in% c(1982, 1984, 1985)) -> BC_catch_filter
AKcatch %>% filter (year %in% c(1982, 1984, 1985)) -> AK_catch_filter
prop_1982_1984_1985 <- rbind(BC_catch_filter, AK_catch_filter)

merge_files <- merge(prop_1982_1984_1985, strata, by=c("country", "year", "area", "period", "gear"), all.x=TRUE)
merge_files  %>%  dplyr::select(country = country, 
                                    year = year,
                                    area = area,
                                    period = period,
                                    gear = gear,
                                    district = district,
                                    catch  = catch, 
                                    prop_AK_fish = prop_AK_fish) %>% 
mutate (prop_BC_fish = 1- prop_AK_fish)-> prop_1982_1984_1985


merge_files <- merge(prop_1982_1984_1985, escape_filter, by=c("year"), all.x=TRUE)
merge_files %>% mutate (esc_AK = old_SEAK_index_expanded_by_2.5,
                        esc_BC = BC_index_expanded_by_1) -> merge_files
merge_files %>% filter (year == 1982) %>%
  mutate (abund_AK = sum(catch * prop_AK_fish, na.rm=T) + esc_AK,
  abund_BC = sum(catch * prop_BC_fish, na.rm=T) + esc_BC)  -> merge_1982
merge_files %>% filter (year == 1984) %>%
  mutate (abund_AK = sum(catch * prop_AK_fish, na.rm=T) + esc_AK,
          abund_BC = sum(catch * prop_BC_fish, na.rm=T) + esc_BC)  -> merge_1984
merge_files %>% filter (year == 1985) %>%
  mutate (abund_AK = sum(catch * prop_AK_fish, na.rm=T) + esc_AK,
          abund_BC = sum(catch * prop_BC_fish, na.rm=T) + esc_BC)  -> merge_1985
merge <- rbind(merge_1982, merge_1984)
merge <- rbind(merge, merge_1985)
merge %>% dplyr::select(country = country, 
              year = year,
              area = area,
              period = period,
              gear = gear,
              district = district,
              catch  = catch, 
              prop_AK_fish = prop_AK_fish,
              prop_BC_fish = prop_BC_fish,
              esc_AK = esc_AK,
              abund_AK = abund_AK,
              esc_BC = esc_BC,
              abund_BC = abund_BC) -> final_merge
write.csv(final_merge, "data/processed/prop_1982_1984_1985.csv")
read.csv('data/processed/prop_1982_1984_1985.csv') -> prop_1982_1984_1985
prop_1982_1984_1985 %>% dplyr::select(country = country, 
                        year = year,
                        abund_AK = abund_AK,
                        abund_BC = abund_BC) -> prop_1982_1984_1985

prop_1982_1984_1985 %>% filter(country =='AK') %>%
  summarise(Median=median(abund_AK)) -> median_AK_828485
prop_1982_1984_1985 %>%
  filter (country =='BC') %>%
  summarise(Median=median(abund_BC)) -> median_BC_828485
median_AK_828485 = as.numeric(median_AK_828485)
median_BC_828485 = as.numeric(median_BC_828485)

#create JIC calc sheet
strata_defs  %>% dplyr::select(country = Region,
                area = Area, 
                gear = Gear,
                period = Period,
                prop_AK_fish = Median) -> strata

BCcatch %>% filter (year %in% c(1999:2017)) -> BC_catch_filter
AKcatch %>% filter (year %in% c(1999:2017)) -> AK_catch_filter
JIC_calc <- rbind(BC_catch_filter, AK_catch_filter)
merge_files <- merge(JIC_calc,strata, by=c("country", "area", "period", "gear"), all.x=TRUE)
merge_files %>% mutate(prop_BC_fish =  1 - prop_AK_fish) -> JIC_calc

escapement %>% dplyr::select(year = year,
                             AK_esc = new_SEAK_index_expanded_by_2.5, 
                             BC_esc =  BC_index_expanded_by_1) -> esc
JIC_calc <- merge(JIC_calc, esc, by=c("year"), all.x=TRUE)

merge_files_by_year <-function(y){ 
  JIC_calc %>% filter (year == 2016) %>%
    mutate(abund_AK = sum(catch * prop_AK_fish, na.rm=T) + AK_esc,
           abund_BC = sum(catch * prop_BC_fish, na.rm=T) + BC_esc,
           prop_AK_fish_adj = round((prop_AK_fish * (abund_AK)/median_AK_828485)/(prop_AK_fish*(abund_AK)/median_AK_828485+(1-prop_AK_fish)*(abund_BC)/median_BC_828485),4),
           prop_BC_fish_adj = round(1-prop_AK_fish_adj,6),
           abund_AK_adj = sum(catch * prop_AK_fish_adj, na.rm=T) + AK_esc,
           abund_BC_adj = sum(catch * prop_BC_fish_adj, na.rm=T) + BC_esc) %>%
    mutate(prop_AK_fish_adj = round((prop_AK_fish * abund_AK_adj/median_AK_828485)/(prop_AK_fish*abund_AK_adj/median_AK_828485+(1-prop_AK_fish)*abund_BC_adj/median_BC_828485),4),
                   prop_BC_fish_adj = round(1-prop_AK_fish_adj,6),
                   prop_BC_fish_adj = round(1-prop_AK_fish_adj,6),
                   abund_AK_adj = sum(catch * prop_AK_fish_adj, na.rm=T) + AK_esc,
                   abund_BC_adj = sum(catch * prop_BC_fish_adj, na.rm=T) + BC_esc) %>%
    rowwise() %>% mutate(by_strata_unnum= ifelse(country == "AK", catch * prop_BC_fish, catch * prop_AK_fish),
                         by_strata_adjnum= ifelse(country == "AK", catch * prop_BC_fish_adj, catch * prop_AK_fish_adj))-> merge2          
write.csv(merge2, paste0('./data/processed/',y , '_','abundance_esc.csv'))}


