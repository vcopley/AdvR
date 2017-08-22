##' Function to calculate carbon impact of change in active travel for HEAT
##' 
##' Takes three user-created data frames as input and returns carbon saving of additional
##' walking and/or cycling
##' 
##' @param df_ucc A data frame class object containing the use case criteria
##' @param df_qual A data frame class object containing the data qualifiers
##' @param df_adj_data A data frame class object containing mode distance data in km
##'  
##' @return A list with overall carbon impact of change in active travel and split by active mode
##' @author Christian Brand, Vicky Copley
##' @export
##' @keywords carbon
##' @examples
##' # Example usage of impact_carbon
##' results <- impact_carbon(df_ucc, df_qual, df_adj_data)
##' 

impact_carbon <- function(df_ucc, df_qual, df_adj_data) {

  # Load the required background data based upon the traffic conditions
  # The next line of code will eventually replace the dummy data loaded from the .csv
  # load(paste0('../data/background_data',df_qual$quali_traffic_condition,'.RData'))
  
  background_data <- read.csv('../data/country_year_GB_r.csv')
  
  # Pull out the country data and years required
  background_country <- background_data[background_data$CountryISO3==as.character(df_ucc$ucc_countryname) &
                                          background_data$Year >= df_ucc$ucc_start_year &
                                          background_data$Year <= df_ucc$ucc_end_year,]
  
  no_years <- df_ucc$ucc_end_year - df_ucc$ucc_start_year + 1
  
  # -----------------------------------------------------------------------------
  # Code does not need to know whether single point in time or pre/post
  # Assumes that reference case mode split in km is all zeroes if it is a 
  # single point in time analysis
  # -----------------------------------------------------------------------------
  if (df_ucc$ucc_active_mode_bike==1) {

    cf_data <- df_adj_data[df_adj_data$case=='cf',]
    ref_data <- df_adj_data[df_adj_data$case=='ref',]
  
    # Set the reference case adjusted km to zero if they are missing
    for (k in 1:length(ref_data$r_number))  if(is.na(ref_data$r_number[k])) ref_data$r_number[k] <- 0
  
    # Calculate the km difference between the reference and counterfactual
    mode_shift_km_wide <- merge(ref_data, cf_data, by="question_varname")
    mode_shift_km_wide$diff <- (mode_shift_km_wide$r_number.x - mode_shift_km_wide$r_number.y)


    # Pull out the hot emissions relevant to the traffic conditions
    # Only keep the modes which are associated with emissions (the merge drops any other modes)
    ef <- data.frame(rbind(
                      cbind(mode = "motormode_cardriver", ef = background_country$ef_car_rw_pkm, year=background_country$Year),
                      cbind(mode = "motormode_bus", ef = background_country$ef_bus_pkm, year=background_country$Year),
                      cbind(mode = "motormode_motorbike", ef = background_country$ef_moto_pkm, year=background_country$Year)))
    
    # Set ef and year to numeric (not needed in final version of country data?)
    ef$ef <- as.numeric(as.character(ef$ef))
    ef$year <- as.numeric(as.character(ef$year))
             
    emis_ppk <- merge(ef, mode_shift_km_wide, by.x = "mode", by.y = "question_varname")[,c(1,2,3,16)]

    # Calculate cold emissions for cars FOR CYCLING ASSESSMENT
    # Check first for user entered trip lengths for walking and cycling
    if(is.na(df_qual$quali_trip_length_bike)) df_qual$quali_trip_length_bike <- 5
    # if(is.na(df_qual$quali_trip_length_walk)) df_qual$quali_trip_length_walk <- 2
    
    # Create vectors for the qualitative variables which are not repeated over year
    # Need to cut vectors and ef dataframe according to user input time horizon
    trip_length_bike <- rep(df_qual$quali_trip_length_bike, no_years)
    trips_pp_year_bike <- rep(df_qual$quali_trips_pp_year_bike, no_years)
    purpose_recreation_proportion_bike <- rep(df_qual$quali_purpose_recreation_proportion_bike, no_years)
    percent_from_car_bike <- rep(df_qual$quali_percent_from_car_bike, no_years)

    # Initialise cold start emissions vector for bike
    cold_car_occ_trip_bike <- rep(NA, no_years)
    
    # Allow cold trip distance to vary by year (and also trip length)
    for (i in 1:no_years) {
      if(trip_length_bike[i] <= background_country$cold_trip_dist[i]) {
        cold_car_occ_trip_bike[i] <- trip_length_bike[i] * emis_ppk[emis_ppk$mode=="motormode_cardriver", c("ef")][i] *
          (background_country$ratioColdHot[i] - 1)
      } else {
        cold_car_occ_trip_bike[i] <- background_country$cold_trip_dist[i] / trip_length_bike[i] * trip_length_bike[i] *
          emis_ppk[emis_ppk$mode=="motormode_cardriver", c("ef")][i]  * 
          (background_country$ratioColdHot[i] - 1)
      }
    }
    
    # Change in direct emissions (in kgCO2e per person per year)

    emis_ppk$change_emis_hot <- emis_ppk$diff * emis_ppk$ef / 1000 * -1

    change_emis_cold_bike <- cold_car_occ_trip_bike * trips_pp_year_bike *
                              (1 - purpose_recreation_proportion_bike) *
                              percent_from_car_bike /1000  * -1

    emis_ppk$change_emis_total[emis_ppk$mode=="motormode_cardriver"] <- 
      emis_ppk$change_emis_hot[emis_ppk$mode=="motormode_cardriver"] + change_emis_cold_bike
    
    emis_ppk$change_emis_total[emis_ppk$mode!="motormode_cardriver"] <- 
      emis_ppk$change_emis_hot[emis_ppk$mode!="motormode_cardriver"] 
    
    emis_ppk$change_emis_cold <- emis_ppk$change_emis_total - emis_ppk$change_emis_hot
    
    change_emis <- emis_ppk[,c(1,3,5,6,7)]

    # -----------------------------------------------------------------------------
    # Change in energy supply emissions ((in kgCO2e per person per year)
    # Food intake question not implemented in this version of HEAT so only 
    # concerned with non-active modes here
    # -----------------------------------------------------------------------------
    change_supply <- data.frame('year' = rep(seq(df_ucc$ucc_start_year, df_ucc$ucc_end_year, by=1),3),
                                'mode' = c(rep("motormode_cardriver", no_years),rep("motormode_bus", no_years),
                                           rep("motormode_lightrail", no_years)),
                                'change_sup' = rep(NA, no_years*3))
      
    # car
    # IS THE WELL TO TANK FOR CAR CORRECT - NEEDS TO DEPEND ON TRAFFIC CONDITIONS??
    change_supply$change_sup[change_supply$mode=="motormode_cardriver"] <- background_country$ef_wtt_car * 
                              mode_shift_km_wide$diff[mode_shift_km_wide$question_varname=='motormode_cardriver']  / 1000 * -1
    # bus
    change_supply$change_sup[change_supply$mode=="motormode_bus"]  <- background_country$ef_wtt_bus * 
                              mode_shift_km_wide$diff[mode_shift_km_wide$question_varname=='motormode_bus']  / 1000 * -1
    # rail
    change_supply$change_sup[change_supply$mode=="motormode_lightrail"]  <- background_country$ef_wtt_rail *
                                  mode_shift_km_wide$diff[mode_shift_km_wide$question_varname=='motormode_lightrail']  / 1000 * -1

    # -----------------------------------------------------------------------------
    # Change in vehicle lifecycle emissions
    # There are no lifecycle emissions for walking so don't need to sum over this
    # Not including e-bikes for now but have left placeholder in bike calcuation
    # -----------------------------------------------------------------------------

    bike_assessment_modes <- mode_shift_km_wide$question_varname %in% 
                                                            c('motormode_bus', 'motormode_cardriver',
                                                            'motormode_carpassenger','motormode_lightrail',
                                                            'motormode_train', 'active_mode_walk',
                                                            'motormode_motorbike')
   
    change_lc <- data.frame('year' = rep(seq(df_ucc$ucc_start_year, df_ucc$ucc_end_year, by=1), 4),
                                  'mode' = c(rep("motormode_cardriver", no_years), rep("motormode_bus", no_years),
                                           rep("motormode_lightrail", no_years), rep("active_mode_bike", no_years)),
                                  'change_lc' = rep(NA, no_years*4))
    
    # car
    change_lc$change_lc[change_lc$mode=="motormode_cardriver"] <- background_country$ef_vlca_car * 
                               mode_shift_km_wide$diff[mode_shift_km_wide$question_varname=='motormode_cardriver']  / 1000 * -1
    # bus
    change_lc$change_lc[change_lc$mode=="motormode_bus"] <- background_country$ef_vlca_bus *
                               mode_shift_km_wide$diff[mode_shift_km_wide$question_varname=='motormode_bus']  / 1000 * -1
    # rail
    change_lc$change_lc[change_lc$mode=="motormode_lightrail"] <- background_country$ef_vlca_rail *
                                mode_shift_km_wide$diff[mode_shift_km_wide$question_varname=='motormode_lightrail']  / 1000 * -1
    # bike
    # NEED TO FACTOR IN ALL OF THE NEW DISTANCE ON BIKES HERE, including new and reassigned (BUT NOT RECREATIONAL?)
    # Any mode shift to walking does not have lifecycle emissions
    change_lc$change_lc[change_lc$mode=="active_mode_bike"] <- background_country$ef_vlca_bike *
                                sum(mode_shift_km_wide$diff[bike_assessment_modes])/
                                (1-(df_qual$quali_reassigned_trips_proportion_bike + df_qual$quali_new_trips_proportion_bike)) *
                                (1-df_qual$quali_ebike_proportion_bike)  / 1000

    
    # Merge the three types of carbon savings to one data frame
    all_carb <- merge(change_emis, change_supply, by=c("mode", "year"), all=TRUE)
    all_carbon <- merge(all_carb, change_lc, by=c("mode", "year"), all=TRUE)
    
    
    
    # total_per_person <- change_emis_total + change_supply_total + change_lifecycle_total 
    # total_per_population <- total_per_person * df_ucc$ucc_pop_bike

    # carb_sav_per_pop_usd <- total_per_population / 1000 * df_ucc$ucc_soc_val_carbon
    # carb_sav_per_pop_euro <- carb_sav_per_pop_usd * df_ucc$ucc_euro_usd_conv
  
    # Carbon savings per km cycled
    # carb_sav_km_bike <- carb_sav_per_pop_euro / sum(mode_shift_km_wide$diff[bike_assessment_modes])/
    #                      (1-(df_qual$quali_reassigned_trips_proportion_bike + 
    #                      df_qual$quali_new_trips_proportion_bike)) / df_ucc$ucc_pop_bike * 100

  } # End bike assessment

  # PUT ALL OUTPUTS IN A LIST WITH INDEX 'BIKE'
  list(bike=c(all_carbon))
  
}
