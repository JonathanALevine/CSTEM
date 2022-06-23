library(spgwr)
library(GWmodel)

#######################################################################################
########################### Model Building ############################################

col.bw <- gwr.sel(no2_ppb ~ B50_ind_ALLandMISC + urban + U_B500_FOREST + U_B1500_DRYCLEAN +
                  U_B500_npri_all + U_B50_nrn_collector + U_DIST_AIRPORT + U_DIST_RAIL_ALL +
                  N_U_B500_intchg_ramp + N_U_B50_IND + N_U_DIST_NO2 + N_U_DIST_SANDGRAVEL,
                  data = cstem_foranalysis_gwr_summer,
                  coords = cbind(cstem_foranalysis_gwr_summer$Longitude, cstem_foranalysis_gwr_summer$Latitude),
                  gweight = gwr.bisquare)

col.bisq <- gwr(no2_ppb ~ B50_ind_ALLandMISC + urban + U_B500_FOREST + U_B1500_DRYCLEAN +
                  U_B500_npri_all + U_B50_nrn_collector + U_DIST_AIRPORT + U_DIST_RAIL_ALL +
                  N_U_B500_intchg_ramp + N_U_B50_IND + N_U_DIST_NO2 + N_U_DIST_SANDGRAVEL,
                data = cstem_foranalysis_gwr_summer,
                coords = cbind(cstem_foranalysis_gwr_summer$Longitude, cstem_foranalysis_gwr_summer$Latitude),
                bandwidth = col.bw, gweight = gwr.bisquare, hatmatrix = TRUE)

#####################################################################################
####################### LOOCV #######################################################



GWR_LOO_est_summer <- data.frame(id = cstem_foranalysis_gwr_summer$Site_ID,
                                 Measured_NO2 = cstem_foranalysis_gwr_summer$no2_ppb,
                                 lat = cstem_foranalysis_gwr_summer$Latitude,
                                 long = cstem_foranalysis_gwr_summer$Longitude)

for (site_num in 1:nrow(GWR_LOO_est_summer)){
  train <- cstem_foranalysis_gwr_summer[-c(site_num),]
  test <- cstem_foranalysis_gwr_summer[c(site_num),]
  LOOCV_bw <- gwr.sel(no2_ppb ~ B50_ind_ALLandMISC + urban + U_B500_FOREST + U_B1500_DRYCLEAN +
                     U_B500_npri_all + U_B50_nrn_collector + U_DIST_AIRPORT + U_DIST_RAIL_ALL +
                     N_U_B500_intchg_ramp + N_U_B50_IND + N_U_DIST_NO2 + N_U_DIST_SANDGRAVEL,
                   data = train, coords = cbind(train$Longitude, train$Latitude), gweight = gwr.bisquare)
  
  GWR_LOO_est_summer$LOO_est[site_num] <- gwr.predict(no2_ppb ~ B50_ind_ALLandMISC + urban + U_B500_FOREST + U_B1500_DRYCLEAN +
                                                        U_B500_npri_all + U_B50_nrn_collector + U_DIST_AIRPORT + U_DIST_RAIL_ALL +
                                                        N_U_B500_intchg_ramp + N_U_B50_IND + N_U_DIST_NO2 + N_U_DIST_SANDGRAVEL,
                                                      data = train, predictdata = test, bw = LOOCV_bw, kernel = "bisquare")
}




