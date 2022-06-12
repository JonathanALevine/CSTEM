library(KRLS)
library(data.table)


# Read in the dataframe


# 
X <- cbind(cstem_foranalysis_gem_wr_summer$urban, cstem_foranalysis_gem_wr_summer$WS100_ind_ALLandMISC,
           cstem_foranalysis_gem_wr_summer$U_B500_FoREST, cstem_foranalysis_gem_wr_summer$U_B1500_DRYCLEAN,
           cstem_foranalysis_gem_wr_summer$U_WS750_npri_all, cstem_foranalysis_gem_wr_summer$U_WS50_nrn_collector,
           cstem_foranalysis_gem_wr_summer$U_DIST_AIRPORT, cstem_foranalysis_gem_wr_summer$U_DIST_RAIL_ALL,
           cstem_foranalysis_gem_wr_summer$N_U_WS500_intchg_ramp, cstem_foranalysis_gem_wr_summer$N_U_WS50_ind,
           cstem_foranalysis_gem_wr_summer$N_U_DIST_NO2, cstem_foranalysis_gem_wr_summer$N_U_DIST_SANDGRAVEL)

Y <- cstem_foranalysis_gem_wr_summer$no2_ppb

krlsout <- krls(X=X, y=Y, whichkernel="gaussian", lambda=NULL, sigma=NULL)

krlsout$R2


# LEAVE-ONE-OUT Cross Validation
# Switch this to K-Fold Cross Validation
KRLS_LOO_est_summer <- data.frame(id = cstem_foranalysis_gem_wr_summer$Site_ID,
                                               Measured_NO2 = cstem_foranalysis_gem_wr_summer$no2_ppb)

for (site_num in 1:nrow(KRLS_LOO_est_summer)){
  train <- cstem_foranalysis_gem_wr_summer[-c(site_num),]
  trainX <- cbind(train$urban, train$WS100_ind_ALLandMISC, train$U_B500_FoREST, 
                  train$U_B1500_DRYCLEAN, train$U_WS750_npri_all, train$U_WS50_nrn_collector, 
                  train$U_DIST_AIRPORT, train$U_DIST_RAIL_ALL, train$N_U_WS500_intchg_ramp, 
                  train$N_U_WS50_ind, train$N_U_DIST_NO2, train$N_U_DIST_SANDGRAVEL)
  trainY <- train$no2_ppb
  test_full <- cstem_foranalysis_gem_wr_summer[c(site_num),]
  test <- cbind(test_full$urban, test_full$WS100_ind_ALLandMISC, 
                test_full$U_B500_FoREST, test_full$U_B1500_DRYCLEAN, 
                test_full$U_WS750_npri_all, test_full$U_WS50_nrn_collector, 
                test_full$U_DIST_AIRPORT, test_full$U_DIST_RAIL_ALL, 
                test_full$N_U_WS500_intchg_ramp, test_full$N_U_WS50_ind, 
                test_full$N_U_DIST_NO2, test_full$N_U_DIST_SANDGRAVEL)
  LOOCV_KRLS_Urb_OLS <- krls(X=trainX, y=trainY, binary = TRUE)
  KRLS_LOO_est_summer$LOO_est[site_num] <- predict(LOOCV_KRLS_Urb_OLS, newdata=test, se.fit=TRUE)
}

##### EXPORT to .CSV
fwrite(KRLS_LOO_est_summer,"D:\\Joyce\\CSTEM\\NO2\\Level_3_Models_Comparison\\PS_KRLS_LOO_summer_binaryTRUE.csv")
