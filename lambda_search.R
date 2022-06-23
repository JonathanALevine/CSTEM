library(KRLS)
library(data.table)
library(pracma)


# Read in the dataframe
dataset <- "datasets/summer_dataset.csv"
dataframe <- read.csv(dataset, header=TRUE, sep=",")
# Remove NaN values of no2_ppb
dataframe <- dataframe[!is.na(dataframe$no2_ppb),]

# Feature matrix
features <- cbind(dataframe$urban, dataframe$WS100_ind_ALLandMISC,
           dataframe$U_B500_FoREST, dataframe$U_B1500_DRYCLEAN,
           dataframe$U_WS750_npri_all, dataframe$U_WS50_nrn_collector,
           dataframe$U_DIST_AIRPORT, dataframe$U_DIST_RAIL_ALL,
           dataframe$N_U_WS500_intchg_ramp, dataframe$N_U_WS50_ind,
           dataframe$N_U_DIST_NO2, dataframe$N_U_DIST_SANDGRAVEL)

# Label vector
labels <- c(dataframe$no2_ppb)

krls_model <- krls(X=features, 
                   y=labels, 
                   whichkernel="gaussian", 
                   lambda=NULL, 
                   sigma=NULL, 
                   L=NULL, 
                   U=NULL, 
                   tol=NULL)

print(krls_model$R2)
print(krls_model$lambda)
print(krls_model$sigma)

