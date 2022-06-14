library(KRLS)
library(data.table)
library(pracma)


# The features of the krls model are 
# the most significant features found from a linear regression

# Read in the dataframe
dataset = "summer_dataset.csv"
dataframe <- read.csv(dataset, header=TRUE, sep=",")
dataframe <- dataframe[!is.na(dataframe$no2_ppb),]

# # Feature matrix
X <- cbind(dataframe$urban, dataframe$WS100_ind_ALLandMISC,
           dataframe$U_B500_FoREST, dataframe$U_B1500_DRYCLEAN,
           dataframe$U_WS750_npri_all, dataframe$U_WS50_nrn_collector,
           dataframe$U_DIST_AIRPORT, dataframe$U_DIST_RAIL_ALL,
           dataframe$N_U_WS500_intchg_ramp, dataframe$N_U_WS50_ind,
           dataframe$N_U_DIST_NO2, dataframe$N_U_DIST_SANDGRAVEL)

# Label vector
y <- c(dataframe$no2_ppb)

lambdas = linspace(0, 1, 1000)

print(lambdas)

# fit the KRLS model
krlsout <- krls(X=X, y=y, whichkernel="gaussian", lambda=0.00001, sigma=0.00001)

krlsout$R2
