library(KRLS)
library(data.table)
library(pracma)


# The features of the krls model are 
# the most significant features found from a linear regression

# Read in the dataframe
dataset = "summer_dataset.csv"
dataframe <- read.csv(dataset, header=TRUE, sep=",")
# Remove NaN values of no2_ppb
dataframe <- dataframe[!is.na(dataframe$no2_ppb),]

# # Feature matrix
features <- cbind(dataframe$urban, dataframe$WS100_ind_ALLandMISC,
           dataframe$U_B500_FoREST, dataframe$U_B1500_DRYCLEAN,
           dataframe$U_WS750_npri_all, dataframe$U_WS50_nrn_collector,
           dataframe$U_DIST_AIRPORT, dataframe$U_DIST_RAIL_ALL,
           dataframe$N_U_WS500_intchg_ramp, dataframe$N_U_WS50_ind,
           dataframe$N_U_DIST_NO2, dataframe$N_U_DIST_SANDGRAVEL)

# Label vector
targets <- c(dataframe$no2_ppb)

lambdas <- linspace(0.0000000000000001, 0.000000000001, 100)
r2_values <- rep(0, length(lambdas))

# Leave-One-Out Cross Validation
# Fit the krls model on all rows but the i'th row
# Test the model on the i'th row
test_lambda <- 0.0000000000000001
num_rows = length(features)
# for(i in 1:num_rows){
#     # TRAIN FEATURES AND LABELS
#     train_features <- cbind(features[-c(i),])
#     train_targets <- targets[-c(i)]

#     # print(ncol(train_features))
#     # print(nrow(train_features))
#     # print(length(train_targets))

#     # TEST FEATURES AND LABELS
#     test_features <- t(matrix(features[c(i),]))

#     print(i)
#     krls_model <- krls(X=train_features, y=train_targets, whichkernel="gaussian", lambda=test_lambda, sigma=NULL)

#     predict(krls_model, newdata=test_features, se.fit=TRUE)
# }

for (site_num in 1:nrow(dataframe)){
    # train set
    train <- dataframe[-c(site_num),]
    # training feautures 
    train_features <- cbind(train$urban, train$WS100_ind_ALLandMISC, train$U_B500_FoREST, 
                    train$U_B1500_DRYCLEAN, train$U_WS750_npri_all, train$U_WS50_nrn_collector, 
                    train$U_DIST_AIRPORT, train$U_DIST_RAIL_ALL, train$N_U_WS500_intchg_ramp, 
                    train$N_U_WS50_ind, train$N_U_DIST_NO2, train$N_U_DIST_SANDGRAVEL)
    # training labels
    train_labels <- train$no2_ppb

    # test set
    test_full <- dataframe[c(site_num),]

    # test feautures
    test_features <- cbind(test_full$urban, test_full$WS100_ind_ALLandMISC, 
                test_full$U_B500_FoREST, test_full$U_B1500_DRYCLEAN, 
                test_full$U_WS750_npri_all, test_full$U_WS50_nrn_collector, 
                test_full$U_DIST_AIRPORT, test_full$U_DIST_RAIL_ALL, 
                test_full$N_U_WS500_intchg_ramp, test_full$N_U_WS50_ind, 
                test_full$N_U_DIST_NO2, test_full$N_U_DIST_SANDGRAVEL)

    krls_model <- krls(X=train_features, y=train_labels,, whichkernel="gaussian", lambda=test_lambda, binary = TRUE)

    print(site_num)
    
    predict(krls_model, newdata=test_features, se.fit=TRUE)
}



# for(i in 1:(length(lambdas))){
#     # Fit the KRLS model
#     krlsout <- krls(X=features, y=targets, whichkernel="gaussian", lambda=lambdas[i], sigma=NULL)
#     # Display the R2 value
#     r2_values[i] <- krlsout$R2
#     cat(i, lambdas[i], krlsout$R2)
# }

plot(lambdas, r2_values, pch=20)
# Also plot log(lambdas) vs. r2_values