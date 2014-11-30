#
# Validation
#
# The script expects the training (in.dta) and test (out.dta) data sets to be 
# available in the parent folder 
#

# Function to calculate the error of the linear regression model represented by 
# the weight vector w.
# The error is the percentage misclassified points
calculate_error <- function(X, y, w) {
	# calculate the output
	g <- X %*% w

	# calculate the number of misclassified points
	n_mismatches <- sum((-1 + 2 * (g >= 0)) != y)

	# calculate the percentage of mismatches
	n_samples = dim(X)[1]
	error <- n_mismatches / n_samples
	
	return(error)
}

# Function to read the data from a file. The function will return
# a list of two matrices, one with the features (X) and one with the 
# classified values (y)
read_data <- function(filename) {
	# read the data
	data <- read.table(filename)
	names(data) <- c("x1", "x2", "y")

	# create the feature matrix
	X <- cbind(1, data$x1, data$x2, data$x1^2, data$x2^2, data$x1*data$x2, abs(data$x1 - data$x2), abs(data$x1 + data$x2))

	# create the output values matrix
	y <- cbind(data$y)
	
	return(list(X = X, y = y))
}

# Function to train a linear regression model given the value of k.
# The function will return the in sample, validation and out of sample errors.
train_and_evaluate <- function(k) {
	# read the training data
	data <- read_data("../in.dta")
	
	# split the data into validation data (first 25 samples) and train data (last 10 samples)
	# we only take the first k features (plus the bias)
	train_data <- list(X = data$X[26:35, 1:(k + 1)], y = data$y[26:35,])
	validate_data <- list(X = data$X[1:25, 1:(k + 1)], y = data$y[1:25,])
	
	# Train the linear regression model
	w <- solve(t(train_data$X) %*% train_data$X) %*% (t(train_data$X) %*% train_data$y)

	# calculate the in sample error
	E_in <- calculate_error(train_data$X, train_data$y, w)

	# calculate validation error
	E_val <- calculate_error(validate_data$X, validate_data$y, w)
	
	# read the test data
	test_data <- read_data("../out.dta")

	# calculate the out of sample error
	E_out <- calculate_error(test_data$X[, 1:(k + 1)], test_data$y, w)
	
	print(E_out)
	
	return(cbind(E_in, E_val, E_out))
}

# the number of features to use
k <- 3:7

# calculate the errors
errors = cbind(k = k, do.call(rbind, lapply(k, train_and_evaluate)))

print(errors)