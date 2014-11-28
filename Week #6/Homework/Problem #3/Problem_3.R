#
# Linear Regression with regularization
#
# The script expects the training (in.dta) and test (out.dta) data sets to be 
# available in the parent folder 
#

# Problem parameter (see the problem description).
# lambda will be 10^k
k <- -3

# function to calculate the error of the linear regression model represented by 
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

# function to read the data from a file. The function will return
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

# read the training data
train_data <- read_data("../in.dta")

# Train the linear regression model with regularization
lambda <- 10^k
I <- diag(dim(train_data$X)[2])
I[1, 1] = 0
w <- solve(t(train_data$X) %*% train_data$X + lambda * I) %*% (t(train_data$X) %*% train_data$y)

# calculate the in sample error
E_in <- calculate_error(train_data$X, train_data$y, w)

# read the test data
test_data <- read_data("../out.dta")

# calculate the out of sample error
E_out <- calculate_error(test_data$X, test_data$y, w)

# print the results
cat("E-in:  ", E_in, "\n")
cat("E-out: ", E_out, "\n")
