#
# PLA vs. SVM
#

random_line <- function() {
	# generate 2 random 2D points
	p <- matrix(runif(2 * 2, -1, 1), 2, 2)
	
	# calculate the weight vector for the line crossing
	# the 2 points
	w <- NA
	w[1] <- (p[1, 1] * p[2, 2]) - (p[2, 1] * p[1, 2])
	w[2] <- (p[1, 2] - p[2, 2])
	w[3] <- (p[2, 1] - p[1, 1])
	
	return(w)
}

classify <- function(X, w) {
	y <- sign(X %*% w)
	
	return(y)
}

perceptron <- function(X, y) {
	# initialize the weights
	w <- rep(0, 3)

	repeat {
		# evaluate the current weights
		h_y <- classify(X, w)
		# see if there are any mismatches left
		mismatches <- which(y != h_y)
		if(length(mismatches) == 0)
		{
			# if not then we are finished :)
			break
		}
		# get one random mismatch
		mismatch <- sample(mismatches, 1)
		
		w <- w + X[mismatch,] * y[mismatch]
	}
	
	return(w)
}

# initialize the random number generator
set.seed(12345)

# number of training samples
N <- 10

repeat {
	# generate some test data
	training_data <- cbind(1, matrix(runif(N * 2, -1, 1), N, 2))

	# generate a random line
	w_f <- random_line()

	# classify the training data
	y <- classify(training_data, w_f)
	
	# repeat until not all points classify to the same set
	if(abs(sum(y)) < (N - 2)) {
		break
	}
}

w_g <- perceptron(training_data, y)

# generate some test data
N_test <- 1000000
test_data <- cbind(1, matrix(runif(N_test * 2, -1, 1), N_test, 2))

y_f <- classify(test_data, w_f)
y_g <- classify(test_data, w_g)
error <- length(y_f[y_f != y_g]) / length(y_f)

print(error)