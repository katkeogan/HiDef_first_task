# breaking transect lines into equal parts ~500 metres

test <- rnorm(50, 3275, 500) # simulate some transects with an average length of x metres and sd of x metres

test/500 # how many times does 500 divide into each transect?

if the decimal is lower than .5, then divide by lower number
if the decimal is higher than .5, then divide by higher number

could also test which is closer to 500 if wanting to check. 

test[1]/500

(500*.436654)/6

test[1]/6
test[1]/7

test[4]/9
test[4]/8

test[9]/500
test[9]/6
test[9]/5
