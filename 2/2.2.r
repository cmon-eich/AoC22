#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
X <- c(0,0,0)
Y <- c(3,3,3)
Z <- c(6,6,6)
m = cbind(X,Y,Z)
rownames(m) <- c("A", "B", "C")
sum = 0
for (d in data) {
	v = strsplit(d, " ")[[1]]
	xyz_val = 1
	if (v[2] == "X") {
		# Rock
		xyz_val = 3 
		if (v[1] == "B") {
			# Paper
			xyz_val = 1  
		} else if (v[1] == "C") {
			# Scissor
			xyz_val = 2
		}
	} else if (v[2] == "Y") {
		# Rock
		xyz_val = 1
		if (v[1] == "B") {
			# Paper
			xyz_val = 2 
		} else if (v[1] == "C") {
			# Scissor
			xyz_val = 3
		}
	} else if (v[2] == "Z") {
		# Rock
		xyz_val = 2
		if (v[1] == "B") {
			# Paper
			xyz_val = 3  
		} else if (v[1] == "C") {
			# Scissor
			xyz_val = 1
		}
	}
	sum = sum + xyz_val
	sum = sum + m[v[1],v[2]]
}
print(sum)

