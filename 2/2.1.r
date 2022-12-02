#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
X <- c(3,0,6)
Y <- c(6,3,0)
Z <- c(0,6,3)
XYZ <- c(1,2,3)
m = cbind(X,Y,Z)
rownames(m) <- c("A", "B", "C")
sum = 0
for (d in data) {
	v = strsplit(d, " ")[[1]]
	xyz_val = 1
	if (v[2] == "Y") {
		xyz_val = 2
	} else if (v[2] == "Z") {
		xyz_val = 3
	}
	sum = sum + xyz_val
	sum = sum + m[v[1],v[2]]
}
print(sum)

