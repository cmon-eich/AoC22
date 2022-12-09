#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
rcount = 0
ccount = 0
mv <- c()
for (d in data) {
	rcount = rcount+1
	digits = as.numeric(strsplit(d, "")[[1]])
	mv <- append(mv, digits)
	if (ccount == 0) {
		ccount = length(digits)
	}
}
m <- matrix(mv, nrow=rcount, ncol=ccount)
print(m)
print(dim(m))
dims = dim(m)
vm <- matrix(TRUE, nrow=dims[1], ncol=dims[2])
# visible_trees = dims[1]*2+(dims[2]-2)*2
for (r in 2:(dims[1]-1)) {
	for (c in 2:(dims[2]-1)) {
		val = m[r,c]
		vl = TRUE
		vr = TRUE
		vu = TRUE
		vd = TRUE
		for (left in 1:(r-1)) {
			if (vl && val <= m[left,c]) {
				vl = FALSE
			}
		}
		for (right in (r+1):dims[1]) {
			if (vr && val <= m[right,c]) {
				vr = FALSE
			}
		}
		for (up in 1:(c-1)) {
			if (vu && val <= m[r,up]) {
				vu = FALSE
			}
		}
		for (down in (c+1):dims[2]) {
			if (vd && val <= m[r,down]) {
				vd = FALSE
			}
		}
		vm[r,c] = vl || vr || vu || vd
	}
}
sum = 0
for (i in vm) {
	if (i) {
		sum = sum + 1
	}
}
print(sum)




