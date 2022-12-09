#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
# data = read_lines("test.txt")
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
dims = dim(m)
max_view = 0
for (r in 2:(dims[1]-1)) {
	for (c in 2:(dims[2]-1)) {
		val = m[r,c]
		dist_up    = 1
		dist_down  = 1
		dist_left  = 1
		dist_right = 1
		while((c-dist_up)>1 && m[r,c] > m[r,(c-dist_up)]) {
			dist_up = dist_up+1
		}
		while((c+dist_down)<=(dims[2]-1) && m[r,c] > m[r,(c+dist_down)]) {
			dist_down = dist_down+1
		}
		while((r-dist_left)>1 && m[r,c] > m[(r-dist_left),c]) {
			dist_left = dist_left+1
		}
		while((r+dist_right)<=(dims[1]-1) && m[r,c] > m[(r+dist_right),c]) {
			dist_right = dist_right+1
		}
		view = dist_up*dist_down*dist_left*dist_right
		if (max_view < view) {
			max_view = view
		}
	}
}
print(max_view)




