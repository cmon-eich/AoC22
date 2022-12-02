#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
max <- c(0,0,0)
max_tmp = 0
for (d in data) {
	current_val = strtoi(d)
	if (!is.na(current_val)) {
		max_tmp = max_tmp + strtoi(d)
	} else {
		for (i in 1:3) {
			if (max_tmp > max[i]) {
				tmp = max[i]
				max[i] = max_tmp
				max_tmp = tmp
			}
		}
		max_tmp = 0
	}
}
print(max)
print(sum(max))
