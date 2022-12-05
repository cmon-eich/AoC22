#!/usr/bin/env Rscript
library(readr)
options(warn=1)
data = read_lines("input.txt")
sum = 0
for (d in data) {
	len = nchar(d)
	s1 = substr(d, 0, len/2)
	s2 = substr(d, len/2+1, len)
	m = rbind(1:52)
	colnames(m) <- c(letters, LETTERS)
	found = FALSE
	print("looping:")
	for (i1 in strsplit(s1, "")[[1]]) {
		for (i2 in strsplit(s2, "")[[1]]) {
			if (i1 == i2) {
				print(paste("match: ", i1))
				if (found) {
					# nasty trap
					return
				} else {
					found = TRUE
					sum = sum + m[,i1]
				}
			}
		}
	}
}
print(sum)
