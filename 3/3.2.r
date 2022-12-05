#!/usr/bin/env Rscript
library(readr)
options(warn=1)
data = read_lines("input.txt")
sum = 0
m = rbind(1:52)
colnames(m) <- c(letters, LETTERS)
i = 0
strings <- c("","","")
for (d in data) {
	i = i+1
	if (i < 3) {
		strings[i] = d
	}
	else {
		strings[i] = d
		i = 0
		found = FALSE
		for (c1 in strsplit(strings[1], "")[[1]]) {
			for (c2 in strsplit(strings[2], "")[[1]]) {
				if (c1 == c2) {
					for (c3 in strsplit(strings[3], "")[[1]]) {
						if (c1 == c3 && found == FALSE) {
							found = TRUE
							sum = sum + m[,c3]
						}
					}
				}
			}
		}
	}
}
print(sum)
