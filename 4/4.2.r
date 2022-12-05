#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
sum = 0
for (d in data) {
	sectors = strsplit(d, ",")[[1]]
	s1 = strsplit(sectors[1], "-")[[1]]
	s2 = strsplit(sectors[2], "-")[[1]]
	s11 = strtoi(s1[1])
	s12 = strtoi(s1[2])
	s21 = strtoi(s2[1])
	s22 = strtoi(s2[2])
	if (s11 <= s21 && s12 >= s21 || s11 <= s22 && s12 >= s22 ||
		s11 >= s21 && s11 <= s21 || s12 >= s21 && s12 <= s22) {
		sum = sum + 1
	}
}
print(sum)


