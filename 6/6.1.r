#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
# data = c("abasdfasdf")
let = strsplit(data[1], "")[[1]]
code <- let[1:4]
pos = 1
for (l in let) {
	code = append(code[2:length(code)], l)
	tmp <- table(code)
	code_is_right = TRUE
	print(code)
	for (c in code) {
		if (tmp[c]>1) {
			code_is_right = FALSE
		}
	}
	if (code_is_right) {
		print(pos)
		stop()
	}
	pos = pos + 1
}

