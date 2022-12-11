#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
# data = read_lines("test.txt")
c = 1
m <- matrix(c("."), ncol=40, nrow=6)
crt = 1
sprite <- c(1,2,3)
for (d in data) {
	command = substring(d, 1, 4)
	if (command == "noop") {
		crt = c%%40
		if (crt %in% sprite) {
			m[(c%/%40+1),crt] = "#"
		}
		c = c+1
	} else if (command == "addx") {
		crt = c%%40
		if (crt %in% sprite) {
			m[(c%/%40+1),crt] = "#"
		}
		c = c+1
		crt = c%%40
		if (crt %in% sprite) {
			m[(c%/%40+1),crt] = "#"
		}
		c = c+1
		sprite = sprite+strtoi(strsplit(d, " ")[[1]][2])
	} else {
		print("fuck you!")
		stop()
	}
}
for (i in 1:dim(m)[1]) {
	print(paste(c(m[i,]), collapse=""))
}
