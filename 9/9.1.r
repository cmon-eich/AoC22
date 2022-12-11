#!/usr/bin/env Rscript
library(readr)
data = read_lines("input.txt")
# data = read_lines("test.txt")
head <- c(0,0)
tail <- c(0,0)
tail_pos <- list()
for (d in data) {
	x = strsplit(d, " ")[[1]]
	direction = x[1]
	times = x[2]
	for (i in 1:strtoi(times)) {
		if (direction == "R") {
			head[1] = head[1]+1
			if (abs(head[1]-tail[1])>1) {
				tail <- c(head[1]-1, head[2])
			}
		} else if (direction == "L") {
			head[1] = head[1]-1
			if (abs(head[1]-tail[1])>1) {
				tail <- c(head[1]+1, head[2])
			}
		} else if (direction == "U") {
			head[2] = head[2]+1
			if (abs(head[2]-tail[2])>1) {
				tail <- c(head[1], head[2]-1)
			}
		} else if (direction == "D") {
			head[2] = head[2]-1
			if (abs(head[2]-tail[2])>1) {
				tail <- c(head[1], head[2]+1)
			}
		}
		tail_pos[[length(tail_pos)+1]] = tail
	}
}
tail_pos_unique <- list()
for (t in tail_pos) {
	unique = TRUE
	for (tu in tail_pos_unique) {
		if (identical(t, tu)) {
			unique = FALSE
		}
	}
	if (unique) {
		tail_pos_unique[[length(tail_pos_unique)+1]] = t
	}
}
print(length(tail_pos_unique))
