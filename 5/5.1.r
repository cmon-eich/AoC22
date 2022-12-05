#!/usr/bin/env Rscript
library(readr)
library(stringr)
options(warn=1)
data = read_lines("input.txt")
matrix_read = FALSE
matrix_processed = FALSE
m_temp <- c()
m = matrix(" ", nrow= 9, ncol=99)
i = 1
# code is ugly af ... sorry
for (d in data) {
	if (d == "") {
		matrix_read = TRUE
	}
	if (!matrix_read) {
		m_temp[i] = d
		i = i + 1
	} else if(!matrix_processed) {
		matrix_processed = TRUE
		pos = str_locate_all(m_temp[length(m_temp)], "[\\d]")[[1]][,"start"]
		i = 1
		for (p in pos) {
			j = 1
			for (s in rev(m_temp[1:length(m_temp)-1])) {
				container = substr(s, p, p)
				if (container[1] != " ") {
					# get container in m 
					m[i,j] = container
					j = j+1
				}
			}
			i = i + 1
		}
		print(m[1:9,1:19])
	} else {
		# print(paste("Order: ", d))
		x = strsplit(substr(d, 6, nchar(d)), " from ")[[1]]
		for (i in 1:strtoi(x[1])) {
			y = strsplit(x[2], " to ")[[1]]

			# print(paste("Count: ", x[1]))
			# print(paste("From: ", toString(m[strtoi(y[1]),])))
			# print(paste("To  : ", toString(m[strtoi(y[2]),])))
			change_pos = 0
			remove_pos = 0
			for (mpos in 1:99) {
				if (m[strtoi(y[2]),mpos] == " ") {
					change_pos = mpos
					if (mpos > 90) {
						print("Waring: mpos may exceed!")
					}
					break
				}
			}
			for (mpos in 1:99) {
				if (m[strtoi(y[1]),mpos] != " ") {
					remove_pos = mpos
					if (mpos > 90) {
						print("Waring: mpos may exceed!")
					}
				}
			}
			m[strtoi(y[2]), change_pos] = m[strtoi(y[1]), remove_pos]
			m[strtoi(y[1]), remove_pos] = " "
			# print(paste("From: ", toString(m[strtoi(y[1]),])))
			# print(paste("To  : ", toString(m[strtoi(y[2]),])))
		}
	}
}
print(m[1:9,1:19])
for (i in 1:9) {
	res = "0"
	for (j in 1:99) {
		if (m[i,j] != " ") {
			res = m[i,j]
		}
	}
	print(res)
}
