#!/usr/bin/env Rscript
library(readr)
library(stringr)
data = read_lines("input.txt")
containers <- vector("list", 9) #list()
process_plan = TRUE
plan <- list()
for (d in data) {
	if (d == "") {
		process_plan = FALSE
		pos = str_locate_all(plan[length(plan)], "[\\d]")[[1]][,"start"]
		for (p in rev(plan[1:length(plan)-1])) {
			for (i in 1:9) {
				c = substr(p, pos[i], pos[i])
				if (c != " ") {
					containers[[i]] <- append(containers[[i]], c)
				}
			}
		}	
	} else if (process_plan) {
		plan <- append(plan, d)
	} else {
		d = substr(d, 6, nchar(d))
		d = strsplit(d, " from ")[[1]]
		count = strtoi(d[1])
		d = strsplit(d[2], " to ")[[1]]
		from = strtoi(d[1])
		to = strtoi(d[2])
		if (count >= length(containers[[from]])) {
			containers[[to]] = append(containers[[to]], containers[[from]])
			containers[[from]] <- c() #list()
		} else {
			temp = containers[[from]][length(containers[[from]])-count+1:length(containers[[from]])]
			print(length(containers[[from]])-count)
			print(containers[[from]])
			containers[[from]] = containers[[from]][1:(length(containers[[from]])-count)]
			containers[[to]] = append(containers[[to]], temp)
		}
	}
}
print(containers)
# for (i in 1:length(containers)) {
# 	print(container[[i]][length(containers[[i]])])
# }
