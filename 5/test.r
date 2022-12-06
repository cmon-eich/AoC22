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
	}
}
print(containers)
