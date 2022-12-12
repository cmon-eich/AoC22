#!/usr/bin/env Rscript
library(readr)
options(warn=1)

v_in_list <- function(vec, lst) {
	return(Position(function(x) identical(x, vec), lst, nomatch=0) > 0)
}
determine_options <- function(cur_pos, m, already_visited) {
	cur_letter = m[cur_pos[1], cur_pos[2]]
	if (cur_letter == "S") {
		cur_letter = "a"
	}
	if (cur_letter == "E") {
		cur_letter = "z"
	}
	options <- list()
	if (cur_pos[1] > 1) {
		up_letter = m[(cur_pos[1]-1), cur_pos[2]]
		if (up_letter == "S") {
			up_letter = "a"
		}
		if (up_letter == "E") {
			up_letter = "z"
		}
		if ((match(cur_letter,letters)+1) >= match(up_letter,letters)) {
			new_pos = c(cur_pos[1]-1, cur_pos[2])
			if (!v_in_list(new_pos, already_visited)) {
				options[[length(options)+1]] = new_pos 
			}
		}
	}
	if (cur_pos[1] < dim(m)[1]) {
		down_letter = m[(cur_pos[1]+1), cur_pos[2]]
		if (down_letter == "S") {
			down_letter = "a"
		}
		if (down_letter == "E") {
			down_letter = "z"
		}
		if ((match(cur_letter,letters)+1) >= match(down_letter,letters)) {
			new_pos = c(cur_pos[1]+1, cur_pos[2])
			if (!v_in_list(new_pos, already_visited)) {
				options[[length(options)+1]] = new_pos 
			}
		}
	}
	if (cur_pos[2] > 1) {
		left_letter = m[cur_pos[1], cur_pos[2]-1]
		if (left_letter == "S") {
			left_letter = "a"
		}
		if (left_letter == "E") {
			left_letter = "z"
		}
		if ((match(cur_letter,letters)+1) >= match(left_letter,letters)) {
			new_pos = c(cur_pos[1], cur_pos[2]-1)
			if (!v_in_list(new_pos, already_visited)) {
				options[[length(options)+1]] = new_pos 
			}
		}
	}
	if (cur_pos[2] < dim(m)[2]) {
		right_letter = m[cur_pos[1], cur_pos[2]+1]
		if (right_letter == "S") {
			right_letter = "a"
		}
		if (right_letter == "E") {
			right_letter = "z"
		}
		if ((match(cur_letter,letters)+1) >= match(right_letter,letters)) {
			new_pos = c(cur_pos[1], cur_pos[2]+1)
			if (!v_in_list(new_pos, already_visited)) {
				options[[length(options)+1]] = new_pos 
			}
		}
	}
	return(options)
}

show <- function(options) {
	view <-matrix(c("."), ncol=dim(m)[2], nrow=dim(m)[1])
	for (o in options) {
		view[o[1], o[2]] = "#"
	}
	for (i in 1:dim(view)[1]) {
		print(paste(view[i,], collapse=""))
	}
}

data = read_lines("input.txt")
m <- matrix(c("."), ncol=nchar(data[1]))
for (d in data) {
	m = rbind(m, strsplit(d, "")[[1]])
}
# remove initial row
m = m[2:dim(m)[1], 1:dim(m)[2]]
# Determine Start and End position
start <- c(0,0)
end <- c(0,0)
for (i in 1:dim(m)[1]) {
	s_pos = match("S", m[i,])
	e_pos = match("E", m[i,])
	if (!is.na(s_pos)) {
		already_visited <- list()
		start <- c(i, s_pos)
	}
	if (!is.na(e_pos)) {
		# for whatever reason the type of end otherwise would be integer which can NOT be identical with an double vector
		end <- as.numeric(c(i, e_pos))
	}
}
options <- list()
list_of_as <- list()
for (i in 1:dim(m)[1]) {
	for (j in 1:dim(m)[2]) {
		if (m[i,j] == "a") {
			list_of_as[[length(list_of_as)+1]] = as.numeric(c(i,j))
		}
	}
}
already_visited <- list()
already_visited <- list_of_as 
options[[1]] <- list_of_as

while (!(v_in_list(end, options[[length(options)]]))) {
	print("----------------------------------------------")
	print(paste("Round: ", as.character(length(options)), 
				"; Iterations: ", as.character(length(options[[length(options)]]))))
	print("----------------------------------------------")
	show(options[[length(options)]])
	tmp <- list()
	for (o in options[[length(options)]]) {
		tmp = append(tmp, determine_options(o, m, already_visited))
		already_visited[[length(already_visited)+1]] = o
	}
	tmp_unique <- list()
	for (t in tmp) {
		if (!v_in_list(t, tmp_unique)) {
			tmp_unique[[length(tmp_unique)+1]] = t
		}
	}
	options[[length(options)+1]] = tmp_unique
}
print(length(already_visited))

print(length(options)-1)

