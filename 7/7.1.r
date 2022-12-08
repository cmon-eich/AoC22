#!/usr/bin/env Rscript
library(readr)


directory <- setClass("directory", slots = c(dirname="character", dirs="list", fileSize="numeric"))

dir_size <- function(dirs, dirname) {
	sum = 0
	for (d in dirs) {
		if (attr(d, "dirname") == dirname) {
			sum = sum + attr(d, "fileSize")
			for (x in attr(d, "dirs")) { 
				ds = dir_size(dirs, paste(dirname, x, sep="/"))
				sum = sum + ds 
			}
		}
	}
	return(sum)
}


data = read_lines("input.txt")
# data = read_lines("test.txt")

current_dir = ""
dirs <- list()
tempdir <- directory(dirname="tempdir", fileSize=0, dirs=list())
for (d in data) {
	# print(d)
	if(substring(d, 1,1) == "$") {
		if (substring(d, 3,4) == "cd") {
			# print(substring(d, 6, nchar(d)))
			if (attr(tempdir, "dirname") != "tempdir") {
				# print(tempdir)
				dirs <- append(dirs, tempdir)
			}
			if (substring(d, 6,7) == "..") {
				tmp = strsplit(current_dir, "/")[[1]]
				if (length(tmp) < 2) {
					current_dir = "/"
				} else {
					tmp = tmp[1:(length(tmp)-1)]
					current_dir = paste(tmp, collapse="/")
				}
			} else if (substring(d, 6,6) == "/") {
				current_dir = "/"
			} else {
				current_dir = paste(current_dir, substring(d, 6, nchar(d)), sep="/")
			}
			tempdir <- directory(dirname=current_dir, fileSize=0, dirs=list())
		} else if( substring(d, 3,4) == "ls") {
			# do nothing I guess ...
		} else {
			print("fuck you!")
			print(d)
			stop()
		}
	} else {
		if (substring(d, 1,3) == "dir") {
			attr(tempdir, "dirs") <- append(attr(tempdir, "dirs"), (substring(d, 5, nchar(d))))
		} else {
			attr(tempdir, "fileSize") <- attr(tempdir, "fileSize") + (strtoi(strsplit(d, " ")[[1]][1]))
		}
	}
}
dirs <- append(dirs, tempdir)
dirnames <- list()
for (d in dirs) {
	d = attr(d, "dirname")
	if (!(d %in% dirnames)) {
		dirnames = append(dirnames, d)
	}
}

sum = 0
for (d in dirnames) {
	size = dir_size(dirs, d) #attr(d, "dirname"))
	if (size <= 100000) {
		sum = sum + size
	}
}
print(sum)


