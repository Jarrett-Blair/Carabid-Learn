my.files <- list.files()
compiled.data <- NA
current.data <- NA

for(i in 1:length(my.files))
	{

	current.data <- read.csv(my.files[i])
	if(i>1){
		names(compiled.data) = names(current.data)
	}
	compiled.data <- rbind(compiled.data,current.data)

}