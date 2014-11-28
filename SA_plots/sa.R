library(ggplot2)

files <- list.files('./sa')
table <- list()
i=1
for (file in files) {

    file_path = paste('./sa/', file, sep="")
    table[[i]] <-read.table(file_path, skip=4, header=T, stringsAsFactors=FALSE)

    for (j in 1:length(table[[i]]$SA)) {
        sa = table[[i]]$SA[j]
        table[[i]]$SA[j] = substring(sa, 1, nchar(sa) - 25)
    }

    # convert SA to factors
    table[[i]]$SA <-as.factor(table[[i]]$SA)
    
    
    
    #order by percent
    table[[i]]$SA <- factor(table[[i]]$SA, levels = table[[i]]$SA[order(table[[i]]$Percent)])
    
    name_tmp <- readLines(file_path, n=1)[1]
    #name=gsub("_", " ", substring(name_tmp, 20, nchar(name_tmp) - 9))
    name = substring(name_tmp, 21, nchar(name_tmp) - 9)
    title_name = gsub("_", " ", name)
    
    pdf(paste('./graphs/', name, sep=""))
    print(qplot(x = SA, y = Percent,data = table[[i]], asp=1) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab("Sample ID")+ ggtitle(title_name)+theme(plot.title = element_text(size=12)))
    
    dev.off()
    
    print(name)
    
    i=i+1

}



