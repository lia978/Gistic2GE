read_cis_genes<-function(file.in.peak, file.in.region){
	#file.in<-"~/Desktop/monti_lab/year2/DataIntegration/ricover/data/ricover/datasets/gistic2_analysis_allelic_capseg/job.50618380/del_genes.conf_99.sorted_in_region.txt"
	res.peak<-read.table(file.in.peak, sep = "\t", header = FALSE, fill = T)
	last.meta.ind<-which(res.peak[,1] == "genes in wide peak")-1
	
	res.meta<-t(res.peak[1:last.meta.ind,])
	colnames(res.meta)<-res.meta[1,]
	res.meta<-data.frame(res.meta[-1,])
	res.genes.peak<-res.peak[-(1:last.meta.ind), -1]
	res.genes.peak<-sapply(1:ncol(res.genes.peak),
		function(x) setdiff(unique(as.character(res.genes.peak[, x])), ""))


	res.region<-read.table(file.in.region, sep = "\t", header = FALSE, fill = T)
	last.meta.ind<-which(res.region[,1] == "genes in wide peak")-1
	res.genes.region<-res.region[-(1:last.meta.ind), -1]
	res.genes.region<-sapply(1:ncol(res.genes.region),
		function(x) setdiff(unique(as.character(res.genes.region[, x])), ""))
	res.list<-list(genesets.peak = res.genes.peak, 
		genesets.region = res.genes.region, 
		geneset.meta = res.meta)
	return(res.list)
}

#examples (not ran)
#f1<-"/Users/amyli/Desktop/git_projects/Gistic2GE/inst/extdata/lymphoma2010/gistic/lymphoma2010.dlbcl_wold.nn5.all.20110620.amp_genes.conf_75.sorted.txt"
#f2<-"/Users/amyli/Desktop/git_projects/Gistic2GE/inst/extdata/lymphoma2010/gistic/lymphoma2010.dlbcl_wold.nn5.all.20110620.amp_genes.conf_75.sorted_in_region.txt"
#x<-read_cis_genes(f1, f2)


