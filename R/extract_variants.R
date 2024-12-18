


# make one BED file for provided variants
make_dragen_bed <- function(
	in_file,
	out_bed,
	verbose=FALSE
)  {

	file_plink  <- system.file("files", "plink_linux_x86_64_20240818.zip", package="ukbrapR")
	file_dragen <- system.file("files", "dragen_pvcf_coordinates.csv.gz", package="ukbrapR")
	
	# install tabix (if not already installed)
	if ( ! suppressWarnings(system2("command", args = c("-v", "tabix"), stdout = FALSE)) == 0 )  {
		system("sudo apt-get update")
		system("sudo apt-get -y install tabix")
	}
	
	# get Plink 1.9 (if not already available)
	if (! file.exists("plink"))  {
		#system("wget https://s3.amazonaws.com/plink1-assets/plink_linux_x86_64_20240818.zip")
		#system("unzip plink_linux_x86_64_20240818.zip")
		system(paste0("cp ", file_plink, " ."))
		system(paste0("unzip ", file_plink))
	}
	
	# load user-provided varlist file (only first two TSV cols are used: must be chr, bp)
	varlist <- NULL
	if (class(in_file)=="character")  varlist <- readr::read_tsv(in_file, progress=FALSE, show_col_types=FALSE)
	varlist <- in_file   ## assume user has passed a data frame
	
	varlist <- varlist |> 
		dplyr::arrange(chr, pos) |>
		dplyr::mutate(filename="")
	
	# load DRAGEN coordinate file
	dragen <- readr::read_csv(file_dragen, progress=FALSE, show_col_types=FALSE)
	dragen <- dragen |> 
		dplyr::mutate(chromosome = stringr::str_remove_all(chromosome, "chr")) |>
		dplyr::filter(chromosome %in% unique(varlist$chr)) |>
		dplyr::arrange(chromosome, starting_position)
	  
	# for each variant get file name
	for (ii in 1:nrow(varlist))  {
		dragen_sub <- dragen |> 
			dplyr::filter(chromosome == varlist$chr[ii] & starting_position < varlist$pos[ii]) |>
			tail(n=1)
		varlist$filename[ii] <- dragen_sub$filename[1]
	}
	
	# empty object for merging
	combined <- NULL
	
	# for each VCF file
	fls <- unique(varlist$filename)
	for (ii in 1:length(fls))  {
	
		cat(stringr::str_c("Extracting file ", ii, " of ", length(fls)))
		
		# this file name
		fl <- fls[ii]
		
		# get variants list for that file
		varlist_sub <- varlist |> 
		dplyr::filter(filename==fl)
		
		# get CHR
		chr <- varlist_sub$chr[1]
		
		# path to VCF
		vcf_path <- stringr::str_c("/mnt/project/Bulk/DRAGEN\\ WGS/DRAGEN\\ population\\ level\\ WGS\\ variants\\,\\ pVCF\\ format\\ \\[500k\\ release\\]/chr", chr, "/", fl, " ")
		
		# create empty VCF file to fill
		system("echo '##fileformat=VCFv4.2' > tmp.vcf")
		system(stringr::str_c("zgrep -m 1 '#CHROM' ", vcf_path, " >> tmp.vcf"))
		
		# use tabix to extract the positions
		system(stringr::str_c(
			"tabix ",
			vcf_path,
			stringr::str_c("chr", chr, ":", varlist_sub$pos-1, "-", varlist_sub$pos, collapse=" "),
			" >> tmp.vcf"
		))
		
		# use Plink to convert
		system("./plink --vcf tmp.vcf --set-missing-var-ids @:#:\\$1:\\$2 --make-bed --out tmp")
		
		# if this is the first one, simply rename
		if (ii==1)  {
			system(paste0("mv tmp.bed ", out_bed, ".bed"))
			system(paste0("mv tmp.bim ", out_bed, ".bim"))
			system(paste0("mv tmp.fam ", out_bed, ".fam"))
		}
		
		# if not the first one, use plink to merge beds
		if (ii>1)  {
			system(paste0("./plink --bfile ", out_bed, " --bmerge tmp --make-bed --out tmp2")
			system(paste0("mv tmp2.bed ", out_bed, ".bed"))
			system(paste0("mv tmp2.bim ", out_bed, ".bim"))
			system(paste0("mv tmp2.fam ", out_bed, ".fam"))
		}

		# remove tmp files
		rm(tmp)
		system("rm tmp")

	}

}


#
#
#
#
#
# load BED file and return as data frame








#
#
#
#
#
# create polygenic score of variants in BED file





