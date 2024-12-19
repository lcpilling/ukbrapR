


# make one BED file for provided variants
make_dragen_bed <- function(
	in_file,
	out_bed,
	verbose=FALSE
)  {
	
	# required files
	file_plink  <- system.file("files", "plink_linux_x86_64_20240818.zip", package="ukbrapR")
	file_dragen <- system.file("files", "dragen_pvcf_coordinates.csv.gz", package="ukbrapR")

	# start
	start_time <- Sys.time()

	#
	#
	# check inputs
	if (verbose) cli::cli_alert("Checking inputs")
	
	# load user-provided varlist file (only first two TSV cols are used: must be chr, bp)
	varlist <- NULL
	
	# if it's a character string, assume user has provided a file path
	if (class(in_file)=="character")  {
		if (length(in_file)>1)  cli::cli_abort("Input file path needs to be length 1")
		# does input file exist?
		if (! file.exists(in_file))  cli::cli_abort("Input file not found")
		varlist <- readr::read_tsv(in_file, progress=FALSE, show_col_types=FALSE)
	}
	else if (! any(class(in_file) %in% c("data.frame","tbl","tbl_df")))  {
		cli::cli_abort(c(
			"{.var in_file} must be a data.frame (or tibble), or a character string",
			"x" = "You've supplied a {.cls {class(in_file)}} vector."
		))
	} else {
		varlist <- in_file   # user has passed a data frame
	}
	
	# data frame needs to include `CHR` and `POS`
	if (any( ! c("CHR","POS") %in% colnames(varlist) ) )  cli::cli_abort("Input file needs to contain cols CHR and POS")
	varlist <- varlist |> 
		dplyr::arrange(chr, pos) |>
		dplyr::distinct() |>
		dplyr::mutate(filename="")
	
	# check output format 
	if (! class(out_bed)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
	if (length(out_bed)>1)  cli::cli_abort("Output file prefix needs to be length 1")
	
	#
	#
	# install tabix (if not already installed)
	if (verbose) cli::cli_alert("Checking tabix installed")
	
	if ( ! suppressWarnings(system2("command", args = c("-v", "tabix"), stdout = FALSE)) == 0 )  {
		if (verbose) cli::cli_alert("Installing tabix")
		system("sudo apt-get update")
		system("sudo apt-get -y install tabix")
	}
	
	# get Plink 1.9 (if not already available)
	if (verbose) cli::cli_alert("Checking plink available")
	if (! file.exists("plink"))  {
		if (verbose) cli::cli_alert("Unpacking plink")
		#system("wget https://s3.amazonaws.com/plink1-assets/plink_linux_x86_64_20240818.zip")
		#system("unzip plink_linux_x86_64_20240818.zip")
		system(paste0("cp ", file_plink, " ."))
		system(paste0("unzip ", file_plink))
	}
	
	# load DRAGEN coordinate file
	if (verbose) cli::cli_alert("Load DRAGEN coordinates file")
	dragen <- readr::read_csv(file_dragen, progress=FALSE, show_col_types=FALSE)
	dragen <- dragen |> 
		dplyr::mutate(chromosome = stringr::str_remove_all(chromosome, "chr")) |>
		dplyr::filter(chromosome %in% unique(varlist$chr)) |>
		dplyr::arrange(chromosome, starting_position)
	  
	# for each variant get file name
	if (verbose) cli::cli_alert("For each variant identify the corresponding DRAGEN pVCF file")
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
	
		if (verbose) cli::cli_alert(stringr::str_c("Extracting file ", ii, " of ", length(fls)))
		
		# this file name
		fl <- fls[ii]
		
		# get variants list for that file
		varlist_sub <- varlist |> 
		dplyr::filter(filename==fl)
		
		# get CHR
		chr <- varlist_sub$chr[1]
		
		# path to VCF
		vcf_path <- stringr::str_c("/mnt/project/Bulk/DRAGEN\\ WGS/DRAGEN\\ population\\ level\\ WGS\\ variants\\,\\ pVCF\\ format\\ \\[500k\\ release\\]/chr", chr, "/", fl, " ")
		if (verbose) cli::cli_alert(stringr::str_c("Path to pVCF: ", vcf_path))
		
		# create empty VCF file to fill
		system("echo '##fileformat=VCFv4.2' > tmp.vcf")
		system(stringr::str_c("zgrep -m 1 '#CHROM' ", vcf_path, " >> tmp.vcf"))
		
		# use tabix to extract the positions
		if (verbose) cli::cli_alert("Use tabix to extract the positions")
		system(stringr::str_c(
			"tabix ",
			vcf_path,
			stringr::str_c("chr", chr, ":", varlist_sub$pos-1, "-", varlist_sub$pos, collapse=" "),
			" >> tmp.vcf"
		))
		
		# use Plink to convert
		if (verbose) cli::cli_alert("Use plink to convert pVCF to BED")
		system("./plink --vcf tmp.vcf --set-missing-var-ids @:#:\\$1:\\$2 --make-bed --out tmp")
		
		# if this is the first one, simply rename
		if (ii==1)  {
			system(paste0("mv tmp.bed ", out_bed, ".bed"))
			system(paste0("mv tmp.bim ", out_bed, ".bim"))
			system(paste0("mv tmp.fam ", out_bed, ".fam"))
		}
		
		# if not the first one, use plink to merge beds
		if (ii>1)  {
			if (verbose) cli::cli_alert("Merge BEDs")
			system(paste0("./plink --bfile ", out_bed, " --bmerge tmp --make-bed --out tmp2"))
			system(paste0("mv tmp2.bed ", out_bed, ".bed"))
			system(paste0("mv tmp2.bim ", out_bed, ".bim"))
			system(paste0("mv tmp2.fam ", out_bed, ".fam"))
		}

		# remove tmp files
		rm(tmp)
		system("rm tmp")

	}
	
	if (verbose) cli::cli_alert_success(c("DRAGEN BED made! Time taken: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))

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





