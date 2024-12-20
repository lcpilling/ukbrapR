#' Extract variants from DRAGEN pVCF file(s) into single BED file 
#'
#' @description For a given set of genomic coordinates extract the UK Biobank WGS DRAGEN variant calls (spread across many pVCFs) into a single BED file.
#'
#' @return A single merged BED file (and BIM and FAM files)
#'
#' @author Luke Pilling
#'
#' @name make_dragen_bed
#'
#' @param in_file A data frame or file path. Contains at least two columns: `CHR` and `POS` (in build 38). Other columns are ignored.
#' @param out_bed A string. 
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#' @param very_verbose Logical. Be very verbose (show individual steps & show terminal output from Plink etc),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' make_dragen_bed(in_file=readr::read_tsv(system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), n_max=3, progress=FALSE, show_col_types=FALSE), out_bed="test_pgs_liver_cirrhosis")
#'
#' @export
#'
make_dragen_bed <- function(
    in_file,     # in_file <- readr::read_tsv(system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), n_max=2)  # example
    out_bed,
    verbose=FALSE,
    very_verbose=FALSE
)  {
  
  # required files
  file_plink  <- system.file("files", "plink_linux_x86_64_20240818.zip", package="ukbrapR")
  file_dragen <- system.file("files", "dragen_pvcf_coordinates.csv.gz", package="ukbrapR")
  
  # start
  start_time <- Sys.time()
  
  #
  #
  # check inputs
  if (very_verbose)  verbose <- TRUE
  if (verbose) cli::cli_alert("Checking inputs")
  
  # load user-provided varlist file (only first two TSV cols are used: must be chr, bp)
  varlist <- NULL
  
  # if it's a character string, assume user has provided a file path
  if (class(in_file)[1] == "character")  {
    
    if (length(in_file)>1)  cli::cli_abort("Input file path needs to be length 1")
    # does input file exist?
    if (! file.exists(in_file))  cli::cli_abort("Input file not found")
    varlist <- readr::read_tsv(in_file, progress=FALSE, show_col_types=FALSE)
    
  } else if (! any(class(in_file) %in% c("data.frame","tbl","tbl_df")))  {
    
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
    dplyr::arrange(CHR, POS) |>
    dplyr::distinct() |>
    dplyr::mutate(filename="")
  
  # check output format 
  if (! class(out_bed)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
  if (length(out_bed)>1)  cli::cli_abort("Output file prefix needs to be length 1")
  
  
  #
  #
  # load DRAGEN coordinate file
  if (verbose) cli::cli_alert("Load DRAGEN coordinates file")
  dragen <- readr::read_csv(file_dragen, progress=FALSE, show_col_types=FALSE)
  dragen <- dragen |> 
    dplyr::mutate(chromosome = stringr::str_remove_all(chromosome, "chr")) |>
    dplyr::filter(chromosome %in% unique(varlist$CHR)) |>
    dplyr::arrange(chromosome, starting_position)
  
  # for each variant get file name
  if (verbose) cli::cli_alert("For each variant identify the corresponding DRAGEN pVCF file")
  for (ii in 1:nrow(varlist))  {
    dragen_sub <- dragen |> 
      dplyr::filter(chromosome == varlist$CHR[ii] & starting_position < varlist$POS[ii]) |>
      tail(n=1)
    varlist$filename[ii] <- dragen_sub$filename[1]
  }
  
  # How many variants/files we doing?
  n_files <- length(unique(varlist$filename))
  
  #
  #
  # install tabix (if not already installed)
  if (verbose) cli::cli_alert("Checking tabix installed")
  
  if ( ! suppressWarnings(system2("command", args = c("-v", "tabix"), stdout = FALSE)) == 0 )  {
    if (verbose) cli::cli_alert("Installing tabix")
    c1 <- "sudo apt-get update"
    c2 <- "sudo apt-get -y install tabix"
    if (very_verbose)  {
      system(c1)
      system(c2)
    } else {
      system(stringr::str_c(c1, " >/dev/null"))
      system(stringr::str_c(c2, " >/dev/null"))
    }
  }
  
  # get Plink 1.9 (if not already available)
  if (verbose) cli::cli_alert("Checking plink available")
  if (! file.exists("plink"))  {
    if (verbose) cli::cli_alert("Unpacking plink")
    
    #system("wget https://s3.amazonaws.com/plink1-assets/plink_linux_x86_64_20240818.zip")
    #system("unzip plink_linux_x86_64_20240818.zip")
    
    c1 <- paste0("cp ", file_plink, " .")
    c2 <- paste0("unzip ", file_plink)
    if (very_verbose)  {
      system(c1)
      system(c2)
    } else {
      system(stringr::str_c(c1, " >/dev/null"))
      system(stringr::str_c(c2, " >/dev/null"))
    }
  }
  
  #
  #
  # for each VCF file
  fls <- unique(varlist$filename)
  
  # show progress
  cli::cli_alert("Extracting {nrow(varlist)} variant{?s} from {n_files} DRAGEN pVCF file{?s} (ETA {prettyunits::pretty_sec(n_files*90)})")
  if (length(fls)>1)  {
    options(cli.progress_show_after = 0)
    cli::cli_progress_bar(format = "Doing file {cli::pb_current} of {cli::pb_total} {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str}", total = length(fls))
  }
  
  # loop over files...
  for (ii in 1:length(fls))  {
    
    if (length(fls)>1)  cli::cli_progress_update()
    #if (verbose) cli::cli_alert(stringr::str_c("Extracting file ", ii, " of ", length(fls)))
    
    # this file name
    fl <- fls[ii]
    
    # get variants list for that file
    varlist_sub <- varlist |> 
      dplyr::filter(filename==fl)
    
    # get CHR
    chr <- varlist_sub$CHR[1]
    
    # path to VCF
    vcf_path <- stringr::str_c("/mnt/project/Bulk/DRAGEN\\ WGS/DRAGEN\\ population\\ level\\ WGS\\ variants\\,\\ pVCF\\ format\\ \\[500k\\ release\\]/chr", chr, "/", fl, " ")
    #if (verbose) cli::cli_alert(stringr::str_c("Path to pVCF: ", vcf_path))
    
    # create empty VCF file to fill
    system("echo '##fileformat=VCFv4.2' > tmp.vcf")
    system(stringr::str_c("zgrep -m 1 '#CHROM' ", vcf_path, " >> tmp.vcf 2>/dev/null"))
    
    # use tabix to extract the positions
    if (verbose) cli::cli_alert("Use tabix to extract the positions")
    system(stringr::str_c(
      "tabix ",
      vcf_path,
      stringr::str_c("chr", chr, ":", varlist_sub$POS-1, "-", varlist_sub$POS, collapse=" "),
      " >> tmp.vcf"
    ))
    
    # subset to just those in the input file - sometimes the grep catches extras due to the tabix requirement of POS-1:POS!
    system("head -n2 tmp.vcf > tmp2.vcf")
    system(stringr::str_c(
      "awk -v list=\"", 
      stringr::str_c(varlist_sub$POS, collapse=","),
      "\" 'BEGIN { split(list, nums, \",\"); for (i in nums) integers[nums[i]] } $2 in integers' tmp.vcf >> tmp2.vcf"
    ))
    system("mv tmp2.vcf tmp.vcf")
    
    # use Plink to convert
    if (verbose) cli::cli_alert("Use plink to convert pVCF to BED")
    c1 <-"./plink --vcf tmp.vcf --set-missing-var-ids @:#:\\$1:\\$2 --make-bed --out tmp"
    if (very_verbose)  {
      system(c1)
    } else {
      system(stringr::str_c(c1, " >/dev/null"))
    }    
    
    # if this is the first one, simply rename
    if (ii==1)  {
      system(paste0("mv tmp.bed ", out_bed, ".bed"))
      system(paste0("mv tmp.bim ", out_bed, ".bim"))
      system(paste0("mv tmp.fam ", out_bed, ".fam"))
    }
    
    # if not the first one, use plink to merge beds
    if (ii>1)  {
      if (verbose) cli::cli_alert("Merge BEDs")
      c1 <- paste0("./plink --bfile ", out_bed, " --bmerge tmp --make-bed --out tmp2")
      if (very_verbose)  {
        system(c1)
      } else {
        system(stringr::str_c(c1, " >/dev/null"))
      }  
      system(paste0("mv tmp2.bed ", out_bed, ".bed"))
      system(paste0("mv tmp2.bim ", out_bed, ".bim"))
      system(paste0("mv tmp2.fam ", out_bed, ".fam"))
    }
    
    # remove tmp files
    system("rm tmp*")

  }
  
  # finished
  if (length(fls)>1)  {
    cli::cli_progress_done()
    options(cli.progress_show_after = 2)
  }
  cli::cli_alert_success(c("DRAGEN BED made!"))
  if (verbose) cli::cli_alert_info(c("Time taken: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
  
}




#' Load BED file into memory
#'
#' @description Use Plink to convert BED to RAW then easily load it
#'
#' @return A data frame
#'
#' @author Luke Pilling
#'
#' @name load_bed
#'
#' @param in_bed A string. BED prefix
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#' @param very_verbose Logical. Be very verbose (show individual steps & show terminal output from Plink etc),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' make_dragen_bed(in_file=readr::read_tsv(system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), n_max=3, progress=FALSE, show_col_types=FALSE), out_bed="test_pgs_liver_cirrhosis")
#'
#' @export
#'
load_bed <- function(
    in_bed,
    verbose=FALSE,
    very_verbose=FALSE
)  {

  # if it's a character string, assume user has provided a file path
  if (class(in_bed)[1] == "character")  {
    
    if (length(in_bed)>1)  cli::cli_abort("Input file path needs to be length 1")
	
    # does input file exist?
    if (! file.exists(stringr::str_c(in_bed, ".bed")))  cli::cli_abort("Input file not found")
    
  }

  # use Plink to convert
  if (verbose) cli::cli_alert("Use Plink to convert BED to RAW text file")
  c1 <- paste0("./plink --bfile ", in_bed, " --recode A --out tmp")
  if (very_verbose)  {
    system(c1)
  } else {
    system(stringr::str_c(c1, " >/dev/null"))
  }
  
  # load data to format / merge
  if (verbose) cli::cli_alert("Read into memory and format")
  bed <- readr::read_delim("tmp.raw", delim=" ", progress=FALSE, show_col_types=FALSE)
  bed <- bed |>
    dplyr::rename(eid=FID) |>
    dplyr::select(-IID, -PAT, -MAT, -SEX, -PHENOTYPE)

  # remove tmp files
  system("rm tmp*")

  # return
  return(bed)
  
}


#
#
# extract_variants
# wrapper function for make bed and load bed



#
#
#
#
#
# create polygenic score of variants in BED file





