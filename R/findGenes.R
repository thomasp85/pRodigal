#' Runs prodigal on AAStringSet
#' 
#' @param genome A DNAStringSet to search in (usually a full genome)
#' 
#' @return An AAStringSet with the proteins detected
#' 
#' @importFrom Biostrings writeXStringSet readAAStringSet
#' @importFrom stringr str_extract
#' 
#' @export
#' 
findGenes <- function(genome, translate=TRUE, runEdge=TRUE, mask=FALSE, meta=FALSE, motifs=FALSE, trainingFile=NULL, quiet=TRUE) {
    origNames <- sub('\\s.*$', '', names(genome))
    names(genome) <- 1:length(genome)
    tempFasta <- tempfile(fileext = '.fasta')
    writeXStringSet(genome, filepath = tempFasta)
    prodigalPath <- system.file('prodigal', 'prodigal', package = 'pRodigal')
    tempGff <- tempfile()
    tempRes <- tempfile(fileext = '.fasta')
    command <- paste0(prodigalPath, ' -i ', tempFasta, ' -o ', tempGff, ' -f gff', ifelse(quiet, ' -q', ''))
    command <- paste0(
        command, 
        ifelse(translate, ' -a ', ' -d '), tempRes,
        ifelse(runEdge, '', ' -c'),
        ifelse(mask, ' -m', ''),
        ifelse(meta, ' -p meta', ' -p single'),
        ifelse(motifs, ' -m', ''),
        ifelse(is.null(trainingFile), '', ' -t'), trainingFile
    )
    system(command)
    res <- readAAStringSet(tempRes)
    rm(tempGff, tempFasta, tempRes)
    names(res) <- sapply(names(res), function(x) {
        ind <- as.integer(str_extract(x, '^\\d+'))
        sub('^\\d+', origNames[ind], x)
    })
    res
}
trainProdigal <- function(genome, filepath) {
    
}
prodigalVersion <- function() {
    prodigalPath <- system.file('prodigal', 'prodigal', package = 'pRodigal')
    system2(prodigalPath, '-v')
}