block_to_bootID <- function (block, bootID, nRows, nBoot, resamp) {
  tb = table(block)
  nLevels = length(tb)
  if (any(tb != nRows/nLevels) && resamp != "case") {
    stop("Sorry, unless you are using case resampling, block needs to be a balanced factor - same number of rows for each level. Try using resamp='case'.")
  } else  {
    blockIDs = vector("list",nLevels)
    for(i.level in 1:nLevels)
      blockIDs[[i.level]] = which(block==names(tb)[i.level])
    unlistIDs = unlist(blockIDs) # needed to match each resampled observation with its correct location
  }
  # then each iteration...
  # generate a bootID matrix if required
  if(is.null(bootID)){
    samp <- matrix(sample(nLevels, nLevels * nBoot, replace=TRUE), ncol=nLevels)
  } else {
    samp <- bootID
  }
  # fill the rest of the nboot with NA's
  # maxNboot <- max(sapply(1:nBoot, function(x) {
  #    length(unlist(blockIDs[samp[x,]]))
  #  }))
  # print(paste('maxNboot', maxNboot))
  bootID <-  matrix(NA,nBoot,nRows)
  for(iBoot in 1:nBoot) {
    # unlistIDs is needed to make sure each unlisted blockID ends up in the right place
    boot_ids <- unlist(blockIDs[samp[iBoot,]])
    if(length(boot_ids) != nRows) {
      boot_ids <- sample(boot_ids, nRows, replace = T)
    }
    bootID[iBoot, ] = boot_ids 
  }
  bootID = bootID - 1 #to fit the format in C, 0 to nRows, C uses 0 indexing
  if(interactive()) cat("Using block resampling...\n")
  bootID
}
