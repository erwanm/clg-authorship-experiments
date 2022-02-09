
#todo:
#  - update "seen author"?
#  - save dataset
#  - prepare expe diversity


library(data.table)
library(ggplot2)
library(plyr)

# fixing random state (comment this line for real randomness)
# set.seed(2022)


# list the content of a dir, only regular files, no subdirs
#
readDataDir <- function(dir) {
  files <- list.files(dir,all.files = FALSE,recursive = FALSE,include.dirs = FALSE,no..=TRUE)
  ldply(files, function(filename) {
    l <- strsplit(filename, '-',fixed=TRUE )
    parts <- l[[1]]
    year <- parts[1]
    author <- parts[2]
    title <- paste(parts[3:length(parts)],collapse ='-')
    data.frame(year=year, author=author, title=title, filename=filename)
  })
}


booksByAuthor <- function(d) {
  r<-ddply(d, 'author', function(b) {data.frame(n=nrow(b))})
  r[order(r$n),]
}


# apply splitting method:
# 1) split by authors into 3 groups in order to keep some authors unseen in training: train-only, shared, test-only
#    (authors with only one book arbitrarily assigned to test set)
# 2) for shared authors, split their books between training and test (a book never appears in both train and test set)
splitAuthors <- function(d,prop.authors.train.only=0.33, prop.authors.test.only=0.33, prop.shared.books=0.5) {
  byauth <- booksByAuthor(d)
  byauthmin2 <- byauth[byauth$n>1,]
  print(paste(nrow(byauth),' authors (', nrow(byauthmin2),'with at least 2 books )'))
  train.authors <- sample(byauthmin2$author, round(nrow(byauthmin2)*prop.authors.train.only),replace = FALSE)
  left <- byauthmin2[!byauthmin2$author %in% train.authors,]
  test.authors <- sample(left$author, round(nrow(byauthmin2)*prop.authors.test.only),replace = FALSE)
  test.authors <- c(test.authors,byauth[byauth$n==1,'author'])
  shared.authors <- unique(left[!left$author %in% test.authors,'author'])
  print(paste('authors in train-only: ',paste(train.authors,collapse=',')))
  print(paste('authors in test-only: ',paste(test.authors,collapse=',')))
  print(paste('authors in shared: ',paste(shared.authors,collapse=',')))
  shared.books <- d[d$author %in% shared.authors, ]
  train.shared.books <- shared.books[sample(nrow(shared.books), round(nrow(shared.books)*prop.shared.books), replace=FALSE),]
  print(paste('books from shared authors in the training set: ',nrow(train.shared.books)))
  train.all.books <- rbind(train.shared.books, d[d$author %in% train.authors & ! d$author %in% shared.authors,]) 
  print(paste('all books in the training set: ',nrow(train.all.books)))
  train.all.books$train.set <- TRUE
  full <- merge(train.all.books,d,all.y=TRUE)
  full[is.na(full$train.set),]$train.set <- FALSE
  full$author.seen <- full$author %in% shared.authors
  print(paste('books from shared authors in the test set: ',nrow(full[full$train.set==FALSE & full$author.seen==TRUE,])))
  print(paste('books NOT from shared authors in the test set: ',nrow(full[full$train.set==FALSE & full$author.seen==FALSE,])))
  print(paste('all books in the test set: ',nrow(full[full$train.set==FALSE,])))
  full
}


#
# pick pairs of distinct books, i.e. returns a list of pairs where no book appears twice
#
pickPairsOfDistinctBooks <- function(populationDT, alreadyPickedDT, nbToPick) {
  # vector of book ids (filenames) already picked
  pickedAT <- c(alreadyPickedDT[,filename.x], alreadyPickedDT[,filename.y])
  for (i in 1:nbToPick) {
    availableDT <- populationDT[!(filename.x %in% pickedAT) & !(filename.y %in% pickedAT),]
    print(paste('    Picking no',i,'.',length(pickedAT),' books already picked; ', nrow(availableDT),'pairs available.'))
    if (nrow(availableDT) < 1) {
      stop('Error: not enough data to pick pairs of distinct books. Try option "with replacement".')
    }
    picked1 <- availableDT[sample(nrow(availableDT),1),]
    pickedAT <- c(pickedAT, picked1$filename.x, picked1$filename.y)
    alreadyPickedDT <- rbind(alreadyPickedDT,picked1)
  }
  alreadyPickedDT
}


#
# d <- readDataDir()
# dataSplitByAuthor <- splitAuthors(d)
#
buildTrainOrTestSet <- function(dataSplitByAuthor, nbCases, train.set, propPositive=.5, withReplacement=FALSE) {
  dt <- as.data.table(dataSplitByAuthor[dataSplitByAuthor$train.set==train.set,])
  setkey(dt, author, title)
  dt[,dummy := rep(1,nrow(dt))]
  # cartesian product
  cp <- merge(dt,dt,by='dummy',allow.cartesian=TRUE)
  print(paste('Cartesian product: ',nrow(cp)))
  # remove same book
  cp <- cp[filename.x != filename.y,]
  print(paste('Removing pairs with same book: ',nrow(cp)))
  # label as same author or not
  cp[, same.author := author.x == author.y,]
  print(paste('pairs same author:',nrow(cp[same.author==TRUE,])))
  print(paste('pairs diff author:',nrow(cp[same.author==FALSE,])))
  same <- cp[same.author==TRUE,]
  diff <- cp[same.author==FALSE,]
  if (withReplacement) {
    print('picking with replacement (same book allowed several times)')
    r <- rbind(same[sample(nrow(same),round(propPositive*nbCases)),], diff[sample(nrow(diff),(1-propPositive)*nbCases),])
  } else {
    print('picking without replacement (a book can appear only once):')
    # easy way to create an empty data.table with same columns as cp:
    r <- cp[filename.x!=filename.x,]
    print(' *** (1) same author')
    r <- pickPairsOfDistinctBooks(same, r, round(propPositive*nbCases))
    print(' *** (2) different author')
    r <- pickPairsOfDistinctBooks(diff, r, round((1-propPositive)*nbCases))
    r
  }
  print('shuffling rows')
  r[sample(1:nrow(r)),]
}


#
# d <- readDataDir()
# dataSplitByAuthor <- splitAuthors(d)
#
buildFullDataset <- function(dataSplitByAuthor, nbCasesTrain, nbCasesTest, propPositive=.5, withReplacement=FALSE) {
  trainDT <- buildTrainOrTestSet(dataSplitByAuthor, nbCasesTrain, TRUE, propPositive, withReplacement)
  testDT <- buildTrainOrTestSet(dataSplitByAuthor, nbCasesTest, FALSE, propPositive, withReplacement)
  rbind(trainDT, testDT)
}
