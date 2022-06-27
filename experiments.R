


library(data.table)
library(ggplot2)
library(plyr)
library(cowplot)
library('RJSONIO')


# fixing random state (comment this line for real randomness)
# set.seed(2022)


# list the content of a dir, only regular files, no subdirs
#
readDataDir <- function(dir, ignore.filename.if.contains.dot=TRUE) {
  files <- list.files(dir,all.files = FALSE,recursive = FALSE,include.dirs = FALSE,no..=TRUE)
  r<-ldply(files, function(filename) {
    # to ignore .POS or .tok or any spurious files; the normal data filenames are not supposed to contain '.'
    if (!ignore.filename.if.contains.dot | (length(grep('.',filename,fixed=TRUE))==0)) {
      l <- strsplit(filename, '-',fixed=TRUE )
      parts <- l[[1]]
      year <- parts[1]
      author <- parts[2]
      title <- paste(parts[3:length(parts)],collapse ='-')
      data.frame(year=year, author=author, title=title, filename=filename)
    }
  })
  if (nrow(r) != 554) {
    warning(paste('Normally there should be 554 documents, found',nrow(r)))
  }
  r
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
#    print(paste('    Picking no',i,'.',length(pickedAT),' books already picked; ', nrow(availableDT),'pairs available.'))
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
buildTrainOrTestSet <- function(dataSplitByAuthor, nbCases, train.set, propPositive=.5, withReplacement=FALSE,silent=FALSE) {
  dt <- as.data.table(dataSplitByAuthor[dataSplitByAuthor$train.set==train.set,])
  setkey(dt, author, title)
  dt[,dummy := rep(1,nrow(dt))]
  # cartesian product
  cp <- merge(dt,dt,by='dummy',allow.cartesian=TRUE)
  cp[,dummy:=NULL]
  if (!silent) print(paste('Cartesian product: ',nrow(cp)))
  # remove same book
  cp <- cp[filename.x != filename.y,]
  if (!silent) print(paste('Removing pairs with same book: ',nrow(cp)))
  # label as same author or not
  cp[, same.author := author.x == author.y,]
  if (!silent) print(paste('pairs same author:',nrow(cp[same.author==TRUE,])))
  if (!silent) print(paste('pairs diff author:',nrow(cp[same.author==FALSE,])))
  same <- cp[same.author==TRUE,]
  diff <- cp[same.author==FALSE,]
  if (withReplacement) {
    if (!silent) print('picking with replacement (same book allowed several times)')
    # avoid duplicates by symmetry like (x,y) = (y,x)
    same <- same[filename.x < filename.y,]
    diff <- diff[filename.x < filename.y,]
    r <- rbind(same[sample(nrow(same),round(propPositive*nbCases)),], diff[sample(nrow(diff),(1-propPositive)*nbCases),])
  } else {
    if (!silent) print('picking without replacement (a book can appear only once):')
    # easy way to create an empty data.table with same columns as cp:
    r <- cp[filename.x!=filename.x,]
    if (!silent) print(' *** (1) same author')
    r <- pickPairsOfDistinctBooks(same, r, round(propPositive*nbCases))
    if (!silent) print(' *** (2) different author')
    r <- pickPairsOfDistinctBooks(diff, r, round((1-propPositive)*nbCases))
    r
  }
  if (!silent) print('shuffling rows')
  r[sample(1:nrow(r)),]
}


#
# d <- readDataDir()
# dataSplitByAuthor <- splitAuthors(d)
#
buildFullDataset <- function(dataSplitByAuthor, nbCasesTrain, nbCasesTest, propPositive=.5, withReplacement=FALSE) {
  print('*** TRAIN SET')
  trainDT <- buildTrainOrTestSet(dataSplitByAuthor, nbCasesTrain, TRUE, propPositive, withReplacement)
  trainDT[, train.or.test := 'train']
  print('*** TEST SET')
  testDT <- buildTrainOrTestSet(dataSplitByAuthor, nbCasesTest, FALSE, propPositive, withReplacement)
  testDT[, train.or.test:= 'test']
  r <- rbind(trainDT, testDT)
  r[, train.set.x:= NULL]
  r[, train.set.y:= NULL]
  r[,author.seen.x :=  NULL]
  r[,author.seen.y :=  NULL]
  r[, case.id := paste(filename.x, filename.y)]
  r[same.author==FALSE, answer := 0, ]
  r[same.author==TRUE,  answer := 1, ]
  r <- assignAuthorsSeenInTraining(r)
  r
}

# fullDataset <- buildFullDataset(...)
saveDatasetInCasesFormat <- function(fullDataset, dir='.', trainFile='train.tsv', testFile='test.tsv') {
  trainset <- fullDataset[train.or.test=='train',.(case.id,answer)]
  testset <- fullDataset[train.or.test=='test',.(case.id,answer)]
  fwrite(trainset, paste(dir,trainFile,sep='/'), sep='\t', col.names=FALSE)
  fwrite(testset, paste(dir,testFile,sep='/'), sep='\t', col.names=FALSE)
}


#
# assumes an already computed "full dataset":
# fullDataset <- buildFullDataset(...)
#
assignAuthorsSeenInTraining <- function(fullDataset,silent=FALSE) {
  trainset <- fullDataset[train.or.test=='train',]
  train.authors <- unique(c(trainset[,author.x], trainset[,author.y]))
  if (!silent) print(paste(length(train.authors),'authors in the training set'))
  fullDataset[,author.seen :=  author.x %in% train.authors | author.y %in% train.authors]
  testset <- fullDataset[train.or.test=='test',]
  if (!silent) print(paste("proportion of 'author seen in training' in the test set:", nrow(testset[author.seen==TRUE,])/nrow(testset)))
  fullDataset
}


#
# assumes an already computed "full dataset":
# fullDataset <- buildFullDataset(...)
#
restrictTrainingSetDiversity <- function(fullDataset) {
  trainsetX <- fullDataset[train.or.test=='train',]
  trainsetX[,year := year.x]
  trainsetX[,author := author.x]
  trainsetX[,title := title.x]
  trainsetX[,filename := filename.x]
  trainsetY <- fullDataset[train.or.test=='train',]
  trainsetY[,year := year.y]
  trainsetY[,author := author.y]
  trainsetY[,title := title.y]
  trainsetY[,filename := filename.y]
  nb.cases <- nrow(trainsetX)
  trainset <- rbind(trainsetX[,c('year','author','title','filename')],trainsetY[,c('year','author','title','filename')])
  trainset[,train.set := TRUE]
  nb.books.by.author<-trainset[,nrow(.SD),by=author]
  setnames(nb.books.by.author,'V1','n.books')
  nb.books.by.author <- nb.books.by.author[order(-n.books),]
  nb.books.by.author
  l <- list()
  l[[1]] <- 'NA'
  for (n in 2:nrow(nb.books.by.author)) {
    subset.authors <- nb.books.by.author[1:n,author]
    train.subset <- trainset[author %in% subset.authors,]
    print(paste('#### Generating training set with',n,'authors. Nb books:',nrow(train.subset)))
    s <- buildTrainOrTestSet(train.subset,nb.cases,train.set = TRUE,withReplacement = TRUE,silent=TRUE)
    s[, train.or.test := 'train']
    s[, train.set.x:= NULL]
    s[, train.set.y:= NULL]
    r <- rbind(s, fullDataset[train.or.test=='test',], fill=TRUE)
    r[, case.id := paste(filename.x, filename.y)]
    r[same.author==FALSE, answer := 0, ]
    r[same.author==TRUE,  answer := 1, ]
    assignAuthorsSeenInTraining(r)
    l[[n]] <- r
  }
  l
#  trainset
}


buildTrainOrTestSetWithGroups <- function(dataSplitByAuthor, nbCases, train.set, propPositive=.5, group1.sizes=1:5, group2.sizes=1) {
  print(group1.sizes)
  dt <- as.data.table(dataSplitByAuthor[dataSplitByAuthor$train.set==train.set,])
  setkey(dt, author, title)
  authors <- as.data.table(booksByAuthor(dt))
  positive <- sample(nbCases, round(propPositive*nbCases))
  l <- list()
  for (i in 1:nbCases) {
    group1.size <- group1.sizes[sample(length(group1.sizes),1)]
    group2.size <- group2.sizes[sample(length(group2.sizes),1)]
    if (i %in% positive) {
#      print('POS')
      selectableAuthors <- authors[n>=group1.size+group2.size,author]
#      print(selectableAuthors)
      this.author <- selectableAuthors[sample(length(selectableAuthors),1)]
#      print(paste('selected:',this.author))
      selectable.books <- dt[author==this.author,]
      picked <- selectable.books[sample(nrow(selectable.books),group1.size+group2.size),]
      gp1 <- sample(nrow(picked),group1.size)
      gp2 <- !(1:nrow(picked) %in% gp1)
      group1 <- picked[gp1,]
      group2 <- picked[gp2,]
    } else {
#      print('NEG')
      selectableAuthors1 <- authors[n>=group1.size,author]
#      print(selectableAuthors1)
      author1 <- selectableAuthors1[sample(length(selectableAuthors1),1)]
      selectableAuthors2 <- authors[n>=group2.size & author != author1,author]
#      print(selectableAuthors2)
      author2 <- selectableAuthors2[sample(length(selectableAuthors2),1)]
#      print(paste('selected:',author1,author2))
      selectable.books1 <- dt[author==author1,]
      group1 <- selectable.books1[sample(nrow(selectable.books1),group1.size),]
      selectable.books2 <- dt[author==author2,]
      group2 <- selectable.books2[sample(nrow(selectable.books2),group2.size),]
    }
#    print(group1)
#    print(group2)
    r1 <- data.table(author.x=group1[1,author],filename.x=paste(group1[,filename],collapse=':'),author.y=group2[1,author],filename.y=paste(group2[,filename],collapse=':'), same.author=(i %in% positive), train.set=train.set)
    l[[i]] <- r1
    
  }
  rbindlist(l)
#  r<-rbindlist(l)
#  print(paste('DEBUG', length(unique(c(r$author.x,r$author.y)))))
#  r
}


#
# d <- readDataDir()
# dataSplitByAuthor <- splitAuthors(d)
#
buildFullDatasetWithGroups <- function(dataSplitByAuthor, nbCasesTrain, nbCasesTest, propPositive=.5, group1.sizes=1:5, group2.sizes=1) {
  print('*** TRAIN SET')
  trainDT <- buildTrainOrTestSetWithGroups(dataSplitByAuthor, nbCasesTrain, TRUE, propPositive, group1.sizes, group2.sizes)
  trainDT[, train.or.test := 'train']
  print('*** TEST SET')
  testDT <- buildTrainOrTestSetWithGroups(dataSplitByAuthor, nbCasesTest, FALSE, propPositive, group1.sizes, group2.sizes)
  testDT[, train.or.test:= 'test']
  r <- rbind(trainDT, testDT)
  r[, case.id := paste(filename.x, filename.y)]
  r[same.author==FALSE, answer := 0, ]
  r[same.author==TRUE,  answer := 1, ]
  r <- assignAuthorsSeenInTraining(r)
  r
}


##### RESULTS AND ANALYSIS


# 

readExperimentResults <- function(expe.dir, perf.col='perf.final') {
  d<-fread(paste(expe.dir, 'results/results.tsv', sep='/'))
  p <- d[,..perf.col]
  d[,perf := p] 
  d
}

default.font.size <- 14
default.legend.font.size <- 9
default.legend.title.font.size <- 10

comparePerfsByEvalOn <- function(resultsDT,diff.seen=FALSE, by.model.type=FALSE, y.range=c(0,1),font.size=default.font.size,x.label='variable',y.label='performance',legend.pos=c(.5,.1),legend.font.size=default.legend.font.size,legend.title.font.size=default.legend.title.font.size) {
  d <- resultsDT[selected.by.training==TRUE,]
  if (diff.seen) {
    d <- d[evaluated.on=='test.seen' | evaluated.on=='test.unseen',]
  } else {
    d <- d[evaluated.on=='test' | evaluated.on=='train',]
  }
  ggplot(d, aes(variable,perf,colour=evaluated.on))+geom_point()+geom_line(alpha=.7)+ylim(y.range)+ geom_smooth(method = "lm",linetype='dashed', alpha=.25)+
    theme(text=element_text(size=font.size),
                legend.text=element_text(size=legend.font.size), 
                legend.position = legend.pos, 
                legend.title=element_text(size=legend.title.font.size),
          legend.direction="horizontal")+
    xlab(x.label)+ylab(y.label)
}

perfByModelType <- function(resultsDT, y.range=c(0,1),x.range=NA,font.size=default.font.size,x.label='variable',y.label='performance',legend.pos=c(.5,.1),legend.font.size=default.legend.font.size,legend.title.font.size=default.legend.title.font.size) {
  d <- resultsDT[evaluated.on=='test',]
  g<-ggplot(d, aes(variable,perf,colour=model.type))+geom_point(size=3)+geom_line()+ylim(y.range)+
       geom_point(data=d[selected.by.training==TRUE,],aes(variable,perf),shape=22,size=5, stroke = 1,colour = "black")+
       theme(text=element_text(size=font.size),
            legend.text=element_text(size=legend.font.size), 
            legend.position = legend.pos, 
            legend.title=element_text(size=legend.title.font.size),
            legend.direction="horizontal")+
       xlab(x.label)+ylab(y.label)
  if (!is.na(x.range)) {
    g <- g + xlim(x.range)
  }
  g
}

# call: statsByModelType(list(d1,d2,d3,d4))
statsByModelType <- function(dtList) {
  for (i in 1:length(dtList)) {
    dtList[[i]][,expe.id:=i]
  }
  r<-rbindlist(dtList)
  r<-r[evaluated.on=='test',]
  setkey(r,expe.id,variable)
  r[,is.best:=(perf==max(perf,na.rm=TRUE)),by=key(r)]
  best <-r[is.best==TRUE,]
  best.count <- best[,nrow(.SD),by=model.type]
  setnames(best.count,'V1', 'times.best')
  best.count[,prop.best := times.best/nrow(best)]

  selected <- r[selected.by.training==TRUE,]
  selected.count <- selected[,nrow(.SD),by=model.type]
  setnames(selected.count,'V1', 'times.selected')
  selected.count[,prop.selected := times.selected/nrow(selected)]
  res1 <- merge(best.count,selected.count)

  both <- r[is.best & selected.by.training,]
  both.count <- both[,nrow(.SD),by=model.type]
  setnames(both.count,'V1','times.selected.and.best')
  total.both <- sum(both.count[,times.selected.and.best])
  print(paste('Best model is the one selected by training: ',total.both,'/',nrow(selected),'(',total.both/nrow(selected)*100,'%)'))
  
  res2 <- merge(res1, both.count,all.x=TRUE)
  res2[is.na(times.selected.and.best),times.selected.and.best:=0]
  res2[,prob.best.given.selected := times.selected.and.best / times.selected]
  res2

}

refCase <- function(d1, d2, d3, d4) {
  rbind(d1[variable==100,], d2[variable==1,], d3[variable==100,], d4[variable==12,])
}


convertFullExperimentsToJSON <- function(inputDir, outputDir) {
  dir.create(outputDir, showWarnings = FALSE)
  
  print('expe 1')
  outDir <- paste(outputDir,'1-doc-size',sep='/')
  dir.create(outDir, showWarnings = FALSE)
  inputExpeDir <- paste(inputDir,'1-doc-size',sep='/')
  full <- paste(inputExpeDir,'full-dataset.tsv',sep='/')
  for (N in c(seq(from=20,to=200,by=20),seq(from=100,to=1000,by=100))) {
    convertFullToJSON(full, paste0(outDir,'/',as.character(N)), N, inputExpeDir)
  }
  
  print('expe 3')
  outDir <- paste(outputDir,'3-training-data-size',sep='/')
  dir.create(outDir, showWarnings = FALSE)
  inputExpeDir <- paste(inputDir,'3-training-data-size',sep='/')
  for (N in seq(from=100,to=1000,by=100)) {
    full <- paste(inputExpeDir,as.character(N),'full-dataset.tsv',sep='/')
    convertFullToJSON(full, paste0(outDir,'/',as.character(N)), N, inputExpeDir)
  }
  
  print('expe 4')
  outDir <- paste(outputDir,'4-author-diversity',sep='/')
  dir.create(outDir, showWarnings = FALSE)
  inputExpeDir <- paste(inputDir,'4-author-diversity',sep='/')
  for (N in 2:12) {
    full <- paste(inputExpeDir,as.character(N),'full-dataset.tsv',sep='/')
    convertFullToJSON(full, paste0(outDir,'/',as.character(N)), N, inputExpeDir)
  }
  
}


convertFullToJSON <- function(fullDataFile, outputprefix, variable, expeDir) {
  full <- fread(fullDataFile)
  full$id <- 1:nrow(full)
  trainset <- full[train.or.test=='train',]
  testset <- full[train.or.test=='test',]
  writeDatasetToJSON(trainset,paste0(outputprefix,'.train'), variable, expeDir)
  writeDatasetToJSON(testset,paste0(outputprefix,'.test'), variable, expeDir)
  }

writeDatasetToJSON <- function(dataset, outputprefix, variable, expeDir) {
  truth.json<-ddply(dataset, 'id', function(x) {
    gsub('\n',' ',toJSON(
      list(id=as.character(x$id),same=x$same.author, authors= I(list(x$author.x,x$author.y)))
      ,.escapeEscapes = FALSE)
      , fixed = TRUE)
  })
  writeLines(truth.json$V1, paste0(outputprefix,'-truth.jsonl'))
  pairs.json <- ddply(dataset, 'id', function(x) {
    path1 <- paste(expeDir, variable, 'data',x$filename.x, sep='/')
    path2 <- paste(expeDir, variable, 'data',x$filename.y, sep='/')
    text1 <- readTextDoc(path1)
    text2 <- readTextDoc(path2)
    gsub('\n',' ',toJSON(
      list(id=as.character(x$id),fandoms=I(list("NA","NA")), pair= I(list(text1, text2)))
      ,.escapeEscapes = FALSE)
      , fixed = TRUE)
  })
  writeLines(pairs.json$V1, paste0(outputprefix,'.jsonl'))
  
}

readTextDoc <- function(filename, concatenateAsOneString=TRUE) {
 t <- readLines(filename) 
 if (concatenateAsOneString) {
  t<- paste0(t, collapse=' ') 
 }
}

