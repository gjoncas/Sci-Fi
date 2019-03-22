#setwd("C:/Users/Graham/Documents/R")
library("pscl")   #for pseudo-R^2

#PROGRAM STRUCTURE:
#1. Define year vectors for Hugos & Nebulas + empty data vectors (e.g. hapax)
#2. Define the functions metadata() and refresh()
#3. Hugo data: for (i in h.years.v) {metadata(i,"H")}
#4. Save these as h.hapax.v, etc.
#5. Use refresh() to reset the data vectors to NULL
#6. Nebula data: for (i in n.years.v) {metadata(i,"N")}
#7. Save these as n.hapax.v, etc.
#8. Plot graphs using timeseries(data,award) function
#9. Generate dummies & put regressors into dataframe
#10. Run logistic regression

years.v    <- c(1955:2018)
h.remove.v <- c(1957,2015)    #NB: 1962 is outlier
h.years.v  <- years.v[! years.v %in% h.remove.v]
#
n.remove.v <- c(c(1955:1964),1970,2018)
n.years.v  <- years.v[! years.v %in% n.remove.v]
rm(years.v,h.remove.v,n.remove.v)

#Doubles: N1965_H1966, N1975_H1976, N1977_H1978, N1980_H1981, N2011_H2012, N2016_H2017
#Several versions: H1973, H1987, N2010

     word.count.v <- NULL     #empty vectors (fill by looping through files)
avg.sent.length.v <- NULL
max.sent.length.v <- NULL
 hapax.legomena.v <- NULL
avg.word.length.v <- NULL
  avg.word.freq.v <- NULL

  
refresh <- function() {       #empties data vectors (helpful for debugging)
       word.count.v <<- NULL; avg.sent.length.v <<- NULL; hapax.legomena.v <<- NULL;
  max.sent.length.v <<- NULL; avg.word.length.v <<- NULL;  avg.word.freq.v <<- NULL
  }


metadata <- function(year,award) {      #year is an integer, award is a string 'H' or 'N'
 if (award=="H") award.s <- "Hugos/H"   else
 if (award=="N") award.s <- "Nebulas/N"
    else stop("Award must be 'H' for Hugos or 'N' for Nebulas")
 #NB: I store the texts in two separate folders, one for each award
 #This handles both folder location + preceding H or N in filenames

#story = scan("Sci-Fi Stories/Hugos/H1955.txt", what="character", sep="\n")
story <- paste0("Sci-Fi Stories/", award.s, toString(year),".txt")
text.v <- scan(story, what="character", sep="\n")

text.lower.v <- tolower(text.v)                   #converts to lowercase
text.words.l <- strsplit(text.lower.v, "\\W")     #makes into list, gets rid of punctuation
title = text.words.l[[1]]
if (title[1] == "ï") {                            #get rid of garbage first character "ï"
  text.words.l[[1]] = title[(3:length(title))]    #(why does this happen?)
}
text.word.v  <- unlist(text.words.l)              #makes into one long list
not.blanks.v <- which(text.word.v!="")            #identifies blanks
text.word.v  <- text.word.v[not.blanks.v]         #removes blanks

#average sentence length & largest sentence length         #NB: doesn't account for Mr., 0.5, etc.
     text.v = paste(text.v[3:length(text.v)],collapse=" ") #paste as one long string (minus title)
sentences.v = unlist(strsplit(text.v, "\\!|\\.|\\?"))      #split by end-punctuation
sentences.v = sentences.v[which(sentences.v!="")]          #removes blanks
    s.len.v = NULL
for (i in (1:length(sentences.v))) {                       #get length for each sentence
  sent.words.v = unlist(strsplit(sentences.v[i], " "))
  sent.words.v = sent.words.v[which(sent.words.v!="")]
  s.len.n = length(sent.words.v)
  s.len.v = c(s.len.v,s.len.n)
}
sent.avg.n = round(mean(s.len.v), digits=3)                     #avg for all sentences (rounded)
sent.max.n = max(s.len.v)
avg.sent.length.v <<- c(avg.sent.length.v, sent.avg.n)          #put avg into global variable
max.sent.length.v <<- c(max.sent.length.v, sent.max.n)          #put max into global variable
  #check longest sentence: sentences.v[which.max(s.len.v)]

text.length.n  <- length(text.word.v)                           #NB: using this later (hapax)
 word.count.v <<- c(word.count.v,text.length.n)                 #word count

lengths.v = NULL                                                #vector of word lengths
for (i in (1:length(text.word.v))) {
  lengths.v <- c(lengths.v,nchar(text.word.v[i]))
}
avg.word.length.n  <- round(mean(lengths.v),digits=3)
avg.word.length.v <<- c(avg.word.length.v,avg.word.length.n)    #mean word length (rounded)

   text.freqs.t <- table(text.word.v)                           #frequency table
avg.word.freq.n <- round(mean(text.freqs.t), digits=3)          #mean word frequency (rounded)
avg.word.freq.v <<- c(avg.word.freq.v,avg.word.freq.n)           

hapax.n <- length(which(text.freqs.t==1)) / text.length.n        #hapax as % of text
hapax.legomena.v <<- c(hapax.legomena.v,round(hapax.n,digits=3)) #hapax legomena (rounded)
 }    #end of metadata()


#main program: for-loop on list of years
for (i in h.years.v) {metadata(i,"H")}

#after generating data, assign it to vectors
h.wordcount.v <- word.count.v
    h.hapax.v <- hapax.legomena.v
   h.avglen.v <- avg.word.length.v
  h.avgsent.v <- avg.sent.length.v
  h.maxsent.v <- max.sent.length.v
  h.avgfreq.v <- avg.word.freq.v
refresh()

#re-do the program for Nebula winners
for (i in n.years.v) {metadata(i,"N")}

#'n' for Nebula
n.wordcount.v <- word.count.v
    n.hapax.v <- hapax.legomena.v
   n.avglen.v <- avg.word.length.v
  n.avgsent.v <- avg.sent.length.v
  n.maxsent.v <- max.sent.length.v
  n.avgfreq.v <- avg.word.freq.v
#
rm(word.count.v,hapax.legomena.v,avg.word.length.v,
   avg.sent.length.v,max.sent.length.v,avg.word.freq.v,i)


#use metadata to generate time series plots

#general function to plot data                #small y-axis for n.wordcount.v: c(1000,8500)
timeseries <- function(data,award) {
  
  if (award=="H") {award.s <- "h"; title <- "Hugo Award"} else
    if (award=="N") {award.s <- "n"; title <- "Nebula Award"}
    else stop("Award must be 'H' for Hugos or 'N' for Nebulas")

  if (data=="hapax")   {yaxis <- c(0.05,0.3); ylabel <- "Hapax legomena (%)"}else
  if (data=="wordcount"){yaxis<- c(1000,25000); ylabel <- "Word Count"}      else #outlier: H1962-3
  if (data=="avglen")  {yaxis <- c(3.6,4.65); ylabel <- "Avg Word Length"}   else
  if (data=="avgsent") {yaxis <- c(5,25.5); ylabel <- "Avg Sentence Length"} else #outlier: H2009
  if (data=="maxsent") {yaxis <- c(30,270); ylabel <- "Max Sentence Length"} else #H1976/N1975
  if (data=="avgfreq") {yaxis <- c(2.4,6.3); ylabel <- "Avg Word Frequency"}      #outlier: H1965
  else stop("Data must be 'hapax', 'wordcount', 'avglen', 'avgsent', 'maxsent', or 'avgfreq'")
    
   datavec <- paste0(award.s, ".", data, ".v")
     years <- paste0(award.s, ".years.v")

dataframe <- data.frame(get(years),get(datavec))
plot(dataframe, main = title, type="o", pch=20,
     ylab=ylabel, xlab="Years",
     xlim=c(1955,2018), ylim=yaxis,
     cex.lab=1.55, cex.axis=1.55, cex.main=1.55, cex.sub=1.55)      #enlarge font size
regression <- lm(get(datavec) ~ get(years), data = dataframe)
lines(dataframe$get.years., fitted(regression), col = "blue")
  }  #end of timeseries()


#generate dummy variables to use in regression

h.auth.names <- c('Eric Frank Russell','Arthur C. Clarke','Avram Davidson','Robert Bloch',
                  'Daniel Keyes','Poul Anderson','Brian Aldiss','Jack Vance','Poul Anderson',
                  'Gordon R. Dickson','Harlan Ellison','Larry Niven','Harlan Ellison',
                  'Harlan Ellison','Samuel R. Delany','Larry Niven','R.A. Lafferty','Frederik Pohl',
                  'Ursula K. Le Guin','Larry Niven','Fritz Leiber','Joe Haldeman','Harlan Ellison',
                  'C. J. Cherryh','George R. R. Martin','Clifford D. Simak','John Varley',
                  'Spider Robinson','Octavia E. Butler','David Brin','Frederik Pohl','Greg Bear',
                  'Lawrence Watt-Evans','Mike Resnick','Suzy McKee Charnas','Terry Bisson',
                  'Geoffrey A. Landis','Connie Willis','Connie Willis','Joe Haldeman',
                  'Maureen F. McHugh','Connie Willis','Mike Resnick','Michael Swanwick',
                  'Michael Swanwick','David Langford','Michael Swanwick','Geoffrey A. Landis',
                  'Neil Gaiman','Mike Resnick','David D. Levine','Tim Pratt','Elizabeth Bear',
                  'Ted Chiang','Will McIntosh','Mary Robinette Kowal','Ken Liu','Ken Liu',
                  'John Chu','Naomi Kritzer','Amal El-Mohtar','Rebecca Roanhorse')

#NB: in 2010 both 'Harlan Ellison' and 'Kij Johnson' won the award
n.auth.names <- c('Harlan Ellison','Richard McKenna','Samuel R. Delany','Kate Wilhelm',
                  'Robert Silverberg','Robert Silverberg','Joanna Russ','James Tiptree, Jr.',
                  'Ursula K. Le Guin','Fritz Leiber','Charles L. Grant','Harlan Ellison',
                  'Edward Bryant','Edward Bryant','Clifford D. Simak','Lisa Tuttle','Connie Willis',
                  'Gardner Dozois','Gardner Dozois','Nancy Kress','Greg Bear','Kate Wilhelm',
                  'James K. Morrow','Geoffrey A. Landis','Terry Bisson','Alan Brennert',
                  'Connie Willis','Joe Haldeman','Martha Soukup','Esther Friesner',
                  'Esther Friesner','Jane Yolen','Bruce Holland Rogers','Leslie What',
                  'Terry Bisson','Severna Park','Carol Emshwiller','Karen Joy Fowler',
                  'Eileen Gunn','Carol Emshwiller','Elizabeth Hand','Karen Joy Fowler',
                  'Nina Kiriki Hoffman','Kij Johnson','Harlan Ellison','Ken Liu',
                  'Aliette de Bodard','Rachel Swirsky','Ursula Vernon','Alyssa Wong',
                  'Amal El-Mohtar','Rebecca Roanhorse')

multiwins <- function(auth.names) {
  n <- length(auth.names)
  repeats.v = vector(length=n)
  for (i in (1:n)) {
    repeats.v[i] <- auth.names[i] %in% auth.names[-i]
  }
    return(as.integer(repeats.v))     #returns vector of 1's (multiple wins) & 0's (unique)
}   #end of multiwins()

h.repeats.v <- multiwins(h.auth.names)
n.repeats.v <- multiwins(n.auth.names)

#dummy variable for first-person (1) vs. third-person or second-person (0)
h.tense.v <- c(3,1,3,3,1,1,3,3,3,1,3,1,1,3,1,3,1,3,1,1,1,3,1,3,1,3,3,1,3,1,3,
               3,1,1,1,1,3,1,1,1,1,1,1,3,1,3,3,3,1,1,3,3,3,1,3,3,1,1,1,1,3,2)
n.tense.v <- c(3,1,1,3,1,1,1,1,3,1,1,1,1,3,3,1,1,3,3,1,3,3,3,1,1,1,1,1,3,3,1,
               1,3,3,1,3,1,1,3,1,1,1,1,3,1,1,3,1,3,1,3,2)
h.tense.v[h.tense.v != 1] <- 0    #first-person = 1, else 0
n.tense.v[n.tense.v != 1] <- 0

#dummy variable for author gender (male = 0, female = 1)
h.auth.gender <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1,0,0,
                   0,0,0,1,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,1)
n.auth.gender <- c(0,0,0,1,0,0,1,1,1,0,0,0,0,0,0,1,1,0,0,1,0,1,0,0,0,0,1,0,1,1,1,
                   1,0,1,0,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1)
#                                            |--NB: 2010 (0) has two winners 0/1


#assembles all variables together (H, then N) to put in regression
h.dummies.v = rep(1,length(h.years.v))
n.dummies.v = rep(0,length(n.years.v))
#
years <- c(h.years.v,n.years.v)
award <- c(h.dummies.v, n.dummies.v)              #Hugos (1), Nebulas (0)
words <- c(h.wordcount.v, n.wordcount.v)*0.001    #number of words (in thousands)
hapax <- c(h.hapax.v, n.hapax.v)*10               #hapax legomena percentage
w.len <- c(h.avglen.v, n.avglen.v)                #average word length
w.frq <- c(h.avgfreq.v, n.avgfreq.v)              #average word frequency
s.avg <- c(h.avgsent.v,n.avgsent.v)               #average sentence length
s.max <- c(h.maxsent.v,n.maxsent.v)               #length of longest sentence
a.sex <- c(h.auth.gender,n.auth.gender)           #dummy: author gender (female=1)
tense <- c(h.tense.v,n.tense.v)                   #dummy: first-person (1) or other (0)
multi <- c(h.repeats.v,n.repeats.v)               #dummy: author with multiple wins (1)


#to run different regressions, put the variables you want into scifi, then use logit
#scifi = data.frame(award,words,hapax,w.len,w.frq,s.avg,s.max,a.sex,tense,multi)
scifi <- data.frame(award,w.len,w.frq,a.sex)  #words,hapax,s.avg,s.max,multi,tense

logit <- glm(award ~.,family=binomial(link='logit'),data=scifi[-7,])  #-7 omits H1962
print(summary(logit))
print(pR2(logit))       #McFadden is logit analogue of R^2 -- NB: need "pscl" package

#anova(logit, test="Chisq")
#probit <- glm(award ~.,family=binomial(link='probit'),data=scifi)
#summary(probit)


  #NB: removing these means you can't run alternate regressions or use timeseries()
#rm(h.avgfreq.v,h.avglen.v,h.hapax.v,h.wordcount.v,h.years.v)
#rm(n.avgfreq.v,n.avglen.v,n.hapax.v,n.wordcount.v,n.years.v)
#rm(h.auth.names,n.auth.names,h.auth.gender,n.auth.gender,h.dummies,n.dummies)   #freqs
#rm(h.repeats.v,n.repeats.v,h.tense,n.tense,h.avgsent.v,n.avgsent.v,h.maxsent.v,n.maxsent.v)
#rm(award,years,words,hapax,w.len,w.frq,s.avg,s.max,a.sex,tense,multi)