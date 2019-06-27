#Generate dummy data
getDummyData <- function() {
  
  dummyPeople <- c('Hannah G-Robyn',
                   'Deanna Withey',
                   'Pat McGuire',
                   'Shawn Fabozzi',
                   'Sharon Howe',
                   'Maria Tuzi',
                   'Roseanna Cahill',
                   'Phil Raney',
                   'Sherri Zander',
                   'Diane Beringer',
                   'Janine Johnston',
                   'Babbette Lane',
                   'Michele Gerber',
                   'Ruthann Wheatman',
                   'Sharon Lindhe',
                   'Terri Henry',
                   'Rachel Larson')
  
  dummyErrors <- c('Recon Error',
                   'BJ Drop',
                   'Deposit Error',
                   'Cashier open/close form',
                   'Change bank',
                   'Cleaner Tips',
                   'Tips',
                   'Did not post Table games tips',
                   'Did not sign',
                   'Did not specify misc',
                   'Did not submit cash advance',
                   'Did not submit receipt',
                   'Did not submit synopsis',
                   'Did not verify Cage',
                   'Did not void',
                   'EXCHANGE ERROR',
                   'F&B/Bar Tips',
                   'Incorrectly posted jackpots',
                   'JACKPOT SIGNATURE',
                   'Kiosk boxes',
                   'Misc on Recon',
                   'Signature',
                   'Promos BJ/GRC',
                   'TG tips',
                   'TITO/JP on recon',
                   'Verify Cage',
                   'Verify Tips',
                   'VOID')
  
  datesdf <- data.frame(DATE = seq(as.Date("2019/01/01"), by = "day", length.out = 30))
  datesdf
  
  fdatesdf <- data.frame()
  fpeopledf <- data.frame()
  ferrordf <- data.frame()
  final <- data.frame()
  for (i in c(1:17)) {
    d <- data.frame(DATE = sample(datesdf[,'DATE'], 40, replace = T))
    p <- data.frame(PEOPLE = sample(dummyPeople, 40, replace = T))
    e <- data.frame(ERROR = sample(dummyErrors, 40, replace = T))
    
    fdatesdf <- rbind(fdatesdf, d)
    fpeopledf <- rbind(fpeopledf, p)
    ferrordf <- rbind(ferrordf, e)
    final <- cbind(fdatesdf, fpeopledf, ferrordf, DEP = "CAGE")
    
  }
  final
}
