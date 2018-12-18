set.seed(12121212)
question1<-rnorm(237, mean=5, sd=5)

set.seed(956432)
id<-sample(unlist(sapply(1:25, function(x) rep(x,sample(1:8,1)))))
BP<-ceiling(c(rnorm(length(id)/2, mean=150, sd=10), rnorm(length(id)/2, mean=130, sd=5)))
question4<-data.frame(studyid=id, BP=BP)
rm(id)
rm(BP)
