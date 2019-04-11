library(boot)
mylogit <-glm(dead ~ BornNobility+BirthYear+Culture+Gender+Hometown+siblings+Occupation+SlaveProstitute+Hometown+timeperEp+children+Married+NotPresentButNotDead, data = CharactersTrunc)
summary(mylogit)

#Logit Bootstrap 

mylogit.bootstrap = function ( data, indices){
  d = data[indices,]
  mylogit <-glm(dead ~ BornNobility+BirthYear+Culture+Gender+Hometown+siblings+Occupation+SlaveProstitute+Hometown+timeperEp+children+Married+NotPresentButNotDead, data = d)
 return ( coef(mylogit))

}

set.seed(12)

logit.boot = boot( data = CharactersTrunc, statistic = mylogit.bootstrap, R = 9999)

logit.boot
