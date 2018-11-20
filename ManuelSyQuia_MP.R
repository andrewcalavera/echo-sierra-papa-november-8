###HW2 - Machine Problems
#2. Define an R function that computes the factorial of given an integer argument. The output should be a vector of length 1.
factorial <- function(x){
  Product <- 1
  Multiplier <- 2
  if (x < 0)
  {print("error")} 
  else if (x <= 1)
  {print(Product)}
  else if(x > 1)
  {while(Multiplier <= x){
    (Product <- Product*Multiplier)
    Multiplier = Multiplier+1}
  print(Product)}
}

#5. Define an R function that accepts a Date (POSIXct) as argument and outputs the day of the week as characters. Use modulo operator.
day.of.week <- function(x){
  y <- as.POSIXlt(x)
  if (y$wday%%7 == 1){print("Monday")} 
  else if(y$wday%%7 == 2){print("Tuesday")}
  else if(y$wday%%7 == 3){print("Wednesday")}
  else if(y$wday%%7 == 4){print("Thursday")}
  else if(y$wday%%7 == 5){print("Friday")}
  else if(y$wday%%7 == 6){print("Saturday")}
  else {print("Sunday")}
}

#6. Create a function to compute for your net pay at work.
netpay.permonth <- function(x){
  #x is monthly salary
  #source for computation figures: http://birtaxcalculator.com/tax_tables.php
  sss <- if(x<15750){x*0.0363} else {581.30}
  pagibig <- if(x>4999){100} else if(x<1500){x*0.01} else {x*0.02}
  philhealth <- if(x<10000){137.50} else if(x>40000){550} else (x*0.0275)/2 
  contributions <- sss + pagibig + philhealth
  aftercontributions <- x - contributions
  if(x <= 20833)
    {print(aftercontributions)}
  else if(x < 33333)
    {withholding.tax <- (aftercontributions-20833)*0.20
     aftertax <- aftercontributions - withholding.tax
      print(aftertax)}
  else if(x < 66667)
    {withholding.tax <- ((aftercontributions-33333)*0.25)+2500
     aftertax <- aftercontributions - withholding.tax
     print(aftertax)}
  else if(x < 166667) #calculation breaks down at this point, results do not match https://taxcalculator.dof.gov.ph/ 
    {withholding.tax <- ((aftercontributions-66667)*0.30)+10833.33
     aftertax <- aftercontributions - withholding.tax
     print(aftertax)}
  else if(x < 666667)
    {withholding.tax <- ((aftercontributions-166667)*0.32)+40833.33
     aftertax <- aftercontributions - withholding.tax
     print(aftertax)}
  else
    {withholding.tax <- ((aftercontributions-666667)*0.35)+200833.33
     aftertax <- aftercontributions - withholding.tax
     print(aftertax)}
}

#8. Create a function that computes the compound interest of an investment given the rate, time, and initial amount or principal.
compound.interest <- function(P, r, n, t) {#Where P is the principal; r is the rate; n is the number of times interest is compounded per year; and t is the term of the loan
    Compounded.rate <- (1 +(r/n))^(n*t)
    P * Compounded.rate}
