*********************************************
* Econometrics part 2
*********************************************
* Giacomo Mangiante

clear
clear all
set more off
set matsize 800

* open the dataset
use "/Users/giacomomangiante/Desktop/Zurich/Semester II/Econometrics/part 2/PS 1/mroz.dta", clear

*********************************************
* exercise 5.2
*********************************************
global xvar nwifeinc educ exper expersq age

* a)
gen dummy = kidslt6 > 0

probit inlf $xvar
probit dummy $xvar

* b)
biprobit (dummy = $xvar ) (inlf = $xvar )

* c)
biprobit (inlf = dummy $xvar ) (dummy = $xvar ) 

* e) 
local iter=1

gen rho = 1   // to find the lower and upper value it is necessary to start the iteration 
              // from both +1 and -1
local rho = rho
constraint define 1 [athrho]_cons=`rho'
biprobit (dummy = $xvar ) (inlf = dummy $xvar ), constraint(1)
matrix b = e(b)
matrix v = e(V)
gen z = b[1,7]/(v[7,7]^(1/2))


while abs(z)> 1.28 {

replace rho = rho - 0.005   // when you start the iteration from rho = -1 we have to add
                            // instead of subtract 0.005
	local rho = rho
	display "iteration `iter'"
	local iter=`iter' + 1
	
constraint define 1 [athrho]_cons=`rho'
qui biprobit (dummy = nwifeinc educ exper expersq age) (inlf = dummy nwifeinc educ exper expersq age), constraint(1)
matrix b = e(b)
matrix v = e(V)
drop z
gen z = b[1,7]/(v[7,7]^(1/2))
sum z rho
di b[1,7]
}

constraint define 2 [athrho]_cons= -0.5149997 
biprobit (dummy = nwifeinc educ exper expersq age) (inlf = dummy nwifeinc educ exper expersq age), constraint(2)
*

* g)
biprobit (dummy = $xvar ) (inlf = dummy $xvar )
gen constant = 1
mkmat nwifeinc educ exper expersq age constant, matrix(X)
//matrix list x 
matrix b = e(b)
matrix beta = b[1, 1..6] 
matrix gamma = b[1, 8..13] 

matrix A = X*beta'
matrix B = X*gamma'

svmat A, names(A)
svmat B, names(B)

reg A1 B1
		 
local athrho = atanh(_b[B1])
constraint 4 [athrho]_cons = `athrho'
biprobit (dummy = nwifeinc educ exper expersq age) (inlf = dummy nwifeinc educ exper expersq age), constraint(4)


