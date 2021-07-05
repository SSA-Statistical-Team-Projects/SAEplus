* replicate ordernorm 
cap program drop ordernorm
program define ordernorm 
syntax varlist [iw pw fw/], GENerate(string) 
if "`exp'"=="" {
	tempvar exp
	qui gen `exp'=1
}
sort `varlist'
tempvar rank
gen `rank'=sum(`exp') if `varlist'<. 
qui summ `rank' 
tempname totweight 
scalar `totweight'=r(max)
gen double `generate'=invnormal((`rank'-0.5)/`totweight') if `varlist'<. 
end 
