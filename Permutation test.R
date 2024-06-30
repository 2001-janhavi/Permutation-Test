# Permutation test
n=7
m=9
x=c(94,197,16,38,99,141,23)
y=c(52,104,146,10,51,30,40,27,46)

Diff_obs=mean(x)-mean(y)

z=c(x,y)

diff_p=0
M=10000
for(i in 1:M)
{
  ind=sample(1:(n+m),n,replace = F)
  xp=z[ind]
  yp=z[-ind]
  diff_p[i]=mean(xp)-mean(yp)
}
diff_p
P_value=sum(diff_p>Diff_obs)/M
P_value
