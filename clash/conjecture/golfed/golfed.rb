# 56 @Whale

_,*S=`dd`.split
2000.times{S<<eval(S*?+)%S.size}
p S[-1]

# 59

n,*S=`dd`.split.map &:to_i
n.upto(1e4){S<<S.sum%_1}
p S[-1]

# 66

l,*x=`dd`.split.map &:to_i
s=x.sum
(s+=X;l+=1)until s/l==X=s%l
p X

# 71

_,*x=`dd`.split
s=x.sum &:to_i
l=x.size
(s+=X;l+=1)until s/l==X=s%l
p X