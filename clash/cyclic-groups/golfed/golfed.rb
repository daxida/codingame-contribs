# 67 by @Whale

n,c,*s=`dd`.split
r=[?0,*(1...n.hex).map{c*_1}]-s
puts r[0]?r*' ':n

# 76 

n,c,*s=`dd`.split
s[0]=""
puts s.sort==(g=(0...n.hex).map{c*_1})?n:(g-s)*" "