# https://perldoc.perl.org/perlrun

# 96

#!ruby -anrprime
x,y=$F.map &:to_i
p x*y<1?y.prime?&&y%4>2||x.prime?&&x%4>2:(x*x+y*y).prime?if y

# 102

#!ruby -palrprime
x,y=$F.map &:to_i
y||next
$_=x*y<1?y.prime?&&y%4>2||x.prime?&&x%4>2:(x*x+y*y).prime?