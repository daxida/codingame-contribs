# https://perldoc.perl.org/perlrun

# 80

#!ruby -anrprime
x,y=$F.sort.map &:to_i
p x+2<y%4?y.prime?: (x*x+y*y).prime?if y

# 81

#!ruby -anrprime
x,y=$F.map(&:to_i).sort
p x+2<y%4?y.prime?: (x*x+y*y).prime?if y

# 94 @ricemath
# Note that you can sort before casting to int since the first condition 
# is unaffected and the second is only true if x == 0, which is always 
# the first item even when string comparing (since 0 <= x, y < 100)

require'prime'
$<.max{x,y=_1.split.sort.map &:to_i
p (x*x+y*y).prime?||x<1&&y%4>2&&y.prime?;0}

# 96

#!ruby -anrprime
x,y=$F.map &:to_i
p x*y<1?y.prime?&&y%4>2||x.prime?&&x%4>2:(x*x+y*y).prime?if y

# 102

#!ruby -palrprime
x,y=$F.map &:to_i
y||next
$_=x*y<1?y.prime?&&y%4>2||x.prime?&&x%4>2:(x*x+y*y).prime?