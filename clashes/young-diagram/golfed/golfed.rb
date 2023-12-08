# 62 @whale

eval"*X="+`tr + ,`
$><<(1..X.max).map{|i|X.count{_1>=i}}*' + '

# 96

$/=" + "
x=$<.map &:to_i
$><<x.map{(?1*_1).ljust(x.max,?0).chars}.transpose.map{_1.sum &:hex}*$/

# 96

$/=" + "
x=$<.map &:to_i
((x.map!{_1-1};$*<<x.size.to_s)while x[-1]>0;x.pop)while x[0]
puts$**$/