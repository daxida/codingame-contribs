# @Whale 82

#!ruby -an
*a,f=$F
b,t=a.map{|x|x.split(/([#{f}])/).map{_1.chars.sort}}
b&&p(b==t)

#!ruby -an
b,t,*f=$F
p b.scan(r=/#{f}/)==t.scan(r)&&b.split(r).zip(t.split r).all?{_1&.chars&.sort==_2&.chars&.sort}if t
