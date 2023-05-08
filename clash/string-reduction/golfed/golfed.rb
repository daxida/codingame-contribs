# 58

a,b=`dd`.split
a.chars.map{$><<(_1==b[$.]&&($.+=1)?_1:?-)}
