# 59 (Whale)
a=`tr -cd A-Z`
p a.gsub!(/../){_1[?Z]||?}.sum/90while a[1]

# 66 (Whale)
m=`tr -cd PZT`
K=1
p m.scan(/.{#{K}}/).count{_1[?Z]}while m[-K*=2]

# 72
m=`tr -cd PZT`
p m.scan(/.{#$K}/).count{_1[/Z/]}until m.size<$K=1<<$.+=1

# 79
m=`tr -cd PZT`.bytes.map{_1%4}
p (m=m.each_slice(2).map{_1|_2}).sum/2while m[1]
