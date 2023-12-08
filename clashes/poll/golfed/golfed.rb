# 96 @Whale

h={}
a=0,0
$<.map{eval"I,C,T="+_1.split*?,
V||=I
V+(h[I]||-V)>T||(h[I]=T;a[C]+=1)if C}
$><<a*' '

# 102

(T,),_,*g=$<.map{_1.split.map &:to_i}
h=[-T]*99
a=0,0
g.map{h[_1]+T>_3||(h[_1]=_3;a[_2]+=1)}
$><<a*' '

# 103

T=gets.to_i
h={}
a=0,0
$<.map{i,c,t=_1.split.map &:to_i
t&&t>=T+(h[i]||-T)&&(h[i]=t;a[c]+=1)}
$><<a*' '

# 104

gets
h={}
a=0,0
$<.map{i,c,t=_1.split.map &:to_i
(h[i]&&t-h[i]<$_.to_i)||(h[i]=t;a[c]+=1)if t}
$><<a*' '

# 105

T,_,*g=*$<
h={}
a=0,0
g.map{i,c,t=_1.split.map &:to_i
(h[i]&&t-h[i]<T.to_i)||(h[i]=t;a[c]+=1)}
$><<a*' '
