# 63 (@Whale)
p$<.map{[_1,_1.reverse][$.%2].strip}.join.scan(/=+/).max.size+2

# 67
#!ruby -nl
$0<<[$_.reverse,$_][$.%2]
END{p$0.scan(/=+/).max.size+2}

# 65
p$<.map{x=_1.chomp
$.%2<1?x:x.reverse}.join.scan(/=+/).max.size+2