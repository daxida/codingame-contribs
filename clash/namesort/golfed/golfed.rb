# 47

print *gets.chars.sort_by{"
487216503"[_1.hex]}

$><<gets.chars.sort_by{"9487216503"[_1.hex]}*''

$><<gets.chars.sort_by{" 4872165 3"[_1.hex]}*''

# The idea is that A = 0123456789 is mapped to B = 8549176320
# and the inverse (that is, the list of indexes in B of elements in A)
# is 9487216503 (9 is the index of 0 in B, 4 the index of 1 in B etc.). 
# Any of the following "9487216503", " 487216503" will work, since 
# there are no zeroes (and funilly enough, also "94872165 3" or
# " 4872165 3", since the empty space has lower ascii value 
# than any number).

# 47

gets
"854917632".chars.map{$><<_1*$_.count(_1)}

# 49

puts gets.chars.sort_by{"9487216503"[_1.to_i]}*''

# 50

puts gets.to_i.digits.sort_by{"94872165 3"[_1]}*''
