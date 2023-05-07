# 62

o=(gets.to_i**2).digits.join.to_i**0.5
puts o%1>0?:None:o.to_i

# 63

o=(gets.to_i**2).to_s.reverse.to_i**0.5
puts o%1>0?:None:o.to_i
