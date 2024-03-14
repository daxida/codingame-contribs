n = gets.to_i
matches = n.times.map { gets[0].tr("ZPT", "100").to_i }

ans = []
while matches.size > 1
  matches = matches.each_slice(2).map { |x, y| x | y }
  ans << matches.sum
end

puts ans
