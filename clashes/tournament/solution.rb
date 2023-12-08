n = gets.to_i
matches = n.times.map { gets[0].tr("ZPT", "100").to_i }
best  = matches
worst = matches
bans = []
wans = []
while best.size > 1
  best  = best.each_slice(2).map  { |x, y| x | y }
  worst = worst.each_slice(2).map { |x, y| x & y }
  bans << best.sum
  wans << worst.sum
end

puts bans * " "
puts wans * " "
