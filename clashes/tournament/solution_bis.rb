def play(matches, &block)
  res = []
  while matches.size > 1
    matches = matches.each_slice(2).map { |x, y| yield(x, y) }
    res << matches.sum
  end

  res
end

n = gets.to_i
matches = n.times.map { gets[0].tr("ZPT", "100").to_i }
bans = play(matches) { |x, y| x | y }
wans = play(matches) { |x, y| x & y }

puts bans * " "
puts wans * " "
