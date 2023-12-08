n = gets.to_i
c = gets.chomp
s = gets.split
s[0] = ""

g = (0...n).map { |i| c*i }
puts g == s.sort ? n : g.select { |x| !s.include?(x) }.join(" ")