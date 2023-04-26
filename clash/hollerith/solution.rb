def chunkify(s, n)
  s.scan(/.{1,#{n}}/).map { |ch| ch.size.to_s + "H" + ch }
end

n = gets.to_i
s = gets

puts "/" + chunkify(s, n).join(",") + "/"
