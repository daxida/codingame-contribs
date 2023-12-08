radius = gets.to_i
center = gets.chomp.chars.map { _1.ord - 97 }

cur = [[radius, 1]].to_h
center.each do |c1|
  nxt = Hash.new(0)
  cur.each do |key, occ|
    26.times do |c2|
      t = key - (c1 - c2).abs
      nxt[t] += occ if t >= 0
    end
  end
  cur = nxt
end

p cur.values.sum
