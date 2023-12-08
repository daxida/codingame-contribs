radius = gets.to_i
center = gets.chomp.chars.map { |c| c.ord - 97 }

cur = { radius => 1 }
center.each do |c1|
  cur = cur.flat_map do |key, occ|
    (0..25).map { |c2| (t = key - (c1 - c2).abs) >= 0 ? [t, occ] : nil }
  end.compact.group_by(&:first).transform_values { |ps| ps.sum(&:last) }
end

p cur.sum(&:last)
