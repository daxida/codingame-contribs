# https://www.codingame.com/contribute/view/700438e1d37d9c816b05acdd59782dddbbfef

h = 7
w = 5
charset = "AUCG".chars
rna = Array.new(h) { Array.new(w) { charset.sample }.join }

puts h
puts w
puts rna
puts 

to_delete_idxs_row = []
rna.each_with_index do |row, y|
  idx = row =~ /AUG/ || w
  (0...idx).each do |x|
    to_delete_idxs_row << [y, x]
  end
end

to_delete_idxs_col = []
rna.map(&:chars).transpose.each_with_index do |col, x|
  idx = col.join =~ /AUG/ || h
  (0...idx).each do |y|
    to_delete_idxs_col << [y, x]
  end
end

to_delete_idxs = to_delete_idxs_row & to_delete_idxs_col

rna.each_with_index do |row, y|
  nrow = row.chars.each_with_index.map do |ch, x|
    to_delete_idxs.include?([y, x]) ? "." : ch
  end.join

  puts nrow
end
