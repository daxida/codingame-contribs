partition = gets.chomp.split(" + ").map &:to_i
m = partition.max

young = partition.map{ |s| ("1" * s).ljust(m, "0").chars }
conj = young.transpose.map{ |x| x.sum{_1.to_i} }

puts conj * " + "