MAX_STATEMENT = 2000

def brute_force(s)
  seq = s.map(&:dup)
  10000.times { seq << seq.sum % seq.size }

  length = (0...seq.size - 1).find do |i|
    seq[i, MAX_STATEMENT].uniq.size == 1
  end

  seq[length]
end

n = gets.to_i
s = gets.split.map(&:to_i)
if s.size != n
  puts "Size error: #{s.size} != #{n}"
end

puts brute_force(s)
