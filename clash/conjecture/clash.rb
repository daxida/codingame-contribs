# Take a sequence s of n numbers and repeatedly push to the back 
# its current sum, modulo the current length of the sequence.

# Conjecture: the sequence is stationary.

# CF.
# https://oeis.org/A066910
# https://oeis.org/A073117

def naive(s)
  # Expected solution:
  # It can fail, only checks for 100 consecutive equals

  seq = s.map(&:dup)
  10000.times { seq << seq.sum % seq.size }

  length = (0...seq.size - 1).find do |i|
    seq[i, 100].uniq.size == 1
  end

  if length
    [length, seq[length]]
  else
    [-1, -1]
  end
end

def repeat(s)
  # Infallible solution
  length, sum = s.size, s.sum

  until sum / length == sum % length
    sum += sum % length
    length += 1
  end

  [length, sum % length]
end

n = gets.to_i
s = gets.split.map(&:to_i)
if s.size != n
  puts "Size error: #{s.size} != #{n}"
end

fst = naive(s)
snd = repeat(s)
if fst != snd
  puts "Conflicting solutions: #{fst} != #{snd}"
end

puts snd
