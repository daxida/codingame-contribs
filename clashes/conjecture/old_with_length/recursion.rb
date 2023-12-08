# Checks that the following formula for f does indeed generate the sequences

def f(i, length, start_sum)
  if i == 1
    start_sum
  else
    temp = f(i - 1, length, start_sum)
    temp + temp % (i - 1 + length - 1)
  end
end

def repeat(length, sum, terms)
  seq = []
  # until sum / length == sum % length
  terms.times do
    seq << sum
    sum += sum % length
    length += 1
  end

  seq
end

NTEST = 100
TERMS = 100
MAX_LEN = 100

(1..MAX_LEN).each do |length|
  (1..NTEST).each do |start_sum|
    a = (1..TERMS).map{ |x| f(x, length, start_sum) }*' '
    b = repeat(length, start_sum, TERMS) * ' '
    
    puts "different" if a != b
  end
end

puts "ok"