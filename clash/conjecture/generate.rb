def gen_seq(s)
  length = (s.size..).find do |i|
    q, nxt = s.sum.divmod i
    s << nxt
    q == nxt
  end
  # puts s * ' '

  [length, s.last]
end

MAX_N = 10
MAX_X = 100000

n = rand(1...MAX_N)
s = Array.new(n) { rand(1...MAX_X) }
puts n
puts s * ' '
puts gen_seq(s)
