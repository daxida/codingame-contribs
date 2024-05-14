LETTERS = "AUGC".chars
START = "AUG"
ENDS = ["UAA", "UAG", "UGA"]

=begin
Doesn't guarantee an ending active sequence at the end of rstring
This is possible: AUG----------UGA---------AUGAAAAA
                   ^            ^           ^
               guaranteed   guaranteed   this may happen randomly
=end

n_tests = 5
puts n_tests

n_tests.times do
  rstring_size = 100
  raise "SizeError" if rstring_size < 6

  rstring = Array.new(rstring_size) { LETTERS.sample }.join

  # Force one start and one end
  idx_start = rand(0..rstring_size - 6)
  rstring[idx_start, 3] = "AUG"

  rend = ENDS.sample
  idx_end = rand(idx_start + 3..rstring_size - 3)
  rstring[idx_end, 3] = rend

  puts rstring
end
