# Based on a solution by @[4G]

# We move from left to right and we swap elements from base
# to reach target up to index i.
# If we could do so, we can ignore the previous (to i) elements
# since the strings are equal up to i. If we couldn't, we already
# printed false and exited.
# That way, we can search for target for our swaps in base[i..]

class Array
  def any?(other) = (self & other).size > 0
  def all?(other) = (self - other).size == 0
end

def process(base, target, fixed)
  return false if base.size != target.size || !base.all?(target)

  target.each_with_index { |c, i|
    next if c == base[i]

    j = i + base[i..].index(c)
    return false if base[i..j].any?(fixed)

    base[i], base[j] = base[j], base[i]
  }
  true
end

def main
  gets.to_i.times.each do |i|
    base, target, fixed = gets.split.map &:chars
    puts process(base, target, fixed)
  end
end

main