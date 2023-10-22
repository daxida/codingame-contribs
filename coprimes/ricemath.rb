# DFS-like solution by @Ricemath

require 'prime'

n = gets.to_i
xs = gets.split.map(&:to_i).sort

DIVISORS = xs.uniq.map do |x|
  [x, x.prime_division.map(&:first) - [1]]
end.to_h

COPRIMES = xs.uniq.map do |x|
  [x, xs.select { |y| (DIVISORS[x] & DIVISORS[y]).empty? }]
end.to_h

RES_CACHE = {}

def r_search(from, xs)
  return [from] if xs.empty?

  coprime_xs = xs & COPRIMES[from]
  return [-1] if coprime_xs.empty?

  result = coprime_xs.filter_map do |coprime_x|
    coprime_i = xs.index(coprime_x)

    cache_key = [from, coprime_x, xs].hash
    RES_CACHE[cache_key] ||= r_search(coprime_x, xs[0...coprime_i]+xs[coprime_i+1..])

    break RES_CACHE[cache_key] unless RES_CACHE[cache_key][0] == -1
  end

  result.empty? ? [-1] : [from, *result]
end

candidate = nil

xs.each do |x|
  x_i = xs.index x

  candidate = r_search(x, xs[0...x_i] + xs[x_i+1..])
  break unless candidate == [-1]
end

puts candidate * " "