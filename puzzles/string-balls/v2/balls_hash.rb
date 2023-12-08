require 'benchmark'

M = 10**9 + 7

def inside(j, idx)  = j.abs
def outside(j, idx) = [idx, 25 - idx].max - j.abs

def count(radius, center, mode)
  # This represents the switch between closed and open ball
  upper_bound = radius + (mode == :inside ? 1 : 0)

  dp = { 0 => 1 }

  center.each do |idx|
    dp = (-idx..25 - idx).each.with_object(Hash.new(0)) do |j, nxt|
      length = method(mode).call(j, idx)
      dp.each do |k, occ|
        nxt[k + length] += occ % M if k + length < upper_bound
      end
    end
  end

  dp.values.sum
end

def main
  radius = gets.to_i
  center = gets.chomp.chars.map { _1.ord - 'a'.ord }

  max_radius = center.sum { |idx| [idx, 25 - idx].max }
  max_points = 26.pow(center.size, M)

  if radius < max_radius / 2
    count(radius, center, :inside)
  elsif radius < max_radius
    max_points - count(max_radius - radius, center, :outside)
  else
    max_points
  end
end

warn Benchmark.realtime { puts main % M }
