require 'benchmark'

M = 10**9 + 7

def inside(j, idx)  = j.abs
def outside(j, idx) = [idx, 25 - idx].max - j.abs

def count(radius, center, mode)
  # This represents the switch between closed and open ball
  upper_bound = radius + (mode == :inside ? 1 : 0)

  dp = [0] * (radius + 1)
  dp[0] = 1

  center.each do |idx|
    next_dp = [0] * (radius + 1)

    (-idx..25 - idx).each do |j|
      length = method(mode).call(j, idx)
      (0..radius).each do |k|
        next_dp[k + length] += dp[k] if k + length < upper_bound
      end
    end

    dp = next_dp.map { |r| r % M }
  end

  dp.sum
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
