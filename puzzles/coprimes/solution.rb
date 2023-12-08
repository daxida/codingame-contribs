# Keywords: Held-Karp, DP with subsets
# https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm
# https://codeforces.com/blog/entry/337
# https://atcoder.jp/contests/abc301/tasks/abc301_e
# https://atcoder.jp/contests/abc274/tasks/abc274_e

n = gets.to_i
xs = gets.split.map(&:to_i).sort

# We add an universal point (UP) at index 0
# The costs are 0 for self-paths and (from/to) the UP
# then it's 1 if we can put the two numbers next to each other
# (they are coprime) or 2 if we can't
n += 1 
adj = Array.new(n) { Array.new(n, 0) }
(0...n-1).to_a.product((0...n-1).to_a).each do |i, j|
  adj[i+1][j+1] = xs[i].gcd(xs[j]) == 1 ? 1 : 2 if i != j
end

# Set transition cost from initial state
dp = (1...n).map { |k| [[1 << k, k], [adj[0][k], 0]] }.to_h

(2...n).each do |subset_size|
  (1...n).to_a.combination(subset_size).each do |subset|
    # Set bits for all nodes in this subset
    bits = 0
    subset.each { |bit| bits |= (1 << bit) }

    # Find the lowest cost to get to this subset
    subset.each do |k|
      prev = bits & ~(1 << k)

      res = []
      subset.each do |m|
        next if m == 0 || m == k

        res << [dp[[prev, m]][0] + adj[m][k], m]
      end
      dp[[bits, k]] = res.min
    end
  end
end

# Calculate optimal cost
bits = (1 << n) - 2
res = (1...n).map { |k| [dp[[bits, k]][0] + adj[k][0], k] }
_, parent = res.min

# Backtrack to find full path
path = []
(n - 1).times do
  path << parent
  new_bits = bits & ~(1 << parent)
  _, parent = dp[[bits, parent]]
  bits = new_bits
end

# Add implicit start state
path << 0

ans = path.map { |i| xs[i - 1] if i != 0 }.compact

# We don't know if the solution is actually valid
# The || 1 is there for the last pair of the zip
if ans.zip(ans[1..]).all? { |x, y| x.gcd(y || 1) == 1 }
  puts ans.join(' ')
else
  puts -1
end