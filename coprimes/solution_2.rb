INF = 1e9

n = gets.to_i
xs = gets.split.map(&:to_i).sort

n += 1 # Universal point

dist = Array.new(n) { Array.new(n, 0) }
(0...n-1).to_a.product((0...n-1).to_a).each do |i, j|
  next if i == j

  dist[i+1][j+1] = xs[i].gcd(xs[j]) == 1 ? 1 : 2
end

# Initialize dp
dp = Array.new(n) { Array.new(1 << n, INF) }
(0...n).each { |i| dp[i][1 << i] = dist[0][i] }

# Held-Karp main
(0..1 << n).each do |i|
  next if i & 1 == 0

  (0...n).each do |j|
    next if dp[j][i] == INF

    (0...n).each do |k|
      nxt = dp[j][i] + dist[j][k]
      mask = i | (1 << k)
      dp[k][mask] = [dp[k][mask], nxt].min
    end
  end
end

# Backtrack an optimal TSP tour
mask = (1 << n) - 1 # we have visited everything
path_idxs = []
cur = 0

while mask > 1
  nxt = nil
  min_dist = INF

  (1...n).each do |i|
    if mask & (1 << i) != 0 && dp[i][mask] + dist[i][cur] < min_dist
      min_dist = dp[i][mask] + dist[i][cur]
      nxt = i
    end
  end

  path_idxs.unshift nxt
  mask ^= (1 << nxt) # we visit nxt
  cur = nxt
end

path = path_idxs.map { |idx| xs[idx - 1] }

# We need to verify the solution
ok = path.each_cons(2).all? { |x, y| x.gcd(y) == 1 }
puts ok ? path.reverse.join(' ') : -1