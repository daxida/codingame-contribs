D = [[-1, 0], [1, 0], [0, 1], [0, -1]]

n = gets.to_i
g = $<.map { _1.chars.map &:to_i }
vs = []

(0...n).map { |y|
  (0...n).map { |x|
    D.map { |dy, dx|
      vs << (g[y][x].to_s + (n-1).times.map {
        y, x = (y + dy) % n, (x + dx) % n
        g[y][x].to_s
      }.join).to_i
    }
  }
}

p vs.max