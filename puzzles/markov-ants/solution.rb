'''
https://en.wikipedia.org/wiki/Absorbing_Markov_chain
https://github.com/daxida/CodinGame-contributions/blob/main/markov-ants/Tran_presentation.pdf
(FRENCH) https://idpoisson.fr/berglund/probamass_html/node18.html
'''

require 'matrix'

DIRS = [[-1, 0], [0, 1], [0, -1], [1, 0]]

def solve(start, step, w, h)
  states = generate_states(step, w, h)
  matrix_size = states.size
  matrix = transition_markov(states, matrix_size)

  row_start_point = states.keys.index(start)
  inv = (Matrix.identity(matrix_size) - Matrix[*matrix]).inverse

  expectancy = inv.row(row_start_point).sum
end

def generate_states(step, w, h)
  states = {}
  (1...h).each do |y|
    (1...w).each do |x|
      states[[x, y]] = DIRS.filter_map do |dy, dx|
        nx = x + dx * step
        ny = y + dy * step
        [nx, ny] if nx.abs < w - 1 && ny.abs < h - 1
      end
    end
  end

  states
end

def transition_markov(states, matrix_size)
  matrix = Array.new(matrix_size) { Array.new(matrix_size, 0) }
  states.keys.each_with_index do |pt1, idx1|
    states.keys.each_with_index do |pt2, idx2|
      matrix[idx1][idx2] = 1.0 / 4 if states[pt1].include?(pt2)
    end
  end

  matrix
end

def main
  step = gets.to_i
  w, h = gets.split.map(&:to_i)
  g = h.times.map { gets.chomp }
  start = (0...w).to_a.product((0...h).to_a).find { |x, y| g[y][x] == 'A' }

  expectancy = solve(start, step, w, h)
  puts expectancy.round(1)
end

main
