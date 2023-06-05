# https://en.wikipedia.org/wiki/Secretary_problem

E = 2.7
INPUT = false

if INPUT
  n = gets.to_i
  scores = gets.split.map(&:to_i)
else
  max_score = 100
  n = 90
  scores = Array.new(n) { 1 + rand(max_score) }
end

warn n
warn scores * " "

def secretary(n, scores)
  cutoff = (n / E).to_i
  best_so_far = (0..cutoff).map{ |x| scores[x] }.max
  warn [cutoff, best_so_far] * " "
  (cutoff...n).find { |i| scores[i] > best_so_far } || -1
end

p secretary(n, scores)