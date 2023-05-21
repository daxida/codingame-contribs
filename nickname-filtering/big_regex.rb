require 'benchmark'

def main
  n_bans, n_substitutions, n_nicknames = gets.split.map &:to_i
  banned_words = Array.new(n_bans) { gets.chomp }
  substitutions = Array.new(n_substitutions) { gets.chomp.tr(" ", "") }
  nicknames = Array.new(n_nicknames) { gets.chomp }

  banned_regex = Regexp.new(banned_words.map do |banned_word|
    "(" + banned_word.chars.map do |char|
      "[" + Regexp.escape(substitutions.find { |chars|
        chars.include? char
      } || char) + "]"
    end.join + ")"
  end.join("|"))

  warn banned_regex.to_s.size

  p nicknames.filter { |nickname| nickname =~ banned_regex }.size
end

time = Benchmark.measure { main }
warn "Time %.4f" % time.real
