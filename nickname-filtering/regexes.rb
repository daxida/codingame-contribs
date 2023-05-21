n_bans, n_substitutions, n_nicknames = gets.split.map &:to_i
banned_words = Array.new(n_bans) { gets.chomp }
substitutions = Array.new(n_substitutions) { gets.chomp.tr(" ", "") }
nicknames = Array.new(n_nicknames) { gets.chomp }

banned_regexes = banned_words.map do |banned_word|
  banned_word.chars.map do |char|
    subs = substitutions.find { |chars| chars.include?(char) }
    subs ? "[" + Regexp.escape(subs) + "]" : Regexp.escape(char)
  end.join
end

p nicknames.count { |nickname|
  banned_regexes.any? { |banned_regex| nickname =~ Regexp.new(banned_regex) }
}