def solve original, used
    if @words.values.all? { _1.zero? }
        exit(puts "Unsolvable") if !@solution.empty?
        @solution = used.join(" ")
    end
    @words.each do |word, occurences|
        next if !original.start_with?(word) # || occurences < 1

        @words[word] -= 1
        used << word
        solve(original[word.size..], used)
        @words[word] += 1
        used.pop
    end
end

original = gets.chomp
@words = gets.chomp.split.tally
@solution = ""
solve(original, [])
puts @solution

