# https://www.codingame.com/contribute/view/7443038a912b5b5160da748d4f6917b4452a2

START = "AUG"
ENDS = ["UAA", "UAG", "UGA"]

def solve(s)
  s += "@@" # padding
  ans = ""
  opn = false
  cooldown = 0
  s.chars.each_cons(3) do |pattern|
    pat = pattern.join
    cooldown -= 1 if cooldown > 0
    next if cooldown > 0

    if !opn && pat == START
      opn = true
      ans += pat
      cooldown = 3
    elsif opn && ENDS.include?(pat)
      opn = false
      ans += pat
      cooldown = 3
    elsif opn
      ans += pattern[0]
    else
      ans += "."
    end
  end

  ans
end

# Because the test generator is faulty
def verify_ending(rna_modified)
  rna_modified.split(".").all? do |active|
    # this happens if the string starts/ends with "."
    next true if active.empty?

    active.scan(/AUG.*(UAA|UAG|UGA)/).any?
  end
end

n_tests = gets.to_i
padding = (n_tests - 1).to_s.size
n_tests.times do |i|
  rna = gets.chomp
  ans = solve(rna)

  # DEBUG
  if ans.size != rna.size
    puts "Error at line #{i}:"
    puts "#{" "*padding} #{rna}"
  end
  # puts ["%0#{padding}d" % i, ans] * " "
  unless verify_ending(ans)
    puts "Unending active substring"
    puts rna
  end

  puts ans
end
