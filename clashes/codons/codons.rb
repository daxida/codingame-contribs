# https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables

COD = Hash[*DATA.to_a.join.split]
START = "AUG"
ENDS = ["UAA", "UAG", "UGA"]

def solve(s)
  ans = []
  i = 0
  while i < s.size - 2
    while i < s.size - 2 && s[i, 3] != "AUG"
      i += 1
    end

    cur = []
    cur << COD[s[i, 3]]
    while i < s.size - 2
      i += 3
      cur << COD[s[i, 3]]

      if ENDS.include?(s[i, 3])
        i += 3
        break
      end
    end

    if i < s.size - 2 && cur.last != "Stop"
      puts "ERROR: sequence not ending in a stop, #{cur}"
    end
    cur.pop

    ans << cur.join if cur.size > 0
  end

  ans
end

n_tests = gets.to_i
padding = (n_tests - 1).to_s.size
n_tests.times do |i|
  rna = gets.chomp
  ans = solve(rna)

  puts ans.join("-")
end

__END__
UUU F
CUU L
AUU I
GUU V
UUC F
CUC L
AUC I
GUC V
UUA L
CUA L
AUA I
GUA V
UUG L
CUG L
AUG M
GUG V
UCU S
CCU P
ACU T
GCU A
UCC S
CCC P
ACC T
GCC A
UCA S
CCA P
ACA T
GCA A
UCG S
CCG P
ACG T
GCG A
UAU Y
CAU H
AAU N
GAU D
UAC Y
CAC H
AAC N
GAC D
UAA Stop
CAA Q
AAA K
GAA E
UAG Stop
CAG Q
AAG K
GAG E
UGU C
CGU R
AGU S
GGU G
UGC C
CGC R
AGC S
GGC G
UGA Stop
CGA R
AGA R
GGA G
UGG W
CGG R
AGG R
GGG G