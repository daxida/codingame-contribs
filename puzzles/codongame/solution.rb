# https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables

MAX_N = 20
MIN_LENGTH_RNA = 5
MAX_LENGTH_RNA = 1 << 11 # 2048

COD = Hash[*DATA.to_a.join.split]
START = "AUG"
ENDS = ["UAA", "UAG", "UGA"]

def translate_given_index(idx, s)
  state = :CLOSED
  sequences = []
  sequence = ""

  s[idx..].chars.each_slice(3) do |codon|
    codon = codon.join
    next if codon.size < 3

    if codon == START && state == :CLOSED
      state = :OPENED
      sequence = ""
    end
    if ENDS.include?(codon) && state == :OPENED
      state = :CLOSED
      sequences << sequence
    end

    sequence << COD[codon] if state == :OPENED
  end

  sequences
end

def solve(s)
  translations = [0, 1, 2].map { |idx| translate_given_index(idx, s) }

  translations.sort_by! { |seqs| seqs.join.size }
  other, second_best, ans = translations

  debug_sizes = true
  if debug_sizes
    translations.each do |t|
      warn "%02d %02d %s" % [t.join.size, t.join("-").size, t.join("-")]
    end
    warn "--"

    # We want the length of the "-" joined strings to be ambiguous
    puts "Error" unless translations.map { _1.join("-").size }.uniq.size == 2
  end

  puts "Error: multiple solutions" if second_best.join.size == ans.join.size

  ans * "-"
end

n = gets.to_i
puts "Error: constraints violation max_n" if n >= MAX_N

n.times do |i|
  rna = gets.chomp
  if rna.size >= MAX_LENGTH_RNA || rna.size <= MIN_LENGTH_RNA
    puts "Error: constraints violation rna_size=#{rna.size}"
  end

  puts solve(rna)
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
