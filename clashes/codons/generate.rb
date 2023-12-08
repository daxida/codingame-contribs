# https://en.wikipedia.org/wiki/DNA_and_RNA_codon_tables

NUCL = "AUGC".split
COD = Hash[*DATA.to_a.join.split]
INV_COD = COD.invert # be clear on the mapping
# puts INV_COD.keys.join
# FLIVMSPTAYHNDStopQKECRGW
START = "AUG"
ENDS = ["UAA", "UAG", "UGA"]
tmp = COD.dup
tmp.delete(START)
COD_INNER = tmp

def add_noise
  # Add some random sequence of codons (without START)
  # plus some nucleotides to affect the length

  100.times do
    noise = ""
    3.times.map do
      noise += COD_INNER.keys.sample
      noise += NUCL.sample if rand(0..100) < 10
    end

    return noise if !noise[/AUG/]
  end

  "ERROR"
end

def generate(s)
  seq = []
  s.upcase.split("-").each do |section|
    section.each_char do |ch|
      if !INV_COD.keys.include? ch
        raise StandardError, "Couldn't find the aminoacid #{ch}"
      end 
    end

    seq << add_noise
    seq << to_nucleotides(section)
    seq << add_noise
  end

  seq.join
end

def to_nucleotides(s)
  nucleotides = []
  nucleotides << START

  s.each_char do |ch|
    nucleotides << INV_COD[ch]
  end
  
  nucleotides << ENDS.sample

  nucleotides.join
end

testcase = generate("cdingame")
puts 1
puts testcase

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