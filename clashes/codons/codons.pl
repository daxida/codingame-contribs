use strict;
use warnings;
use 5.32.1;

my %codon_map;
while (<DATA>) {
    chomp;
    my ($codon, $amino) = split /\s+/;
    $codon_map{$codon} = $amino =~ s/Stop/x/gr;
}

for my $i (0..<>-1) {
    chomp(my $line = <STDIN>);
    my $max_size = 0;
    my $translated = "";

    for my $offset (0..2) {
        my $cur = substr($line, $offset);

        my $triplets = $cur =~ s/.{3}/$codon_map{$&}/ger;
        my @seqs = $triplets =~ /M.*?x/g;
        my $trans = join "-", @seqs;
        $trans =~ s/x//g;
        $trans =~ s/-$//;
        my $size = length $trans =~ s/\-//gr;

        if ($size > $max_size) {
            $max_size = $size;
            $translated = $trans;
        }
    }
    print $translated, $/;
}

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