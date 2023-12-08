$filename = "in.txt";
open(STDIN, '<', $filename) or die $!;

$forbidden = '[^-":;, \'?!.\na-zA-Zéàèùçâêîôûëïü]';

sub verify_constraints {
    # Checks for: 
    # (1) Only french characters and -":;, '?!.\n
    # (2) No capital letter with accents
    print "$. $&\n $_" if /$forbidden/;
}

sub fix_characters {
    # For making tests
    %hash = (
        "’" => "'",
        "…" => "...",
        "œ" => "oe",
        "(" => "",
        ")" => "",
        "Ô" => "O",
        "À" => "A",
    );

    s/\Q$k\E/$v/g, while ($k, $v) = each %hash
}

for (0..<STDIN>-1) {
    $_ = <STDIN>;
    fix_characters; print;
    verify_constraints;
    # Thanks to @hydrazer for the [A-ÿ] tip
    s/[A-ÿ]*é([dfrz]s?\b|x)[A-ÿ]*/push @errors, $&/ger
}

$.--;
print STDERR "$.\n";
$, = $/;
print @errors 
