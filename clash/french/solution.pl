$forbidden = '[^-":;, \'?!.\na-zA-Zéàèùçâêîôûëïü]';

sub verify_constraints {
    # Checks for: 
    # (1) Only french characters and -":;, '?!.\n
    # (2) No capital letter with accents
    print "$. $&\n" if /$forbidden/;
}

for (0..<>-1) {
    $_ = <>;
    verify_constraints;
    # Thanks to @hydrazer for the [A-ÿ] tip
    s/[A-ÿ]*é([dfrz]s?\b|x)[A-ÿ]*/push @errors, $&/ger
}

$, = $/;
print @errors 
