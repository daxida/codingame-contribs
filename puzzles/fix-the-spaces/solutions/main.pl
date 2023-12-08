use List::Util qw(all);

my @used;
my $solution;
my $original = <>;
my %tally;
for my $word (split " ", <>) {
    $tally{$word}++;
}
solve($original);
print $solution;

sub solve {
    my $original = "@_";

    if (all { $_ == 0 } values %tally) {
        if ($solution) {
            print "Unsolvable";
            exit;
        }
        $solution = join " ", @used;
    }

    for my $word (keys %tally) {
        next if $original !~ /^$word/ || $tally{$word} < 1;
        $tally{$word}--;
        push @used, $word;
        solve(substr $original, length $word);
        $tally{$word}++;
        pop @used;
    }
}
