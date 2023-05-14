use Moose;
use feature 'say';

my @arr = split "", <>;
my %h;
my @ans;

$h{$_} = substr "9487216503", $_, 1 for 0..9;

# also
# my @keys = (0..9);
# my @values = split '', "9487216503";
# @h{@keys} = @values;

for my $k (sort {$h{$a} cmp $h{$b}} @arr) {
    push(@ans, $k);
}

say join "", @ans;