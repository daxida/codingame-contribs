use Moose;

my $n = <>;
my $pair = sqrt reverse $n ** 2;
my $floor = int $pair;

print $floor == $pair ? $pair : "None"
