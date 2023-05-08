#!perl -l

# Generator

$s .= chr(32 + rand(90) | 0) for (1..100);
print $s;
$_ = $s;

# solution

$sum = unpack "%C*";
$sum % ord || print > exit for/./g;
print "prime"
