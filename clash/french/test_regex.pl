use utf8;

# INCONSISTENT: needs utf8

$\ = "\n";

$s = "Ô";
$reg = '[^ô]';
print "with $s";
if ($s !~ /$reg/i) {
    print "m1";
}
$reg = '[ô]';
if ($s =~ /$reg/i) {
    print "m2";
}

$s = "R";
$reg = '[^r]';
print "with $s";
if ($s !~ /$reg/i) {
    print "m1";
}
$reg = '[r]';
if ($s =~ /$reg/i) {
    print "m2";
}

# using utf8
# with �
# m1
# m2
# with R
# m1
# m2

# without
# with Ô
# m2
# with R
# m1
# m2