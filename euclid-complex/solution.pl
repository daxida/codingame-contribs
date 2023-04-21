use Moose;
use POSIX qw(ceil floor);

package Complex;
use overload
    '-' => "subs",
    '*' => "mul",
    "/" => "div",
    '""' => "stringify";

sub new {
    my $class = shift;
    my $self = { _re => shift, _im => shift };
    bless $self, $class;
    return $self;
}
 
sub re {
    my ($self) = @_;
    return $self->{_re};
}

sub im {
    my ($self) = @_;
    return $self->{_im};
}

sub zero {
    my ($self) = @_;
    return $self->re == 0 && $self->im == 0;
}

sub subs {
    my ($x, $y) = @_;
    return Complex->new(
        $x->re - $y->re, 
        $x->im - $y->im
    )
}

sub mul {
    my ($x, $y) = @_;
    return Complex->new(
        $x->re * $y->re - $x->im * $y->im,
        $x->re * $y->im + $y->re * $x->im,
    );
}

sub div {
    # Based on the OCaml implementation of Complex
    my ($x, $y) = @_;

    if (abs($y->re) >= abs($y->im)) {
        my $r = $y->im / $y->re;
        my $d = $y->re + $r * $y->im;
        return Complex->new(
            ($x->re + $r * $x->im) / $d,
            ($x->im - $r * $x->re) / $d
        );
    } else {
        my $r = $y->re / $y->im;
        my $d = $y->im + $r * $y->re;
        return Complex->new(
            ($r * $x->re + $x->im) / $d,
            ($r * $x->im - $x->re) / $d
        );
    }
}
 
sub stringify {
    # Uses Python conventions for printing complex numbers
    my ($self) = @_;
    my ($re, $im) = ($self->re, $self->im);
    my $out = sprintf("%.0fj", $im || 0);
    if ($re != 0) {
        if ($im > 0) { $out = sprintf("(%.0f+%.0fj)", $re, $im); }
        elsif ($im < 0) { $out = sprintf("(%.0f-%.0fj)", $re, -$im); }
        else { $out = sprintf("(%.0f+0j)", $re); }
    }
    return $out;
}
1;

# End Complex

sub closest {
    my $n = shift;
    # Why is it searching for Complex::ceil ???
    my $c = POSIX::ceil($n); 
    my $f = POSIX::floor($n);
    my $d = abs($n - $c);

    return $d <= 0.5 ? $c : $f;
}

sub euclid {
    my ($z1, $z2) = @_;

    my $z = $z1 / $z2;

    my $x = $z->re;
    my $y = $z->im;
    
    my $cx = closest($x);
    my $cy = closest($y);
    
    my $q = Complex->new($cx, $cy);
    my $r = $z1 - $z2 * $q;

    return $q, $r;
}

sub gcd {
    my ($z1, $z2) = @_;

    my ($q, $r) = euclid($z1, $z2);
    print "$z1 = $z2 * $q + $r\n";

    return $r->zero() ? $z2 : gcd($z2, $r);
}

sub main {
    my ($xa, $ya) = split(/ /, <STDIN>);
    my ($xb, $yb) = split(/ /, <STDIN>);

    my $z1 = Complex->new($xa, $ya);
    my $z2 = Complex->new($xb, $yb);

    my $g = gcd($z1, $z2);
    print "GCD($z1, $z2) = $g"
}

main()
