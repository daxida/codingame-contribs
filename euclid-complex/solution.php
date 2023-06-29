<?php

class complex {
    private $re;
    private $im;

    public function __construct(float $re, float $im) {
        $this->re = $re;
        $this->im = $im;
    }

    public function getRe(): float {
        return $this->re;
    }

    public function getIm(): float {
        return $this->im;
    }

    public function isZero(): bool {
        return $this->re == 0 && $this->im == 0;
    }

    public function add(complex $z): complex {
        $re = $this->re + $z->getRe();
        $im = $this->im + $z->getIm();
        return new complex($re, $im);
    }

    public function divide(complex $z): complex {
        $divisor = pow($z->getRe(), 2) + pow($z->getIm(), 2);
        $re = ($this->re * $z->getRe() + $this->im * $z->getIm()) / $divisor;
        $im = ($this->im * $z->getRe() - $this->re * $z->getIm()) / $divisor;
        return new complex($re, $im);
    }

    public function subtract(complex $z): complex {
        $re = $this->re - $z->getRe();
        $im = $this->im - $z->getIm();
        return new complex($re, $im);
    }

    public function multiply(complex $z): complex {
        $re = $this->re * $z->getRe() - $this->im * $z->getIm();
        $im = $this->im * $z->getRe() + $this->re * $z->getIm();
        return new complex($re, $im);
    }

    public function __toString(): string {
        $re = $this->re;
        $im = $this->im;
        if ($re == 0) return sprintf("%.0fj", $im);

        if ($im > 0) {
            $out = sprintf("(%.0f+%.0fj)", $re, $im);
        } elseif ($im < 0) {
            $out = sprintf("(%.0f-%.0fj)", $re, -$im);
        } else {
            $out = sprintf("(%.0f+0j)", $re);
        }
        return $out;
    }
} // End Complex

function closest($n) {
    $c = ceil($n);
    $f = floor($n);
    $d = abs($n - $c);

    return $d <= 0.5 ? $c : $f;
}

function euclid($z1, $z2) {
    $z = $z1->divide($z2);

    $x = $z->getRe();
    $y = $z->getIm();

    $cx = closest($x);
    $cy = closest($y);

    $q = new complex($cx, $cy);
    $r = $z1->subtract($z2->multiply($q));

    return array($q, $r);
}

function gcd($z1, $z2) {
    list($q, $r) = euclid($z1, $z2);
    echo "$z1 = $z2 * $q + $r\n";
    return $r->isZero() ? $z2 : gcd($z2, $r);
}

fscanf(STDIN, "%d %d", $xa, $ya);
fscanf(STDIN, "%d %d", $xb, $yb);

$z1 = new complex($xa, $ya);
$z2 = new complex($xb, $yb);
$g = gcd($z1, $z2);
echo "GCD($z1, $z2) = $g";
?>