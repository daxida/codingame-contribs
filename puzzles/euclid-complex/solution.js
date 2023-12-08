// Adapted from https://github.com/infusion/Complex.js/blob/master/complex.js

function Complex(a, b) {
  this.re = a;
  this.im = b;
}

Complex.prototype = {
  constructor: Complex,

  add: function(z) {
    return new Complex(
      this.re + z.re,
      this.im + z.im
    );
  },

  sub: function(z) {
    return new Complex(
      this.re - z.re,
      this.im - z.im
    );
  },

  mul: function(z) {
    return new Complex(
      this.re * z.re - this.im * z.im,
      this.re * z.im + this.im * z.re
    );
  },

  div: function(z) {
    const a = this.re;
    const b = this.im;
    const c = z.re;
    const d = z.im;
    var t, x;

    if (Math.abs(c) < Math.abs(d)) {
      x = c / d;
      t = c * x + d;

      return new Complex(
        (a * x + b) / t,
        (b * x - a) / t
      );
    } else {
      x = d / c;
      t = d * x + c;

      return new Complex(
        (a + b * x) / t,
        (b - a * x) / t
      );
    }
  },

  'toString': function() {
    const y = this.im;
    const x = this.re;
  
    if (x === 0) {
      return `${(y !== 0) ? `${y}j` : '0j'}`;
    } else {
      if (y > 0) return `(${x}+${y}j)`;
      else if (y < 0) return `(${x}-${-y}j)`;
      else return `(${x}+0j)`;
    }
  }
};


function closest(n) {
  const c = Math.ceil(n);
  return Math.abs(n - c) <= 1 / 2 ? c : Math.floor(n);
}

function euclid(z1, z2) {
  z = z1.div(z2)
  const q = new Complex(closest(z.re), closest(z.im));
  const r = z1.sub(z2.mul(q));
  return [q, r];
}

function gcd(z1, z2) {
  const [q, r] = euclid(z1, z2);
  console.log(`${z1} = ${z2} * ${q} + ${r}`);
  return r.re === 0 && r.im === 0 ? z2 : gcd(z2, r);
}

const [xa, ya] = readline().split(' ').map(Number);
const [xb, yb] = readline().split(' ').map(Number);
const z1 = new Complex(xa, ya);
const z2 = new Complex(xb, yb);
const g = gcd(z1, z2);
console.log(`GCD(${z1}, ${z2}) = ${g}`);
