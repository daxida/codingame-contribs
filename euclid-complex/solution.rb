class Numeric
  def closest; (self - ceil).abs <= 0.5 ? ceil : floor end
end

class Complex
  def gcd(other)
    z = self / other
    q = Complex(z.real.closest, z.imag.closest)
    r = self - other * q
    puts "#{self} = #{other} * #{q} + #{r}"
    r == 0 ? other : other.gcd(r)
  end

  def to_s
    y, x = imag, real
    return "%.0fj" % (y != 0 ? y : 0) if x == 0

    if y > 0
      "(%.0f+%.0fj)" % [x, y]
    elsif y < 0
      "(%.0f-%.0fj)" % [x, -y]
    else
      "(%.0f+0j)" % x
    end
  end
end

za, zb = $<.map { Complex(*_1.split.map(&:to_i)) }
puts "GCD(#{za}, #{zb}) = #{za.gcd(zb)}"