# Applies two times the conversion to obtain the original testcase

def to_func(ary, fr_symb, to_symb)
  if ary[0] == '('
    ary.shift
    parens = true
  end

  while ary.size > 1
    # first prio composition .
    if com_idx = ary.index(fr_symb)
      x, *_, y = ary.slice!(com_idx - 1, 3)
      ary.insert(com_idx - 1, "#{y} #{to_symb} #{x}")
    elsif sum_idx = ary.index('+')
      x, _, y = ary.slice!(sum_idx - 1, 3)
      ary.insert(sum_idx - 1, "#{x} + #{y}")
    else
      puts "Error " + ary.inspect
      exit
    end
  end

  ary[0] = '(' + ary[0] + ')' if parens

  ary
end

def parse(s, fr_symb, to_symb)
  s = s.gsub(/ /, '')

  stack = [[]]
  s.each_char do |c|
    case c
    when '('
      stack << ['(']
    when ')'
      func = stack.pop
      # warn stack.inspect
      stack[-1] += to_func(func, fr_symb, to_symb)
    else
      stack[-1] << c
    end
  end

  to_func(stack[0], fr_symb, to_symb)[0]
end

arrow = gets.chomp
ans = parse(arrow, '.', '$')
puts ans.gsub(/\$/, "|>")

sym = parse(ans, '$', '.')
if arrow != sym
  puts "Expected " + arrow.inspect
  puts "But got  " + sym.inspect
end