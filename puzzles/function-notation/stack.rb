def to_func(ary)
  if ary[0] == '('
    ary.shift
    parens = true
  end

  while ary.size > 1
    # first prio composition .
    if com_idx = ary.index('.')
      x, _, y = ary.slice!(com_idx - 1, 3)
      ary.insert(com_idx - 1, "#{y} |> #{x}")
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

def parse(s)
  stack = [[]]
  s.each_char do |c|
    case c
    when '('
      stack << ['(']
    when ')'
      func = stack.pop
      # warn stack.inspect
      stack[-1] += to_func(func)
    else
      stack[-1] << c
    end
  end

  to_func(stack[0])
end

arrow = gets.chomp
arrow.gsub!(/ /, '')
warn arrow
ans = parse(arrow)
puts ans[0]