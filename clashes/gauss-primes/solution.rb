def prime? n
  n >= 2 && (2..(n ** 0.5).floor).all? { |i| n % i > 0 }
end

def gauss? x, y
  if x == 0
    y % 4 == 3 && prime?(y)
  elsif y == 0
    x % 4 == 3 && prime?(x)
  else
    prime?(x ** 2 + y ** 2)
  end
end

n = gets.to_i
n.times do
  x, y = gets.split.map &:to_i
  # Constaints check
  puts "Error" unless 0 <= x && x < 100 && 0 <= y && y < 100
  puts gauss?(x, y)
end
