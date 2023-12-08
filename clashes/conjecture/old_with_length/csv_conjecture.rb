require 'csv'

def repeat(length, sum)
  until sum / length == sum % length
    sum += sum % length
    length += 1
  end

  [length, sum]
end

MAX_SIZE = 250
MAX_LENGTH = 200
CSV.open('repeater_dump.csv', 'w') do |csv|
  csv << ['.', *1..MAX_LENGTH]
  (1..MAX_SIZE).each do |size|
    puts "At size #{size}" if size % 25 == 0
    csv << [size, *(1..MAX_LENGTH).map { |sum| repeat(size, sum)*":" }]
  end
end