# 56

puts`tr [:punct:] ' '`.split.select{_1[/é[dfrz]s?$|éx/]}

# 64

puts$<.map{_1.scan(/([A-ÿ]*é([dfrz]s?\b|x)[A-ÿ]*)/).map &:first}