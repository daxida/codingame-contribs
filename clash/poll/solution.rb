INPUT = true

Entry = Struct.new(:vote, :time, :id) do
  def to_s
    "#{vote} #{time} #{id}"
  end
end

if !INPUT
  TIMEOUT = 10
  AMT_VOTERS = 50
  N_ENTRIES = 100

  entries = N_ENTRIES.times.map do
    vote = rand(2)
    time = rand(1..TIMEOUT * 4) * TIMEOUT / 4
    id = rand(AMT_VOTERS)
    Entry.new(vote, time, id)
  end.sort_by { [_1.time, _1.id] }

  # Reorders the ids by first appearance
  reindexing = Hash.new
  cnt = -1
  entries.each do |e|
    reindexing[e.id] = (cnt += 1) if !reindexing[e.id]
    e.id = reindexing[e.id]
  end

  puts TIMEOUT
  puts entries.size # Paranoia check, should be N_ENTRIES
  puts entries
else
  TIMEOUT = gets.to_i
  puts "Error" unless 0 < TIMEOUT && TIMEOUT <= 100
  gets
  entries = $<.map { 
    vote, time, id = _1.split.map &:to_i
    puts "Error" unless 0 <= vote && vote <= 1 && 0 <= time && time <= 200
    Entry.new(vote, time, id)
  }
end

rec = Hash.new
ans = [0, 0]
entries.each do |e|
  next if x = rec[e.id] and e.time - x < TIMEOUT

  ans[e.vote] += 1
  rec[e.id] = e.time
end

puts ans * " "
