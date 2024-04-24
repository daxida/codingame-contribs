LOCAL = true
DEBUG = false
TITLES = DATA.to_a.map(&:strip)

def generate(n)
  upto_id = n
  chapters_ids = (1..upto_id).to_a.sample(n)
  raise "SizeError" if TITLES.size < [n, upto_id].min

  # chapters_ids = [5, 6, 2, 1, 3]
  # n = chapters_ids.size
  chapters = chapters_ids.map do |idx|
    "#{idx}. #{TITLES[idx]}"
  end
  
  [n, chapters]
end

def get_lis(ary)
  n = ary.size
  dp = Array.new(n, 1)
  prev = Array.new(n, -1)
  (0...n).each do |i|
    (0...i).each do |j|
      if (ary[j] < ary[i] && dp[i] < dp[j] + 1) # <= doesn't work!
        dp[i] = dp[j] + 1
        prev[i] = j
      end
    end
  end

  max_sz = dp.max
  solutions = (0...dp.size).select { |idx| dp[idx] == max_sz }
  # warn "NonUniqueError" if solutions.size > 1
  # warn solutions.inspect if solutions.size > 1

  # Paranoia check:
  # We try every possible LIS even though the first index is always the solution.
  candidates = []
  solutions.each do |pos|
    ans = []
    until pos == -1
      ans << ary[pos]
      pos = prev[pos]
    end
    candidates << ans.reverse
  end
  warn "Candidates size = #{candidates.size}"

  candidates
end

def patch_order(chapters)
  get_lis(chapters).map do |lis|
    to_patch = chapters - lis
    to_patch.sort
  end
end

def get_requests(mapping, order)
  chapters_ids = mapping.keys
  fix_members = [0] + (chapters_ids - order)

  if DEBUG
    warn "to reorder > " + order * " "
    warn "fix_members  > " + fix_members * " "
    warn "chapters_ids > " + chapters_ids * " "
    warn "\n\n"
  end

  requests = []
  order.each_with_index do |chapter_id, idx|
    first_bigger_fix_member_idx = (0...fix_members.size).find { |nidx| fix_members[nidx] > chapter_id }
    first_bigger_fix_member_idx ||= fix_members.size
    first_bigger_fix_member_idx -= 1

    should_go_after_this = fix_members[first_bigger_fix_member_idx]
    fix_members.insert(first_bigger_fix_member_idx + 1, chapter_id)
    should_go = (chapters_ids.index(should_go_after_this) || -1) + 1

    # And simulate to double check
    chapter_idx = chapters_ids.index(chapter_id)
    chapters_ids.delete_at(chapter_idx)
    offset = should_go > chapter_idx ? 1 : 0
    chapters_ids.insert(should_go - offset, chapter_id)
    requests << [mapping[chapter_id], should_go - offset + 1] * " "

    if DEBUG
      warn "#{chapter_id} should_go_after_this:        #{should_go_after_this}"
      warn "#{chapter_id} should go at index:          #{should_go}"
      warn "fix_members  > " + fix_members * " "
      warn "chapters_ids > " + chapters_ids * " "
      warn "\n\n"
    end
  end

  raise "NotSortedError" unless chapters_ids == chapters_ids.sort

  requests
end

def main(n=nil)
  if LOCAL
    sz, chapters = generate(n)
    warn sz, chapters
    warn "------------"
  else
    sz = gets.to_i
    chapters = sz.times.map { gets.chomp }
  end

  mapping = chapters.map do |chapter|
    [chapter.match(/\d+/)[0].to_i, chapter]
  end.to_h
  chapters_ids = mapping.keys

  possible_requests = []
  smaller_number_requests = nil
  possible_requests = patch_order(chapters_ids).map do |order|
    smaller_number_requests = order.size # always the same
    get_requests(mapping, order)
  end

  smaller_request = possible_requests.min_by do |requests|
    requests.map { |request| request.match(/\d+/)[0].to_i }
  end
  raise StandardError unless smaller_request == possible_requests[0]

  puts smaller_number_requests, smaller_request
end

if LOCAL
  1.times { main(20) }
else
  main
end

__END__
The Beginning
Into the Unknown
Trials and Tribulations
A New Journey
Unexpected Encounters
Crossing Paths
The Call to Adventure
Shadows of the Past
Allies and Adversaries
Lost in Time
In Pursuit of Truth
Echoes of Destiny
The Road Ahead
Dark Horizons
Secrets Unveiled
The Forbidden Realm
Wandering Souls
Bound by Fate
Whispers of Hope
Fractured Memories
Quest for Redemption
The Edge of Oblivion
Veil of Illusions
Tales of Valor
Echoes from the Abyss
Shattered Dreams
In the Shadow of Giants
Beyond the Veil
Echoes of Eternity
Dance of Shadows
Bloodlines
Chasing Legends
Chronicles of Chaos
The Last Stand
Beyond the Unknown
Legends Reborn
The Final Confrontation
Rise of the Fallen
Destiny's Crossroads
The Lost Chronicles
Bonds of Brotherhood
Echoes of the Past
Darkness Rising
A Hero's Journey
Whispers in the Wind
Legacy of the Ancients
The Eternal Flame
Trials of the Heart
Echoes of War
The Journey's End
The Hidden Chamber
Sands of Time
Masks and Mirrors
Flames of Passion
The Enchanted Forest
Dreams of Tomorrow
The Crimson Blade
Echoes of the Mind
Ghosts of the Past
The Crystal Key
Stormy Seas
Echoes of Silence
Secrets of the Stars
Echoes of Destiny
Shadows in the Night
Voices from Beyond
The Emerald Isle
Echoes of the Soul
The Silver Lining
Echoes of Hope
The Crystal Palace
The Ivory Tower
Echoes of Madness
Echoes of Love
The Midnight Hour
Echoes of Freedom
Echoes of Despair
The Golden City
Echoes of Justice
Echoes of Power
The Lost World
Echoes of Wisdom
Echoes of Betrayal
The Dark Tower
Echoes of Mystery
Echoes of Change
Echoes of Courage
Echoes of Triumph
Echoes of Serenity
Echoes of Magic
Echoes of Destruction
Echoes of Peace
Echoes of Revenge
The Secret Garden
Echoes of Desire
Echoes of Faith
Echoes of Destiny
Echoes of Legends
Echoes of Honor
Echoes of Sacrifice
Echoes of Victory
Echoes of Chaos
Echoes of Discovery
Echoes of Adventure
Echoes of Sorrow
Echoes of Wonder
Echoes of Regret
Echoes of Redemption
Echoes of Salvation
Echoes of Forgiveness
Echoes of Miracles
Echoes of Revelation
Echoes of Enlightenment
Echoes of Purity
Echoes of Darkness
Echoes of Light
Echoes of Shadows
Echoes of Truth
Echoes of Illusion
Echoes of Reality
Echoes of Fantasy
Echoes of Dreams
Echoes of Nightmares
Echoes of Hopelessness
Echoes of Determination
Echoes of Resilience
Echoes of Endurance
Echoes of Strength
Echoes of Weakness
Echoes of Vulnerability
Echoes of Courage
Echoes of Fear
Echoes of Dread
Echoes of Anticipation
Echoes of Anxiety
Echoes of Excitement
Echoes of Calm
Echoes of Chaos
Echoes of Order
Echoes of Harmony
Echoes of Discord
Echoes of Unity
Echoes of Division
Echoes of Union
Echoes of Separation
Echoes of Connection
Echoes of Disconnection
Echoes of Bonding
Echoes of Breaking
Echoes of Healing
Echoes of Harm
Echoes of Growth
Echoes of Decay
Echoes of Renewal
Echoes of Decline
Echoes of Beginning
Echoes of Ending
Echoes of Continuation
Echoes of Conclusion
Echoes of Origin
Echoes of Destination
Echoes of Arrival
Echoes of Departure
Echoes of Arrival
Echoes of Departure
Echoes of Existence
Echoes of Nonexistence
Echoes of Creation
Echoes of Destruction
Echoes of Preservation
Echoes of Obliteration
Echoes of Conservation
Echoes of Consumption
Echoes of Inception
Echoes of Conclusion
Echoes of Inquiry
Echoes of Answer
Echoes of Question
Echoes of Solution
Echoes of Problem
Echoes of Analysis
Echoes of Synthesis
Echoes of Thesis
Echoes of Antithesis
Echoes of Perspective
Echoes of Perception
Echoes of Reality
Echoes of Fantasy
Echoes of Imagination
Echoes of Dream
Echoes of Nightmare
Echoes of Consciousness
Echoes of Unconsciousness
Echoes of Mind
Echoes of Matter
Echoes of Energy
Echoes of Spirit
Echoes of Form
Echoes of Essence
Echoes of Being