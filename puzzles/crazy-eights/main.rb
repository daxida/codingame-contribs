require "benchmark"

STDOUT.sync = true

RANKS = "A 2 3 4 5 6 7 8 9 T J Q K".split
SUITS = "C D H S".split
CARDS = RANKS.product(SUITS).map { |r, s| [r, s].join }

INPUT = false  # false for random
LOCAL = true # For switching CG / LOCAL
TESTC = true # true for test creation
N     = 7

# TODO

# 1. ORDER can be simplified
# [[[6H, 8S, 5S], [8D], 0], [[6H, 8S, 5S], [8S], 0],
# [[6H, 5S, 8D], [8S], 0], [[6H, 5S, 8H], [8S], 0], [[6H, 8S, 5S], [8H], 0]]

# 2. Can simplify multiple 8s (like in simulate)

# 3. Can simplify 8s suits (done, but correct?)

def init
  if !INPUT
    c, p1, p2 = generate(CARDS.dup)
    c  = Card.new(c)
    p1 = p1.map { |s| Card.new(s) }
    p2 = p2.map { |s| Card.new(s) }
  else
    $stdin = File.open("in.txt", "r") if LOCAL
    c, p1, p2 = $<.map { |l| l.chomp.split.map { |c| Card.new(c) } }
    c = c[0]
  end
  [c, p1, p2]
end

class Array
  def is_prefix(other)
    sz = self.size
    return false if sz > other.size

    (0...sz).all? { |i| self[i] == other[i] }
  end
end

class Card
  attr_accessor :rank, :suit

  def initialize(s)
    @rank = RANKS.index(s[0])
    @suit = SUITS.index(s[1])
  end

  def matches(other)
    # Careful, it's not symmetric (7 because 0-idx)
    @rank == 7 || @rank == other.rank || @suit == other.suit
  end

  def to_s
    "#{RANKS[@rank]}#{SUITS[@suit]}"
  end

  # For printing arrays of cards
  def inspect = to_s
end

def generate(cards)
  cards.shuffle!
  current_card = cards.shift
  player_1_cards = cards.shift(N)
  player_2_cards = cards.shift(N)
  [current_card, player_1_cards, player_2_cards]
end

def simulate(cur, p1, p2)
  # Fails at this test

  # 6C
  # 8D 6H 5S
  # 3H KH 8H

  # Gives true but it's impossible to win
  # The prefix logic should not be working as intended

  q = []
  q << [cur, p1, p2, true, true, 0, [], []]
  results = []

  while !q.empty?
    # q.map { print _1 }
    # puts
    # cp1 = could_play 1
    cur, p1, p2, cp1, cp2, cur_player, rec1, rec2 = q.shift

    if p1.empty? || p2.empty?
      results << [rec1, rec2, cur_player ^ 1]
      next
    end
    next if !cp1 && !cp2

    # Doesn't manke any sense to simulate suits that are not in anybody's hand
    suits = (p1 + p2).map { |c| SUITS[c.suit] }.uniq
    # warn suits * " "

    case cur_player
    when 0
      cards = p1.select { |c| c.matches(cur) }
      did_simulate_eight = false # only need to simulate 1 eight
      if !cards.empty?
        cards.map { |card|
          # We play "card"
          np1 = p1.reject { |c| c == card }
          if card.rank != 7
            q << [card, np1, p2, true, true, 1, rec1 + [card], rec2]
          elsif !did_simulate_eight
            suits.map { |s|
              # We use the 8 + new_suit for the record
              new_card = Card.new("8#{s}")
              q << [new_card, np1, p2, true, true, 1, rec1 + [new_card], rec2]
            }
            did_simulate_eight = true
          end
        }
      else
        q << [cur, p1, p2, false, cp2, 1, rec1, rec2]
      end
    when 1
      cards = p2.select { |c| c.matches(cur) }
      did_simulate_eight = false # only need to simulate 1 eight
      if !cards.empty?
        cards.map { |card|
          np2 = p2.reject { |c| c == card }
          if card.rank != 7
            q << [card, p1, np2, true, true, 0, rec1, rec2 + [card]]
          elsif !did_simulate_eight
            suits.map { |s|
              # We use the 8 + new_suit for the record
              new_card = Card.new("8#{s}")
              q << [new_card, p1, np2, true, true, 0, rec1, rec2 + [new_card]]
            }
            did_simulate_eight = true
          end
        }
      else
        q << [cur, p1, p2, cp1, false, 0, rec1, rec2]
      end
    end
  end

  if !results.empty?
    # warn results.inspect
    results.dup.map { |r|
      r1, r2, w = r
      if w == 1
        results.reject! { |o|
          or1, or2, ow = o
          r != o && r1.is_prefix(or1) || or1.is_prefix(r1)
        }
      end
    }
    # This may be empty now, meaning that 1 wins everytime
    warn results.inspect
    win = results.any? { |rec1, rec2, winner| winner == 0 }
    warn win
    return win
  else
    puts "No winner"
  end
end

def dfs(cur, p1, p2)
  @results = []
  def aux(cur, p1, p2, cp1, cp2, cur_p, rec1, rec2)
    # warn [rec1 * ",", "-", rec2 * ";"] * " "

    if p1.empty? || p2.empty?
      @results << [rec1, rec2, cur_p ^ 1]
      return p1.empty? ? true : false
    end

    if !cp1 && !cp2
      return nil
    end

    suits = (p1 + p2).map { |c| SUITS[c.suit] }.uniq
    res = []

    case cur_p
    when 0
      # Stop as soon as a branch gives false (other player win)
      cards = p1.select { |c| c.matches(cur) }

      # Explore succesors, exit if a single branch is false (other player win)
      if !cards.empty?
        cards.map { |card|
          np1 = p1.reject { |c| c == card }

          if card.rank != 7
            return true if aux(card, np1, p2, true, true, 1, rec1 + [card], rec2)
          else
            return true if suits.any? { |s|
              new_card = Card.new("8#{s}")
              aux(new_card, np1, p2, true, true, 1, rec1 + [new_card], rec2)
            }
          end
        }
      else
        return true if aux(cur, p1, p2, false, cp2, 1, rec1, rec2)
      end
      return false

    when 1
      cards = p2.select { |c| c.matches(cur) }

      # Explore succesors, exit if a single branch is false (other player win)
      if !cards.empty?
        cards.map { |card|
          np2 = p2.reject { |c| c == card }

          if card.rank != 7
            res << aux(card, p1, np2, true, true, 0, rec1, rec2 + [card])
          else
            res += suits.map { |s|
              new_card = Card.new("8#{s}")
              aux(new_card, p1, np2, true, true, 0, rec1, rec2 + [new_card])
            }
          end
        }
      else
        res << aux(cur, p1, p2, cp1, false, 0, rec1, rec2)
      end

      return nil if res.compact.empty?

      # warn ["W1", rec1, rec2, res.empty? ? nil : !res.any? { _1 == false }]* " "

      return res.all? # easier to think like this -> !res.any? { _1 == false }
    end
  end

  res = aux(cur, p1, p2, true, true, 0, [], [])
  # warn "DFS"
  # warn @results * " "
  warn res.nil? ? "No winner" : res
  res
end

def main
  if !TESTC
    c, p1, p2 = init
    warn c, p1 * " ", p2 * " ", "\n"
    # res1 = simulate(c, p1, p2)
    puts dfs(c, p1, p2)
    # puts "ERROR" if res1 != res2
  else
    # For creating tests

    # res1 = 1
    # res2 = 1
    # while true
    #   c, p1, p2 = init
    #   res1 = simulate(c, p1, p2)
    #   res2 = dfs(c, p1, p2)
    #   if res1 != res2
    #     warn c, p1 * " ", p2 * " ", "\n"
    #     exit
    #   end
    #   sleep 0.1
    # end

    while true
      c, p1, p2 = init
      time = Benchmark.measure { dfs(c, p1, p2) }
      if time.real > 1
        puts time.real
        warn c, p1 * " ", p2 * " ", "\n"
        break
      end
      sleep 0.05
    end

  end
end

time = Benchmark.measure { main }
warn "Time %.4f" % time.real
