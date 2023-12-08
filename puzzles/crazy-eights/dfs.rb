require "benchmark"

STDOUT.sync = true

RANKS = "A 2 3 4 5 6 7 8 9 T J Q K".split
SUITS = "C D H S".split
CARDS = RANKS.product(SUITS).map { |r, s| [r, s].join }

INPUT = true  # false for random input
LOCAL = false # For switching CG / LOCAL
N     = 6     # number of cards (if random input)

def init
  if !INPUT
    c, p1, p2 = generate(CARDS.dup)
    c  = Card.new(c)
    p1 = p1.map { |s| Card.new(s) } + [Card.new("8S")]
    p2 = p2.map { |s| Card.new(s) }
  else
    $stdin = File.open("in.txt", "r") if LOCAL
    c, p1, p2 = $<.map { |l| l.chomp.split.map { |c| Card.new(c) } }
    c = c[0]
  end
  [c, p1, p2]
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

def dfs(cur, p1, p2)
    
  def aux(cur, p1, p2, cp1, cp2, cur_p)
    if p1.empty? || p2.empty?
      return p1.empty? ? true : false
    end
    return nil if !cp1 && !cp2
    
    # Can simplify 8s suits (done, but correct?)
    suits = (p1 + p2).map { |c| SUITS[c.suit] }.uniq
    res = []

    case cur_p
    when 0
      cards = p1.select { |c| c.matches(cur) }

      if !cards.empty?
        cards.map { |card|
          np1 = p1.reject { |c| c == card }

          if card.rank != 7
            return true if aux(card, np1, p2, true, true, 1)
          else
            return true if suits.any? { |s|
              new_card = Card.new("8#{s}")
              aux(new_card, np1, p2, true, true, 1)
            }
          end
        }
      else
        return true if aux(cur, p1, p2, false, cp2, 1)
      end
      return false

    when 1
      cards = p2.select { |c| c.matches(cur) }

      if !cards.empty?
        cards.map { |card|
          np2 = p2.reject { |c| c == card }

          if card.rank != 7
            res << aux(card, p1, np2, true, true, 0)
          else
            res += suits.map { |s|
              new_card = Card.new("8#{s}")
              aux(new_card, p1, np2, true, true, 0)
            }
          end
        }
      else
        res << aux(cur, p1, p2, cp1, false, 0)
      end

      return nil if res.compact.empty?
      return res.all? 
    end
  end

  aux(cur, p1, p2, true, true, 0)
end

def main
  c, p1, p2 = init
  warn c, p1 * " ", p2 * " ", "\n"
  puts dfs(c, p1, p2)
end

time = Benchmark.measure { main }
warn "Time %.4f" % time.real
