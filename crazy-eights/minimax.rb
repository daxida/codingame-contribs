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

class State
  attr_reader :p1

  def initialize(cur, p1, p2, cp1, cp2, cur_p)
    @cur = cur     # current card in play
    @p1 = p1       # player 1's hand
    @p2 = p2
    @cp1 = cp1     # player 1 can play (false if he passed its turn)
    @cp2 = cp2
    @cur_p = cur_p # current player
  end

  def over?
    @p1.empty? || @p2.empty? || (!@cp1 && !@cp2)
  end

  def successors
    # Don't simulate suits that are not in anyone's hand
    suits = (@p1 + @p2).map { |c| SUITS[c.suit] }.uniq
    succs = []

    case @cur_p
    when 0
      cards = @p1.select { |c| c.matches(@cur) }

      if !cards.empty?
        cards.map { |card|
          @np1 = @p1.reject { |c| c == card }

          if card.rank != 7
            succs << State.new(card, @np1.dup, @p2.dup, true, true, 1)
          else
            suits.each { |s|
              new_card = Card.new("8#{s}")
              succs << State.new(new_card, @np1.dup, @p2.dup, true, true, 1)
            }
          end
        }
      else
        succs << State.new(@cur, @p1.dup, @p2.dup, false, @cp2, 1)
      end
    when 1
      cards = @p2.select { |c| c.matches(@cur) }

      if !cards.empty?
        cards.map { |card|
          @np2 = @p2.reject { |c| c == card }

          if card.rank != 7
            succs << State.new(card, @p1.dup, @np2, true, true, 0)
          else
            suits.each { |s|
              new_card = Card.new("8#{s}")
              succs << State.new(new_card, @p1.dup, @np2, true, true, 0)
            }
          end
        }
      else
        succs << State.new(@cur, @p1.dup, @p2.dup, @cp1, false, 0)
      end
    end
    succs
  end
end

def minimax(state, maximizing_player)
  return state.p1.empty? ? 1 : -1 if state.over?
  
  if maximizing_player
    state.successors.map { |s| minimax(s, false) }.max
  else
    state.successors.map { |s| minimax(s, true) }.min
  end
end

def main
  c, p1, p2 = init
  s = State.new(c, p1, p2, true, true, 0)
  m = minimax(s, true)
  puts m == 1 ? "true" : "false"
end

time = Benchmark.measure { main }
warn "Time %.4f" % time.real
