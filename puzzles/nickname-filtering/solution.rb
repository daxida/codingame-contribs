class TrieNode
  attr_accessor :children, :is_end

  def initialize()
    @children = {}
    @is_end = false
  end
end

class Trie
  def initialize()
    @root = TrieNode.new
  end

  def insert(word, substitutions)
    node = @root
    q = [node]

    word.chars.each_with_index do |ch, idx|
      nxt = []
      # Ideally this is processed before the trie building into
      # a more convenient data structure like a Hashmap
      subs = substitutions.find { |equal| equal.include?(ch) } || ch

      while !q.empty?
        node = q.shift
        subs.chars.each do |nch|
          node.children[nch] ||= TrieNode.new
          new_node = node.children[nch]
          nxt << new_node
        end
      end
      q = nxt

      q.each { |node| node.is_end = true } if word.size - 1 == idx
    end
  end

  def search(word)
    node = @root
    word.chars.each do |ch|
      return false if node.children[ch].nil?

      node = node.children[ch]
    end
    node.is_end
  end

  def query(word)
    sz = word.size
    (0...sz).any? { |i| (i+1..sz).any? { |j| search(word[i...j]) } }
  end

  def debug(node = @root, prefix = "")
    STDERR.puts prefix if node.is_end
    node.children.each { |char, child| debug(child, prefix + char) }
  end
end

def main
  n_bans, n_substitutions, n_nicknames = gets.split.map &:to_i
  banned_words = Array.new(n_bans) { gets.chomp }
  substitutions = Array.new(n_substitutions) { gets.chomp.tr(" ", "") }
  nicknames = Array.new(n_nicknames) { gets.chomp }

  trie = Trie.new
  banned_words.each do |word|
    trie.insert(word, substitutions)
  end
  # trie.debug

  p nicknames.count { |nickname| trie.query(nickname) }
end

main