require 'tsort'

def E(*args)
  warn "DEB: #{args.inspect}"
end

def compiled?(deps)
  compiled = []
  deps.each do |lib, _deps|
    _deps.each do |dep|
      unless compiled.include? dep
        puts "Import error: tried to import #{lib} but #{dep} is required."
        return false
      end
    end

    compiled << lib
  end

  puts "Compiled successfully!"
  true
end

deps = gets.to_i.times.map {
  lib = gets.chomp.gsub("import ", "")
  [lib, []]
}.to_h

gets.to_i.times do
  lib, _deps = gets.chomp.split(" requires ")
  deps[lib] = _deps.split(", ")
end

warn deps.inspect

$out = []
class Hash
  include TSort

  # def each_item
  #   return unless @state

  #   item = @state.head
  #   while item
  #     yield item
  #     item = item.next
  #   end
  # end

  # def each_key_bis
  #   return to_enum(:each_key) { size } unless block_given?

  #   E "TEST", self

  #   each_item { |e| yield e.key }

  #   self
  # end

  alias each_key_bis each_key

  def each_key(&block)
    E 'hi'
    # can't modify self
    # self = self.sort_by { |k, v| [k, (v - $out).size] }.to_h
    each_key_bis(&block)
  end

  alias tsort_each_node each_key
  # def tsort_each_node(&block)
  #   warn $out.inspect
  #   warn self
  #   warn find { |k, v| (v - $out).size == 0 }
  #   find { |k, v| (v - $out).size == 0 }
  # end

  def tsort_each_child(node, &block)
    warn node, node.class
    $out << node
    fetch(node).each(&block)
  end
end

unless compiled?(deps)
  begin
    warn deps
    warn deps.sort.to_h
    warn deps.sort_by { |k, v| [v.size, k] }.to_h
    reordering = deps.sort.to_h.tsort
    puts "Suggest to change import order:"
    reordering.each { |lib| puts "import #{lib}" }
  rescue TSort::Cyclic
    puts "Fatal error: interdependencies."
  end
end

warn deps.strongly_connected_components.inspect