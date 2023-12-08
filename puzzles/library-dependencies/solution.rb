require 'tsort'

class MaxArray < Array
  def <<(v)
    idx = bsearch_index { _1 >= v } || size
    insert(idx, v)
  end
end

def die(msg); puts msg; exit end

def compiled?(deps)
  deps.each.with_object([]) do |(lib, _deps), compiled|
    next compiled << lib unless dep = _deps.find { |dep| !compiled.include? dep }

    puts "Import error: tried to import #{lib} but #{dep} is required."
    return
  end

  die "Compiled successfully!"
end

deps = gets.to_i.times.map do
  lib = gets.chomp.gsub("import ", "")
  [lib, []]
end.to_h

gets.to_i.times do
  lib, _deps = gets.chomp.split(" requires ")
  deps[lib] = _deps.split(", ")
end

compiled?(deps)

# Tarjans just for cycles. Couldn't find a way to make it work
# for building the reordering. Completely unneeded, but I just
# wanted to explore the module

# https://ruby-doc.org/stdlib-2.6.1/libdoc/tsort/rdoc/TSort.html
class Hash
  include TSort
  alias tsort_each_node each_key
  def tsort_each_child(node, &block)
    fetch(node).each(&block)
  end
end

deps.tsort rescue die "Fatal error: interdependencies."

# Custom max heap
h = MaxArray.new(deps.filter_map { |k, v| k if v.empty? }.sort)
record = []

while !h.empty?
  record << lib = h.shift
  deps.each do |other, _deps|
    _deps.delete(lib) if _deps.include? lib
    next unless _deps.empty? && !h.include?(other) && !record.include?(other)

    h << other
  end
end

puts "Suggest to change import order:", record.map { "import #{_1}" }
