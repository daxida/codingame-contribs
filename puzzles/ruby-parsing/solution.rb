require 'stringio'

main = lambda do
  n = gets.to_i
  code = Array.new(n) { gets }.join
  $stdout = StringIO.new
  eval(code)
  ($stdout.string.size == 0).if? {
    STDOUT.puts :none
  }.else?{
    STDOUT.puts $stdout.string
  }
end

class Comparator
  def initialize(starter)
    @cond_chain = [starter] # Array of bool
  end

  def elsif?(condition, &block)
    beginning = proc { condition.if?(&block) }

    @cond_chain.reverse_each.reduce(beginning) do |pro, cond|
      proc { cond.else? { pro.call } }
    end.call

    @cond_chain.push(condition)

    self
  end

  def else?(&block)
    beginning = proc(&block)

    @cond_chain.reverse_each.reduce(beginning) do |pro, cond|
      proc { cond.else? { pro.call } }
    end.call
  end
end

class Object
  def if?
    yield

    Comparator.new self
  end

  def elsif?
    raise 'elsif? must be chained to an if? or elsif?'
  end

  def else?; end
end

class NilClass
  def if?
    Comparator.new self
  end

  def else?
    yield
    self
  end
end

class FalseClass
  def if?
    Comparator.new self
  end

  def else?
    yield
    self
  end
end

main.call