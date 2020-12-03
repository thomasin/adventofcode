require 'yaml'

class Parse # :nodoc:
  attr_accessor :file

  def initialize(filename)
    binary = File.open(filename, 'rb')
    @file = binary.read
  end

  def into_array
    @file.split(',')
  end
end

class Read # :nodoc:
  def initialize(commands)
    @commands = commands
    @array = %w[a b c d e f g h i j k l m n o p]
  end

  def apply(command, args)
    puts command
    puts args
  end

  def s(num_to_move)
    @array.rotate!(-num_to_move)
  end

  def x 

  def read
    @commands.each do |command|
      apply(command[0], command[1..-1])
    end
  end
end

file = Parse.new('input.txt')
read = Read.new(file.into_array)
read.read
