require 'yaml'

class Parse # :nodoc:
  attr_reader :towers

  def initialize(file)
    @file = File.open(file)
    @towers = {}
  end

  def turn_into_hashes
    @file.each_line do |line|
      tower_name = line.split(' ')[0].strip.to_sym
      @towers[tower_name] = {
        :weight => line.scan(/\(([^\)]+)\)/)[0][0],
        :children => {}
      }

      if line.include?('->')
        line.split('>')[1].split(', ').map do |child|
          @towers[tower_name][:children][child.strip.to_sym] = nil
        end
      end
      @towers[tower_name][:children] ||= nil
    end
    @towers
  end
end

class Merge # :nodoc:
  attr_reader :towers

  def initialize(towers)
    @towers = towers
  end

  def search
    find_tip(@towers)
    return if @towers.keys.length == 1
    search
  end

  def find_tip(tower)
    tower.each do |key, value|
      if value.nil?
        tower[key] = {
          :children => @towers[key] ? @towers[key][:children] : nil,
          :weight => @towers[key] ? @towers[key][:weight].to_i : nil
        }
        @towers.delete(key)
      elsif value[:children].nil?
        next
      else
        find_tip(value[:children])
      end
    end
  end
end

class Weight # :nodoc:
  def initialize(towers)
    @towers = towers
  end

  def find
    second_level = @towers.keys[0].to_sym
    weights(
      @towers,
      @towers[second_level]
    )
  end

  def children_weights(tower)
    weight = 0
    tower.each do |key, value|
      weight += value[:weight]
      weight += children_weights(value[:children]) unless value[:children].nil?
    end
    weight
  end

  def balanced(prev_weights, prev_tower)
    normal = nil
    unique = nil
    prev_weights.each_value do |value|
      normal = value if find_similar(prev_weights, value).length > 1
      unique = value if find_similar(prev_weights, value).length == 1
    end

    difference = normal - unique
    puts prev_tower[:weight] + difference
  end

  def find_similar(weights, value)
    weights.values.find_all { |weight| weight == value }
  end

  def manage_all_weights(prev_weights, tower, all_weights)
    return balanced(prev_weights, tower) if all_weights.values.uniq.length == 1
    all_weights.each do |key, value|
      next unless find_similar(all_weights, value).length == 1
      weights(all_weights, tower[:children][key])
    end
  end

  def weights(prev_weights, tower)
    all_weights = {}
    tower[:children].each do |key, value|
      unless value[:children].nil?
        all_weights[key] = (value[:weight] + children_weights(value[:children]))
      end
    end
    manage_all_weights(prev_weights, tower, all_weights)
  end
end

parse = Parse.new(ARGV[0])
merge = Merge.new(parse.turn_into_hashes)
merge.search
File.write('towers.yaml', merge.towers.to_yaml)
weights = Weight.new(merge.towers)
weights.find
