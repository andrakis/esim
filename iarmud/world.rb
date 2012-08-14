# Define the different exits available
class Symbol
	def complete
		case self.to_s.downcase[0]
			when ?n; :north
			when ?s; :south
			when ?e; :east
			when ?w; :west
			when ?u; :up
			when ?d; :down
			else; self
		end
	end

	def opposite
		case self
			when :north; :south
			when :south; :north
			when :east; :west
			when :west; :east
			when :up; :down
			when :down; :up
			when :northeast; :southwest
			when :southeast; :northwest
			when :southwest; :northeast
			when :northwest; :southeast
		end
	end

	def position(x,y)
		case self
			when :north; [x, y - 1]
			when :south; [x, y + 1]
			when :east; [x + 1, y]
			when :west; [x - 1, y]
			when :northeast; [x + 1, y - 1]
			when :southeast; [x + 1, y + 1]
			when :southwest; [x - 1, y + 1]
			when :northwest; [x - 1, y - 1]
		end
	end
end

# Standard directions
Directions = [:north, :south, :east, :west, :northeast, :southeast, :southwest, :northwest]
All_Directions = [:north, :south, :east, :west, :northeast, :southeast, :southwest, :northwest, :up, :down]

# A standard room
class Room
	def initialize (id, title, description = "", exits = {}, tile = " ")
		@id, @title, @description = id, title, description 
		@exits = exits
		@tile = tile
		@art = {}
		@contents = []
	end

	def join (dir, room)
		@exits[dir] = room
		room.exits[dir.opposite] = self
	end

	def border (dir)
		return (@art[dir] || ".") unless @exits[dir]
		return @exits[dir].tile
	end

	def players
		@contents.find_all { |i| i.is_a? Player }
	end

	def contents_as_s(exclude = nil)
		ret = ""
		@contents.each { |i| 
			next if (i.name == exclude) || i.class == exclude
			if i.is_a? Player
				ret += "#{i.name} is here.\n"
			else
				ret += "There is #{i.name} here.\n"
			end
		}
		ret
	end

	def move_entity (what, dir)
		if @exits[dir]
			@contents.delete what
			@exits[dir].contents.push what
			return true
		end
		false
	end

	def map
		v = WorldVisualizer.new(self)
		v.visualize
	end

	attr_accessor :id, :title, :description, :contents, :exits, :tile, :art
end

class WorldVisualizer
	def initialize (start)
		@root = start
		@min_x, @min_y, @max_x, @max_y = -1, -1, 1, 1
		@visual = {}
		@done = {}
	end

	def visualize
		map_room @root, [0, 0]
		size_x = -@min_x + @max_x
		size_y = -@min_y + @max_y
		map = []
		@min_y.upto(@max_y) do |y|
			line = []
			@min_x.upto(@max_x) do |x|
				line.push @visual[[x, y]] || " "
			end
			map.push line.join
		end
		return map.join("\n")
	end

	def map_room (room, pos, prev = nil)
		return if @done[pos]
		@done[pos] = true
		prev = room if prev == nil
		x, y = pos[0], pos[1]
		@min_x = x - 1 if x - 1 < @min_x
		@min_y = y - 1 if y - 1 < @min_y
		@max_x = x + 1 if x + 1 > @max_x
		@max_y = y + 1 if y + 1 > @max_y

		if room.contents.length > 0
			@visual[[x, y]] = room.contents.length
		else
			@visual[[x, y]] = room.tile
		end
		Directions.each do |dir|
			pos = dir.position(x, y)
			@visual[pos] = room.border(dir) unless @visual[pos]
		end

		Directions.each do |dir|
			if room.exits[dir]
				map_room room.exits[dir], dir.position(x, y), prev unless prev == room.exits[dir]
			end
		end
	end
end

class World
	def initialize
		@root = Room.new(0, "Spawning room", "Spawning vat")
		@next_id = 1
	end

	def create_room (title, description = nil, exits = {}, tile = " ")
		id = @next_id
		@next_id++
		Room.new(id, title, description, exits, tile)
	end

	def map
		v = WorldVisualizer.new(@root)
		v.visualize
	end

	attr_accessor :root, :next_id
end

#this is a base class for any Item which a Location can contain:
#	any object in our Game world, from players to AIs to swords and gold.
# you differentiate items in the game world based on their class.
# it doesn't have much in the way of attrubutes: a reference to its location, 
#	a name and description
class Mappable
	attr_accessor :name, :description, :location
	def initialize(name, description, location = nil)
		@name, @description, @location = name, description, location
	end
end

#this is the base class for all living creatures: Human players, AIs
# it has attributes like: health, race, strength, dexterity, speed
class Creature < Mappable
	attr_reader :title, :race, :health, :armour
	def initialize(name, title = nil, race = nil)
		@title = title ? title : ""
		@race = race ? race : "Human"
		@health = 100
		@armour = 0
		super(name, "A #{race}")
	end
end

#this is something which goes in the world which is not a player.
#	it has things like: weight (can a player pick it up? should it just be 'carryable'?)
class Item < Mappable
	
end

# Example subclass:
# swords, knives...
class StabbyItem < Item # < AttackItem ?
	
end

#this is a base class for events which occur within the world.
#	they have a location. Multiple events can be chained into a 
#	sequence.
# we might want a duration / start time (in ticks) for events or
#	a descendant thereof - earthquakes take more than one tick. 
#		maybe so does an attack, leaving you vulnerable...
class Event
	attr_accessor :text, :before, :after
	def initialize(text)
		@text = text
		@before = @after = nil
	end
	
	def insert_before(event)
		event = Event.new(event) if !event.is_a? Event
		@before.after = event if @before
		@before = event
		event.after = self
	end
	
	def insert_after(event)
		event = Event.new(event) if !event.is_a? Event
		@after.before = event if @after
		@after = event
		event.before = self
	end
	
	def to_s
		@text
	end
	
end

class FailureEvent < Event
	def to_s
		"EPIC FAIL: #{@text}"
	end
end

def testworld
	w = World.new
	w.root.join(:east, Room.new(w.next_id + 1, "East", "East"))
	w.root.join(:south, Room.new(w.next_id + 2, "South", "South"))
	w.root.join(:north, Room.new(w.next_id + 3, "North", "North"))
	w.root.join(:west, Room.new(w.next_id + 4, "West", "West"))
	w.root.join(:northeast, Room.new(w.next_id + 5, "Northeast", "NE"))
end

