module A
  def a1
    puts 'a1 is called'
  end
end
 
module B
  def b1
    puts 'b1 is called'
  end
end
 
module C
  def c1
    puts 'c1 is called'
  end
end
 
class Test
  include A
  include B
  include C
 
  def display
    puts 'Modules are included'
  end
end
 
object=Test.new
object.display
object.a1
object.b1
object.c1
