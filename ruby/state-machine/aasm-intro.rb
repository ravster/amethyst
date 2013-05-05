# Taken straight from https://github.com/aasm/aasm
require "aasm"

class Job
  include AASM

  aasm do
    state :sleeping, :initial => true
    state :running
    state :cleaning

    event :run do
      transitions :from => :sleeping, :to => :running
    end

    event :clean do
      transitions :from => :running, :to => :cleaning
    end

    event :sleep do
      transitions :from => [:running, :cleaning], :to => :sleeping
    end
  end
end

a = Job.new
puts a.to_s
puts a.sleeping?
puts a.may_run?  # => true
puts a.run
puts a.running?  # => true
puts a.sleeping? # => false
puts a.may_run?  # => false
puts a.run       # This throws an exception since we can't start the "run" event when we are already in the "running" state.
