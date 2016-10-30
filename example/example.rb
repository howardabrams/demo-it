#!/usr/bin/env ruby

# Simplistic example of a Ruby function demonstrating that
# quintessential program, the hello world:

def greeting(name)
  if name
    print "Hello, #{name}!\n"
  else
    print "Hello World.\n"
  end
end

greeting(ARGV.first)
