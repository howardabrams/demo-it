# Given a list of integer values, this
# returns the cube (power of 3) for each even
# number in the list of values.

def cube(x)
  x * x * x
end

def even_cubes(values)
  values.select(&:even?).map(&:cube)
end
