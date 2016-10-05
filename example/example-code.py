def even_cubes(values):
    """Given a list of integer values, this
returns the cube (power of 3) for each even
 number in the list of values."""
    return [x*x*x for x in values if x % 2 == 0]
