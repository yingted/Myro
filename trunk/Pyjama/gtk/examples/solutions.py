# This is the solution for Laura's MIT Python Homework
# D.S. Blank

import operator

def check(number, parts, factors):
	"""
	Checks to see if a number can be factored by parts * factors.
	"""
	if len(parts) != len(factors):
		return False
	return number == sum(map(operator.mul, parts, factors))

def factor(number, parts):
	if len(parts) == 0:
		return []
	part = parts[0]
	solutions = []
	for i in range(number / part, -1, -1):
		if number - i * part == 0:
			solutions.append([i])
		else:
			sub_factors = factor(number - i * part, parts[1:])
			for solution in sub_factors:
				if check(number, parts, [i] + solution):
					solutions.append([i] + solution)
	return solutions
