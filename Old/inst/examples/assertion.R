# Example for trying out assert.
# usage: assertTest()

library(debugR)

assertTest <- function() {
	b = 3
	c = 4

	# Condition is true, so this assertion does nothing.
	assert(8 < 10, env=environment())

	# Condition is false, so this condition allows inspection
	# of the passed environment env, which contains b and c.
	# Once in the assert()'s browser() call, their values
	# can be seen with env$b and env$c.
	assert(5 < 3, env=environment())
}
