def mapReduce(unit: Int, combine: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int): Int = {
  if (a > b) unit else combine(f(a), mapReduce(unit, combine)(f)(a + 1, b))
}
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  mapReduce(0, _ + _)(f)(a, b)
}
def product(f: Int => Int)(a: Int, b: Int): Int = {
  mapReduce(1, _ * _)(f)(a, b)
}
def factorial = (product(x => x) _).curried(1)
sum(x => x)(1, 5)
product(x => x)(1, 5)
factorial(5)
