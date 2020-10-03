// Homework. Implement functions that calculate
// https://en.wikipedia.org/wiki/Lowest_common_denominator and
// https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

object Basics {
  def lcm(a: Int, b: Int): Int = {
    (a*b).abs/gcd(a,b)
  }

  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a.abs else gcd(b, a % b)
  }

}