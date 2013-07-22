/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 */

object Numbers {

  def EPS = 1e-5

  /**
   * Calculates the SQRT of given value 'y' by Newton's method.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def sqrt(x: Double): Double = {
    def loop(y: Double): Double = 
      if (math.abs(y * y - x) / x > EPS) loop(((x / y) + y) / 2.0)
      else y

    loop(1.0)
  }

  /**
   * Computes the exponentiation of given number 'x' in power of 'y'.
   *
   * NOTES: The implementation is taken from SO discussion here:
   * http://stackoverflow.com/questions/17468478/what-is-a-good-way-of-reusing-function-result-in-scala
   *
   * x^2y = (x^y)^2
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def power(x: Double, y: Int): Double = {
    def loop(xx: Double, acc: Double, yy: Int): Double = 
      if (yy == 0) acc
      else if (yy % 2 == 0) loop(xx * xx, acc, yy / 2)
      else loop(xx, acc * xx, yy - 1)

    loop(x, 1.0, y)
  }

  /**
   * Computes the GCD of two given numbers.
   *
   * NOTES: It used Euclid's algorithm.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def gcd(x: Int, y: Int): Int = 
    if (y == 0) x else gcd (y, x % y)

  /**
   * Computes the LCM of two given numbers.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def lcm(x: Int, y: Int): Int = math.abs(x * y) / gcd(x, y)

  /**
   * Prints the `n`th levels of Pascal's Triangle.
   *
   * NOTE: I can do it better. The 'loop' operation looks awfull.
   *
   * Time - O()
   * Space - O()
   */
  def pascalTriangle(n: Int): Unit = {
    def pascal(i: Int, j: Int): Int =
      if (j == 0 || j == i) 1
      else pascal(i - 1, j - 1) + pascal(i - 1, j) 

    def loop(m: Int): Unit =
      if (m < n) {
        for (k <- 0 to m) print(pascal(m, k) + " ")
        print("\n")
        loop(m + 1)
      } else print("\n")

    loop(0)
  }

  /**
   * Checks whether the given number 'n' is power of two.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def isPowerOfTwo(x: Int): Boolean =
   (x & (x - 1)) == 0

  /**
   * Checks whether the given numbers 'n' and 'm' contain the same number of
   * one bits or not.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def isSnoob(x: Int, y: Int): Boolean = 
    if (x == 0 && y == 0) true
    else if (x == 0 || y == 0) false
    else if (x % 2 == 1 && y % 2 == 0) isSnoob(x, y >> 1)
    else if (x % 2 == 0 && y % 2 == 1) isSnoob(x >> 1, y)
    else isSnoob(x >> 1, y >> 1)

  /**
   * Swaps even and odd bits in given number 'x'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def swapEvenAndOdd(x: Int): Int =
    ((x & 0xaaaaaaaa) >> 1) | ((x & 0x55555555) << 1)

  /**
   * Computes the 'n'th Fibonacci number.
   *
   * NOTES: It grows exponentially with golden ratio in base.
   *        It is extremally slow!
   *
   * Time - O(1.6180 ^ n)
   * Space - O(1.6180 ^ n)
   */
  def fibonacci(n: Int): Int =
    if (n == 0 || n == 1) 1
    else fibonacci(n - 1) + fibonacci(n - 2)

  /**
   * Computes the 'n'th Fibonacci number.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def fibonacciIter(n: Int): Int = {
    def loop(a: Int, b: Int, k: Int): Int =
      if (k > 0) loop(b, a + b, k - 1)
      else b

    loop(0, 1, n)
  }

  /**
   * Computes the 'n'th Fibonacci number. 
   *
   * NOTES: It uses the following idea:
   *
   * | 1  1 | ^ n = | F(n+1)  F(n) |
   * | 1  0 |       | F(n)  F(n-1) |
   *
   * Time - O()
   * Space - O()
   */
  def fibonacciMat(n: Int): Int = {
    def loop(a: Int, b: Int, c: Int, d: Int, n: Int, 
             e: Int, f: Int, g: Int, h: Int): Int =
      if (n == 0) f
      else if (n % 2 != 0)
        loop(a * a + b * c, a * b + b * d, c * a + d * c, c * b + d * d, n / 2, 
             e * a + g * b, f * a + h * b, e * c + g * d, f * c + h * d)
      else 
        loop(a * a + b * c, a * b + b * d, c * a + d * c, c * b + d * d, n / 2, 
            e, f, g, h)

    loop(1, 1, 1, 0, n + 1, 1, 0, 0, 1)
  }

  /**
   * Calculates the number of ways of changing 'x' money with 'c' coins.
   *
   * Time - O()
   * Space - O()
   */
  def change(x: Int, c: List[Int]): Int = 
    if (x < 0 || c.isEmpty) 0
    else if (x == 0) 1
    else change(x, c.tail) + change(x - c.head, c)


  /**
   * Negates 'x'.
   *
   * Time - O()
   * Space - O()
   */
  def neg(x: Int): Int = {
    def loop(xx: Int, yy: Int, zz: Int): Int = 
      if (xx == 0) yy
      else loop(xx + zz, yy + zz, zz)

    loop(x, 0, if (x < 0) 1 else -1)
  }

  /**
   * Multiplies 'x' by 'y'.
   *
   * Time - O()
   * Space - O()
   */
  def mul(x: Int, y: Int): Int = {
    def loop(xx: Int, yy: Int): Int = 
      if (yy == math.abs(y)) xx
      else loop(xx + x, yy + 1)

    if (x < 0 && y < 0) loop(0, 0)
    else if (x < 0 || y < 0) neg(loop(0, 0))
    else loop(0, 0)
  }

  /**
   * Subtracts 'y' from 'x'.
   *
   * Time - O()
   * Space - O()
   */
  def sub(x: Int, y: Int): Int = x + neg(y)

  /**
   * Divides 'x' by 'y'.
   *
   * Time - O()
   * Space - O()
   */
  def div(x: Int, y: Int): Int = {
    def loop(xx: Int, yy: Int, zz: Int): Int = 
      if (xx < math.abs(yy)) zz
      else loop(xx + yy, yy, zz + 1)

    if (y == 0) throw new ArithmeticException("Division by zero.")
    else if (x < 0 && y < 0) loop(math.abs(x), neg(math.abs(y)), 0)
    else if (x < 0 || y < 0) neg(loop(math.abs(x), neg(math.abs(y)), 0))
    else loop(math.abs(x), neg(math.abs(y)), 0)
  }

  /**
   * Finds the remainer of division 'x' by 'y'.
   *
   * Time - O()
   * Space - O()
   */
  def rem(x: Int, y: Int): Int = sub(x, mul(y, div(x, y)))

  /**
   * Sums given numbers without '+' or any other arithmetic operation.
   *
   * Time - O()
   * Space - O()
   */
  def sum(x: Int, y: Int): Int =
    if (y == 0) x
    else sum(x ^ y, (x & y) << 1)

  /**
   * Returns the maximum of two numbers.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def max(x: Int, y: Int) = x - (((x - y) >> 31) & 0x1) * (x - y)

  /**
   * Generates first 'n' prime numbers using Sieve of Erastosthenes.
   *
   * Time - O(n log log n)
   * Space - O(n)
   */
  def sievePrimes(n: Int): List[Int] = {
    def loop(s: Stream[Int]): Stream[Int] = 
      s.head #:: loop(s.tail.filter(_ % s.head != 0))

    loop(Stream.from(2)).take(n).toList
  }

  /**
   * Performs the factorization of given number 'n'.
   *
   * Time - O()
   * Space - O()
   */
  def primeFactors(n: Int): List[Int] = {
    def loop(n: Int, m: Int, as: List[Int]): List[Int] = 
      if (m > n) as
      else if (n % m == 0) loop(n / m, m, m :: as)
      else loop(n, m + 1, as)

    loop(n, 2, Nil)
  }

  /**
   * Converts Roman string representation of a number into integer.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def romanToInt(s: String): Int = {
    val digits = Map('M' -> 1000, 'D' -> 500, 
                     'C' -> 100,  'L' -> 50 ,
                     'X' -> 10,   'V' -> 5, 
                     'I' -> 1)

    def loop(i: Int, r: Int): Int =
      if (i == s.length - 1) r + digits(s.charAt(i))
      else {
        val c = digits(s.charAt(i))
        val n = digits(s.charAt(i + 1))
        if (n > c) loop(i + 1, r - c)
        else loop(i + 1, r + c)
      }

    loop(0, 0)
  }

  /**
   * Converts given number 'x' into Roman representaion.
   *
   * Time - O(n)
   * Space - O(n)
   */
  def intToRoman(x: Int): String = {
    val digits = List(1000 ->  "M", 900 -> "CM", 500 ->  "D", 
                       400 -> "CD", 100 ->  "C",  90 -> "XC", 
                        50 ->  "L",  40 -> "XL",  10 ->  "X", 
                         9 -> "IX",   5 ->  "V",   4 -> "IV", 
                         1 ->  "I")

    def loop(l: List[(Int, String)], y: Int): String =
      if (y == 0) ""
      else l.head match {
        case (v, s) if (v <= y) => s + loop(l, y - v)
        case _ => loop(l.tail, y)
      }

    loop(digits, x)
  }
}
