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
      if (math.abs(y * y - x) > EPS) loop(((x / y) + y) / 2.0)
      else y

    loop(1.0)
  }

  /**
   * Computes the exponentiation of given number 'x' in power of 'y'.
   *
   * Time - O(log n)
   * Space - O(log n)
   */
  def power(x: Double, y: Int): Double = {
    def loop(xx: Double, yy: Int): Double = 
      if (yy == 0) xx
      else if (yy % 2 == 0) { val s = loop(xx, yy / 2); s * s }
      else loop(xx * x, yy - 1)

    loop(1.0, y)
  }

  /**
   * Computes the GCD of two given numbers.
   *
   * NOTES: It used Euclid's algorithm.
   *
   * Time - O(log n)
   * Sparse - O(log n)
   */
  def gcd(x: Int, y: Int): Int = 
    if (y == 0) x else gcd (y, x % y)

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


  //def prime()

  //def lcm()

}
