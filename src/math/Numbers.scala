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
  def isPowerOfTwo(n: Int): Boolean =
   (n & (n-1)) == 0

  /**
   * Checks whether the given numbers 'n' and 'm' contain the same number of
   * one bits or not.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def isSnoob(n: Int, m: Int): Boolean = 
    if (n == 0 && m == 0) true
    else if (n == 0 || m == 0) false
    else if (n % 2 == 1 && m % 2 == 0) isSnoob(n, m >> 1)
    else if (n % 2 == 0 && m % 2 == 1) isSnoob(n >> 1, m)
    else isSnoob(n >> 1, m >> 1)

  /**
   * Swaps even and odd bits in given number 'n'.
   *
   * Time - O(1)
   * Space - O(1)
   */
  def swapEvenAndOdd(n: Int): Int = {
    def a(m: Int): Int = m match {
      case 0 => 0
      case 1 => 2
      case 2 => 1
      case 3 => 3
      case 4 => 8
      case 5 => 10
      case 6 => 9
      case 7 => 11
      case 8 => 4
      case 9 => 6
      case 10 => 5
      case 11 => 7
      case 12 => 12
      case 13 => 14
      case 14 => 13
      case 15 => 15
    }

    a((n >> 28) & 0xf) << 28 | a((n >> 24) & 0xf) << 24 |
    a((n >> 20) & 0xf) << 20 | a((n >> 16) & 0xf) << 16 |
    a((n >> 12) & 0xf) << 12 | a((n >> 8) & 0xf) << 8 |
    a((n >> 4) & 0xf) << 4 | a(n & 0xf)
  }

  //def prime()

  //def gcd()

  //def exp()

  //def lcm()

}
