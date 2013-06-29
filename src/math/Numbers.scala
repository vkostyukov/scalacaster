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
      else yf

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

  //def prime()

  //def gcd()

  //def exp()

  //def lcm()

}
