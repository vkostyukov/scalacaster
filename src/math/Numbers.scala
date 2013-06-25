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
   * Time - O()
   * Space - O()
   */
  def sqrt(x: Double): Double = {
    def loop(y: Double): Double = 
      if (math.abs(y * y - x) > EPS) loop(((x / y) + y) / 2.0)
      else y

    loop(1.0)
  }

  //def prime()

  //def gcd()

  //def exp()

  //def lcm()
}
