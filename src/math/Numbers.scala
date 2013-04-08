/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 */

object Numbers {

  def fibonacci(n: Int): Array[Int] = 
    if (n == 1 || n == 0) 1
    else fibonacci(n - 1) + fibonacci(n - 2)

  //def prime()

  //def gcd()

  //def exp()

  //def lcm()
}