/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 */

object Strings {

  /**
   * Checks whether the given string 's' is palindrom or not.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def isPalindrom(s: String): Boolean = {
    def loop(i: Int, j: Int): Boolean =
      if (i == j) true
      else if (s.charAt(i) == s.charAt(j)) loop(i + 1, j - 1)
      else false

    loop(0, s.length - 1)
  }

  /**
   * Searches for the longest palindrom in given string 's'.
   *
   * Time - O(n^2)
   * Space - O(n)
   */
  def longestPalindrom(s: String): String = {
    def check(i: Int, j: Int): Boolean = 
      if (i == j) true
      else if (s.charAt(i) == s.charAt(j)) check(i + 1, j - 1)
      else false

    def fill(i: Int, l: Int): List[Int] = 
      if (i == s.length) Nil
      else if (i - l - 1 >= 0 && check(i - l - 1, i)) l :: fill(i + 1, l + 2)
      else if (i - l >= 0 && check(i - l, i)) l :: fill(i + 1, l + 1)
      else l :: fill(i + 1, 1)

    def fetch(d: List[Int]): String = {
      val l = d.max 
      val i = d.indexOf(l)
      s.substring(i - l + 1, i + 1)
    }

    if (s.isEmpty) s
    else fetch(fill(1, 1))
  }

  /**
   *
   *
   *
   *
   */
  def longestCommonSubstring(a: String, b: String): String = ???

  /**
   *
   *
   *
   *
   */
  def mostFrequentWords(s: String, n: Int): List[String] = ???

  /**
   * Checks whether the string 'ss' is substring of 's' with Rabin-Karp 
   * algorithm.
   *
   * Time - O(n + m)
   * Space - O(1)
   */
  def isSubstring(s: String, ss: String): Boolean = ???
}
