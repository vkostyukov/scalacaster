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
}
