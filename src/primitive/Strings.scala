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
   * http://www.geeksforgeeks.org/dynamic-programming-set-12-longest-palindromic-subsequence/
   *
   * Time - O(n^2)
   * Space - O(n)
   */
  def longestPalindrom(s: String): String = {
    def check(i: Int, j: Int): Boolean = 
      if (i == j) true
      else if (s.charAt(i) == s.charAt(j)) check(i + 1, j - 1)
      else false

    def search(i: Int, l: Int, j: Int, m: Int): String = 
      if (i == s.length) s.substring(j - m + 1, j + 1)
      else if (i - l > 0 && check(i - l - 1, i))
        if (l + 2 > m) search(i + 1, l + 2, i, l + 2)
        else search(i + 1, l + 2, j, m)
      else if (i - l >= 0 && check(i - l, i))
        if (l + 1 > m) search(i + 1, l + 1, i, l + 1)
        else search(i + 1, l + 1, j, m)
      else search(i + 1, 1, j, m)

    if (s.isEmpty) s
    else search(1, 1, 1, 1)
  }

  /**
   * Returns the longest common substring of two strings 'a' and 'b'.
   * 
   * http://www.geeksforgeeks.org/longest-common-substring/
   *
   * TODO: The DP approach can be used here to reduce the compexity to O(mn)
   *
   * Time - O(mn^2)
   * Space - O(mn)
   */
  def longestCommonSubstring(a: String, b: String): String = {
    def loop(i: Int, j: Int): String = 
      if (i == -1 || j == -1) ""
      else if (a.charAt(i) == b.charAt(j)) loop(i - 1, j - 1) + a.charAt(i)
      else {
        val sa = loop(i - 1, j)
        val sb = loop(i, j - 1)
        if (sa.length > sb.length) sa else sb
      }

    loop(a.length - 1, b.length - 1)
  }

  /**
   * Searches for the fist 'n' most frequent words in the string 's'.
   *
   * Time - O()
   * Space - O()
   */
  def mostFrequentWords(s: String, n: Int): List[String] =
    s.split(" ").groupBy(w => w).mapValues(_.size).toList.sortBy(-_._2).map(_._1).take(n)

  /**
   * Checks whether the pattern 'p' is substring of 's' with Rabin-Karp algorithm.
   * If it maches then the function returns the start index, else returns -1.
   * 
   * http://www.geeksforgeeks.org/searching-for-patterns-set-3-rabin-karp-algorithm/
   *
   * Time - O(n + m)
   * Space - O(1)
   */
  def matchRabinKarp(s: String, p: String): Int = {
    ???
  }

  /**
   * Checks whether the pattern 'p' is substring of 's' with naive sliding algorithm.
   * If it maches then the function returns the start index, else returns -1.
   *
   * http://www.geeksforgeeks.org/searching-for-patterns-set-1-naive-pattern-searching/
   *
   * Time - O(nm)
   * Space - O(n)
   */
  def matchNaive(s: String, p: String): Int = {
    def loop(i: Int): Int = 
      if (i == s.length - p.length) -1
      else {
        val ii = matchPattern(i, 0)
        if (ii != -1) ii
        else loop(i + 1)
      }

    def matchPattern(i: Int, j: Int): Int =
      if (j == p.length) i - p.length
      else if (s.charAt(i) == p.charAt(j)) matchPattern(i + 1, j + 1)
      else -1

    loop(0)
  }
}

println(Strings.matchNaive("asdcgd", "dcg"))
