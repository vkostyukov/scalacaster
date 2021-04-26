/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 */

object Strings {

  /**
   * Checks whether the given string 's' is palindrome or not.
   *
   * Time - O(n)
   * Space - O(1)
   */
  def isPalindrome(s: String): Boolean = {
    def loop(i: Int, j: Int): Boolean =
      if (i >= j) true
      else if (s.charAt(i) == s.charAt(j)) loop(i + 1, j - 1)
      else false

    loop(0, s.length - 1)
  }

  /**
   * Searches for the longest palindrome in given string 's'.
   *
   * http://www.geeksforgeeks.org/dynamic-programming-set-12-longest-palindromic-subsequence/
   *
   * Time - O(n^2)
   * Space - O(n)
   */
  def longestPalindrome(s: String): String = {
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
   * Time - O(mn)
   * Space - O(mn)
   */
  def longestCommonSubstring(a: String, b: String) : String = {
    def loop(m: Map[(Int, Int), Int], bestIndices: List[Int], i: Int, j: Int) : String = {
      if (i > a.length) {
        b.substring(bestIndices(1) - m((bestIndices(0),bestIndices(1))), bestIndices(1))
      } else if (i == 0 || j == 0) {
        loop(m + ((i,j) -> 0), bestIndices, if(j == b.length) i + 1 else i, if(j == b.length) 0 else j + 1)
      } else if (a(i-1) == b(j-1) && math.max(m((bestIndices(0),bestIndices(1))), m((i-1,j-1)) + 1) == (m((i-1,j-1)) + 1)) {
        loop(
          m + ((i,j) -> (m((i-1,j-1)) + 1)),
          List(i, j),
          if(j == b.length) i + 1 else i,
          if(j == b.length) 0 else j + 1
        )
      } else {
        loop(m + ((i,j) -> 0), bestIndices, if(j == b.length) i + 1 else i, if(j == b.length) 0 else j + 1)
      }
    }
    loop(Map[(Int, Int), Int](), List(0, 0), 0, 0)
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
    val n = s.length()
    val m = p.length()
    val q = 3355439
    val r = 256
    val d = (1 until m).foldLeft(1)((a, v) => (a * r) % q)

    def hash(ss: String, m: Int): Int = 
      (0 until m).foldLeft(0)((a, v) => ((a * r) + ss.charAt(v)) % q)

    def loop(hs: Int, hp: Int, i: Int): Int =
      if (hs == hp) i - m
      else if (i == n) -1
      else {
        val dss = (d * s.charAt(i - m)) % q
        val hss = if (hs < dss) (hs - dss + q) else hs - dss
        loop((hss * r + s.charAt(i)) % q, hp, i + 1)
      }

    if (m > n) -1 else loop(hash(s, m), hash(p, m), m)
  }

  /**
   * Checks whether the pattern 'p' is substring of 's' with naive sliding algorithm.
   * If it matches then the function returns the start index, else returns -1.
   *
   * http://www.geeksforgeeks.org/searching-for-patterns-set-1-naive-pattern-searching/
   *
   * NOTES: The good question here: why Java's String.indexOf() uses the similar brute-force 
   *        algorithm instead of Rabin-Karp or KMP?
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

  /**
   * Checks whether the parenthesis are balanced or not.
   * 
   * Time - O(n)
   * Space - O(n)
   */
  def validateParenthesis(s: String): Boolean = {
    def left(i: Int, k: Int): Boolean = 
      if (i == s.length) k == 0
      else if (s.charAt(i) == '(') right(i + 1, k + 1)
      else false

    def right(i: Int, k: Int): Boolean = 
      if (i == s.length) false
      else if (s.charAt(i) == '(') right(i + 1, k + 1)
      else if (k == 1) left(i + 1, k - 1)
      else right(i + 1, k - 1)

    left(0, 0)
  }
}
