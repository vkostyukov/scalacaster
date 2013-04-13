/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * http://en.wikipedia.org/wiki/Longest_increasing_subsequence
 *
 * Worst - O(n^2)
 * Best - O(n^2)
 * Average - O(n^2)
 */

def lis(s: Seq[Int]): Seq[Int] = {
  val n = s.length
  val l: Array[Int] = Array.fill(n) { 1 }
  val p: Array[Int] = Array.fill(n) { -1 }

  for (i <- 1 until n) {
    for (j <- 0 until i) {
      if (s(j) < s(i) && l(i) < l(j) + 1) {
        l(i) = l(j) + 1
        p(i) = j
      }
    }
  }

  var r: List[Int] = Nil
  var i = l.indexOf(l.max)
  while (i != -1) {
    r = s(i) :: r
    i = p(i)
  }

  return r
}
