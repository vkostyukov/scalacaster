/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * TODO:
 * 1. Move methods 'variations', 'permutations' and 'shuffle' to src/collection/List.scala
 * 2. Add Set as src/collection/Set and move methods 'combinations' and 'subsets' there.
 */

object Sets {

  /**
   * Generates combinations of given set 's' with given length 'k'.
   * The order doesn't matter.
   * 
   * NOTES: To count number of combinations the following formula can be used:
   *
   * C_k,n = n!/(k!(n - k)!
   *
   * Time - O(C_k,n)
   * Space - O(C_k,n)
   */
  def combinations[A](s: Set[A], k: Int): Set[Set[A]] = 
    if (k > s.size) Set()
    else if (k == 1) s.map(Set(_))
    else combinations(s.tail, k - 1).map(_ + s.head) ++ combinations(s.tail, k)

  /**
   * Generates all subsets of given set 's'.
   * The order doesn't matter.
   *
   * NOTES: To count number of subsets the following formual can be used:
   *
   * S_n = SUM(k=1..n, C_k,n), or
   * S_n = 2^n
   *
   * Time - O(S_n)
   * Space - O(S_n)
   */
  def subsets[A](s: Set[A]): Set[Set[A]] =
    (2 to s.size).foldLeft(combinations(s, 1))((a, i) => combinations(s, i) ++ a)
}
