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
   * Generates variations of given set 's' with given length 'k'.
   * 
   * NOTES: To count number of variations the following formula can be used:
   * 
   * V_k,n = n!/(n - k)!
   *
   * Time - O(V_k,n)
   * Space - O(V_k,n)
   */
  def variations[A](s: List[A], k: Int): List[List[A]] = {
    def mixmany(x: A, ss: List[List[A]]): List[List[A]] = 
      if (ss.isEmpty) Nil
      else foldone(x, ss.head) ::: mixmany(x, ss.tail)

    def foldone(x: A, ss: List[A]): List[List[A]] = 
      (1 to ss.length).foldLeft(List(x :: ss))((a, i) => mixone(i, x, ss) :: a)

    def mixone(i: Int, x: A, ss: List[A]): List[A] = 
      ss.slice(0, i) ::: List(x) ::: ss.slice(i, ss.length)

    if (k > s.length) Nil
    else if (k == 1) s.map(List(_))
    else mixmany(s.head, variations(s.tail, k - 1)) ::: variations(s.tail, k)
  }

  /**
   * Generates all permutations of given set 's'.
   *
   * NOTES: To count number of permutations the following formula can be used:
   *
   * P_n = V_n,n = n!
   *
   * Time - O(P_n)
   * Space - O(P_n)
   */
  def permutations[A](s: List[A]): List[List[A]] = 
    (2 to s.length).foldLeft(variations(s, 1))((a, i) => variations(s, i) ::: a)


  /**
   * Generates all subsets of given set 's'.
   * The order doesn't matter.
   *
   * NOTES: To count number of subsets the following formual can be used:
   *
   * S_n = SUM(k=1..n, C_k,n)
   *
   * Time - O(S_n)
   * Space - O(S_n)
   */
  def subsets[A](s: Set[A]): Set[Set[A]] =
    (2 to s.size).foldLeft(combinations(s, 1))((a, i) => combinations(s, i) ++ a)

   /**
    * Shuffles given set 's'.
    *
    * Time - O(n^2)
    * Space - O(n^2)
    */
  // def shuffle[A](s: List[A]): List[A] = {
  //   val r = new scala.util.Random()
  //   def dropIndex(ss: List[A], n: Int) = {
  //     val (l, r) = ss.splitAt(n); l ::: r.tail
  //   }
  //   def loop(ss: List[A], n: Int, rr: List[A]): List[A] = 
  //     if (ss.isEmpty) rr
  //     else { val i = r.nextInt(n - 1); loop(dropIndex(ss.tail, i), n - 1, ss.tail(i) :: rr) }

  //   loop(s, s.length, Nil)
  // }
}
