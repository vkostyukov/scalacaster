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
  def variations[A](l: List[A], k: Int): List[List[A]] = {
    def mixmany(x: A, ll: List[List[A]]): List[List[A]] = 
      if (ll.isEmpty) Nil
      else foldone(x, ll.head) ::: mixmany(x, ll.tail)

    def foldone(x: A, ll: List[A]): List[List[A]] = 
      (1 to ll.length).foldLeft(List(x :: ll))((a, i) => mixone(i, x, ll) :: a)

    def mixone(i: Int, x: A, ll: List[A]): List[A] = 
      ll.slice(0, i) ::: List(x) ::: ll.slice(i, ll.length)

    if (k > l.length) Nil
    else if (k == 1) l.map(List(_))
    else mixmany(l.head, variations(l.tail, k - 1)) ::: variations(l.tail, k)
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
  def permutations[A](l: List[A]): List[List[A]] = 
    (2 to l.length).foldLeft(variations(l, 1))((a, i) => variations(l, i) ::: a)


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
