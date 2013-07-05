/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 */

object Sets {

  /**
   * Generates combinations of given set 's' with given length 'k'.
   * 
   * NOTES: To count number of combinations the following formula can be used:
   *
   * C_k,n = n!/(k!(n - k)!
   *
   * Time - O(C_k,n)
   * Space - O(C_k,n)
   */
  def combinations[A](s: List[A], k: Int): List[List[A]] = 
    if (k > s.length) Nil
    else if (k == 1) s.map(List(_))
    else combinations(s.tail, k - 1).map(s.head :: _) ::: combinations(s.tail, k)

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
      else foldOne(x, ss.head) ::: mixmany(x, ss.tail)

    def foldOne(x: A, ss: List[A]): List[List[A]] = 
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
}
