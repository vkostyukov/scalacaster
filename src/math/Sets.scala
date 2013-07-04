/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 */

object Sets {

  /**
   * Generates all combinations of given set 's' with given length 'n'.
   *
   * Time - O()
   * Space - O()
   */
  def combinations[A](s: List[A], k: Int): List[List[A]] = 
    if (k > s.length) Nil
    else if (k == 1) s.map(List(_))
    else combinations(s.tail, k - 1).map(s.head :: _) ::: combinations(s.tail, k)

  /**
   * Generates all combinations of given set 's'.
   *
   * Time - O()
   * Space - O()
   */
   def combinations[A](s: List[A]): List[List[A]] = 
     (2 to s.length).foldLeft(combinations(s, 1))((a, i) => combinations(s, i) ::: a)
}
