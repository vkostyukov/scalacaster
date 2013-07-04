/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 */

object Sets {

  /**
   * Generates all subsets of given set 's'.
   *
   * NOTES: This method uses Scala Style.
   *
   * Time - O()
   * Space - O()
   */
  def subsets[A](s: List[A]): List[List[A]] = s match {
    case x :: xs => val ss = subsets(xs); List(x) :: ss ::: ss.map(x :: _)
    case Nil => Nil
  }

  /**
   * Generates all combinations of given length 'n'.
   *
   * Time - O()
   * Space - O()
   */
  def combinations[A](s: List[A], n: Int): List[List[A]] = {
    def mix(a: A, as: List[List[A]]): List[List[A]] = 
      as.map(a :: _)

    if (n == 0) Nil
    else if (n == 1) s.map(List(_))
    else mix(s.head, combinations(s.tail, n - 1))
  }

  /**
   * Generates all permutations of given set 's'.
   *
   * Time - O()
   * Space - O()
   */
  def permutations[A](s: List[A]): List[List[A]] = 
    combinations(s, s.length)
}