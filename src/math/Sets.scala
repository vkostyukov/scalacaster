/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 */

object Sets {

  /**
   * Generates all subsets of given set 's'.
   *
   * Time - O()
   * Space - O()
   */
  def subsets[A](s: List[A]): List[List[A]] = s match {
    case a :: as => val ss = subsets(as); List(a) :: ss ::: ss.map((x: List[A]) => a :: x)
    case Nil => Nil
  }

  /**
   * Generates all permutations of given set 's'
   *
   * Time - O()
   * Space - O()
   */
  def permutations[A](s: List[A]): List[List[A]] = ???
}