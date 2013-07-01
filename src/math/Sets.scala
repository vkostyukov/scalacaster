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
  def subsetsScala[A](s: List[A]): List[List[A]] = s match {
    case a :: as => val ss = subsets(as); List(a) :: ss ::: ss.map((x: List[A]) => a :: x)
    case Nil => Nil
  }

  /**
   * Generates all subsets of given set 's'.
   *
   * NOTES: This method uses purely functional style.
   *
   * Time - O()
   * Space - O()
   */
  def subsetsFunc[A](s: List[A]): List[List[A]] = {
    def loop(ss: List[A]): List[List[A]] = ss match {
      case a :: as => merge(a, loop(as), Nil)
      case Nil => Nil
    }

    def merge(a: A, as: List[List[A]], bs: List[List[A]]): List[List[A]] = as match {
      case c :: cs => merge(a, cs, (a :: c) :: c :: bs)
      case Nil => List(a) :: bs
    }

    loop(s)
  }

  /**
   * Generates all permutations of given set 's'
   *
   * Time - O()
   * Space - O()
   */
  def permutations[A](s: List[A]): List[List[A]] = ???
}