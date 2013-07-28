/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Set http://en.wikipedia.org/wiki/Set_(abstract_data_type)
 *
 * Union - O(1)
 * Intersect - O(1)
 * Difference - O(1)
 * Subset - O(1)
 */

abstract class Set[+A] {

  /**
   *
   *
   *
   *
   */
  def isEmpty: Boolean = ???

  /**
   *
   *
   *
   *
   */
  def size: Int = ???

  /**
   *
   *
   *
   *
   */
  def contains[B >: A](x: B): Boolean = ???

  /**
   *
   *
   *
   *
   */
  def union[B >: A](s: Set[B]): Set[B] = ???

  /**
   *
   *
   *
   *
   */
  def intersection[B >: A](s: Set[B]): Set[B] = ???

  /**
   *
   *
   *
   *
   */
  def difference[B >: A](s: Set[B]) = ???


  /**
   *
   *
   *
   *
   */
  def isSubset[B >: A](s: Set[B]): Boolean = ???

  /**
   *
   *
   *
   *
   */
  def combinations(n: Int): List[Set[A]] = 
    if (k > size) Nil // Set()
    else if (k == 1) map(Set(_)).toList
    else tail.combinations(k - 1).map(_ + head) :: tail.combinations(k)

  /**
   *
   *
   *
   *
   */
  def subsets: List[Set[A]] =
    if (isEmpty) Nil
    else (2 to size).foldLeft(s.combinations(1))((a, i) => s.combinations(i) :: a)

  /**
   * Calculates the subset with max sub.
   *
   * Time - O()
   * Space - O()
   */
  def maxSumSubset: Set[A] = ???

  /**
   *
   *
   *
   *
   */
  def maxSumSubset(n: Int): Set[A] = ???


  /**
   *
   * http://www.geeksforgeeks.org/backttracking-set-4-subset-sum/
   *
   *
   */
  def subsetWithSum(x: A): Set[A] = {

  }
}
