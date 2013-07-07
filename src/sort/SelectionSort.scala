/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Insertion Sort http://en.wikipedia.org/wiki/Selection_sort
 *
 * Worst - O(n^2)
 * Best - O(n^2)
 * Average - O(n^2)
 */

def selectionsort[A <% Ordered[A]](list: List[A]): List[A] = {
  def sort(as: List[A], bs: List[A]): List[A] = as match {
    case h :: t => select(as.max, as, bs)
    case Nil => bs
  }

  def select(m: A, as: List[A], bs: List[A]): List[A] = 
    sort(as.filter(_ != m), m :: bs)

  sort(list, Nil)
}
