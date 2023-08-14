/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Insertion Sort http://en.wikipedia.org/wiki/Insertion_sort
 *
 * Worst - O(n^2)
 * Best - O(n)
 * Average - O(n^2)
 */
import scala.annotation.tailrec

def insertionsort[A <% Ordered[A]](list: List[A]): List[A] = {
  @tailrec
  def sort(as: List[A], bs: List[A]): List[A] = as match {
    case h :: t => sort(t, insert(List(h), bs))
    case Nil => bs
  }

  @tailrec
  def insert(a: List[A], as: List[A]): List[A] = as match {
    case h :: t if (a.head > h) => insert(h :: a, t)
    case _ => a ++ as
  }

  sort(list, Nil)
}
