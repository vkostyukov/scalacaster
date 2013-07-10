/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Quick Sort http://en.wikipedia.org/wiki/Quicksort
 *
 * Worst - O(n^2)
 * Best - O(n log n)
 * Average - O(n log n)
 */

def quicksort[A <% Ordered[A]](list: List[A]): List[A] = {
  def sort(t: (List[A], A, List[A])): List[A] = t match {
    case (Nil, p, Nil) => List(p)
    case (Nil, p, g) => p :: partitionAndSort(g)
    case (l, p, Nil) => partitionAndSort(l) :+ p
    case (l, p, g) =>  partitionAndSort(l) ::: (p :: partitionAndSort(g))
  }

  def partition(as: List[A]): (List[A], A, List[A]) = {
    def loop(p: A, as: List[A], l: List[A], g: List[A]): (List[A], A, List[A]) = 
      as match {
        case h :: t => if (h < p) loop(p, t, h :: l, g) else loop(p, t, l, h :: g)
        case Nil => (l, p, g)
      }

    loop(as.head, as.tail, Nil, Nil)
  }

  def partitionAndSort(as: List[A]): List[A] = sort(partition(as))

  if (list.isEmpty) Nil
  else partitionAndSort(list)
}
