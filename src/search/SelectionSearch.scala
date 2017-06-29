/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Selection http://en.wikipedia.org/wiki/Selection_algorithm 
 *
 * Worst - O(n^2)
 * Best - O(1)
 * Average - O(n)
 */

def selectionsearch[A <% Ordered[A]](list: List[A], n: Int): Option[A] = { // quickselect
  def search(t: (List[A], A, List[A]), m: Int): Option[A] = t match {
    case (Nil, p, Nil) if m == 0 => Some(p)
    case (Nil, _, Nil) => None
    case (l, p, g) => select(l, p, g, l.length, m)
  }

  def select(l: List[A], p: A, g: List[A], q: Int, m: Int): Option[A] = 
    if (m < q) partitionAndSearch(l, m)
    else if (m > q) partitionAndSearch(g, m - q - 1)
    else Some(p)

  /**
   * The same as in quicksort.
   */
  def partition(as: List[A]): (List[A], A, List[A]) = {
    def loop(p: A, as: List[A], l: List[A], g: List[A]): (List[A], A, List[A]) = 
      as match {
        case h :: t => if (h < p) loop(p, t, h :: l, g) else loop(p, t, l, h :: g)
        case Nil => (l, p, g)
      }

    loop(as.head, as.tail, Nil, Nil)
  }

  def partitionAndSearch(as: List[A], m: Int): Option[A] = 
    if (as.isEmpty) None
    else search(partition(as), m)

  partitionAndSearch(list, n)
}
