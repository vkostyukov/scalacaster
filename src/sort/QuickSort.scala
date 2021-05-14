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
    list match {
      case Nil        => Nil
      case a :: Nil   => List(a)
      case a :: tail  => quickSort(tail.filter(x=> x <= a)) ::: List(a) ::: quickSort(tail.filter(x => x > a))
    }

}
