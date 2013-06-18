/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Bubble Sort http://en.wikipedia.org/wiki/Bubble_sort
 *
 * Worst - O(n^2)
 * Best - O(n)
 * Average - O(n^2)
 *
 * -Notes-
 *
 * This is a functional implementation of standard bubble sort. There are two lists which represent
 * source and result lists correspondingly. So, the 'bubble' function performs element-wise swaps and
 * bubbles the greater element to the last position in the source list. This element should be transfered
 * to the head of result list (which is Nil by default). The interesting thing here is that source list
 * was reversed and we have to reverse it back to origin order. But, we didn't do that, since we don't 
 * care about source data which is still unsorted. We only have to keep sorted data in a right order.
 *
 * Thus this implementation fits into standard requirements about performance. Also, it works in O(n)
 * for lists wich are already sorted.
 */

def bubblesort[A <% Ordered[A]](list: List[A]): List[A] = {
  def sort(as: List[A], bs: List[A]): List[A] =
    if (as.isEmpty) bs
    else bubble(as, Nil, bs)

  def bubble(as: List[A], zs: List[A], bs: List[A]): List[A] = as match {
    case h1 :: h2 :: t =>
      if (h1 > h2) bubble(h1 :: t, h2 :: zs, bs)
      else bubble(h2 :: t, h1 :: zs, bs)
    case h1 :: Nil => sort(zs, h1 :: bs)
  }

  sort(list, Nil)
}

