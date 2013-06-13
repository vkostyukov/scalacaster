/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Linear Search https://en.wikipedia.org/wiki/Linear_search
 *
 * Worst - O(n)
 * Best - O(1)
 * Average - O(n)
 *
 * -Notes-
 * 
 * There are some pages about using binary search with singly-linked lists:
 *
 *     http://www.ccscjournal.willmitchell.info/Vol7-91/No5/Marcin%20Paprzycki.pdf
 *
 *                 http://dl.acm.org/citation.cfm?id=101088
 *
 * They says that binary search on linked lists can give some benefits.
 */

def binarysearch[T](a: Array[T], k: T): Int = {
  def loop(l: Int, r: Int): Int = 
    if (l == r) -1
    else {
      val p = (l + r) / 2
      if (a(p) < k) loop(p + 1, r)
      else if (a(p) > k) loop(l, p)
      else p
    }

  loop(0, a.length)
}
