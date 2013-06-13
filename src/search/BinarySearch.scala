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

def binarysearch[A <% Ordered[A]](list: List[A], key: A): A = {
  def loop(l: List[A], r: List[A]): A =
    if (l == r) null.asInstanceOf[A]
    else test(l, r, middle(l, r))

  def test(l: List[A], r: List[A], m: List[A]): A =
    if (key < m.head) loop(l, m)
    else if (key > m.head) loop(m.tail, r)
    else m.head

  def middle(l: List[A], r: List[A]): List[A] = {
    def race(t: List[A], h: List[A]): List[A] =
      if (h != r && h.tail != r)
        race(t.tail, h.tail.tail)
      else t

    race(l, l.tail)
  }

  loop(list, Nil)
}