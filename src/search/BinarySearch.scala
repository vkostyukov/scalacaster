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
 * There is a research by Firooz Khosraviyani about binary search on linked lists
 *
 *                  http://dl.acm.org/citation.cfm?id=101088
 *
 * So, Firoozz suggested that it is possible to use binary search on linked lists
 * and it is sometimes more efficient than using simple sequential search. There is
 * also paper 
 *
 *     http://www.ccscjournal.willmitchell.info/Vol7-91/No5/Marcin%20Paprzycki.pdf
 *
 * that says: "the binary search can give some benefits if comparsion of keys at
 * least 2x more expensive than iterating over list".
 *
 * There is also another interesting thing in this implementation. It uses 
 *
 *       http://en.wikipedia.org/wiki/Cycle_detection#Tortoise_and_hare
 *
 * algorithm by R. Floyd for searching the middle of the list in O(n).
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