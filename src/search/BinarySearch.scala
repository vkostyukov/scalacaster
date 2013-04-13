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
