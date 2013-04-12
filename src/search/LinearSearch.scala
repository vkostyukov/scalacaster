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
 * Space - O(n)
 */

def linearsearch[T](a: Array[T], k: T): Int = {
  def loop(i: Int): Int = 
    if (i == a.length) -1
    else if (a(i) == k) i
    else loop(i + 1)

  loop(0)
}
