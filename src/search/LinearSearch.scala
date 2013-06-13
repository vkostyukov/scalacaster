/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Linear Search https://en.wikipedia.org/wiki/Linear_search
 *
 * Worst - O(n)
 * Best - O(1)
 * Average - O(n)
 */

def linearsearch[A](list: List[A], key: A): A = {
  def loop(as: List[A]): List[A] = 
    if (as.isEmpty) Nil
    else if (as.head == key) as.head
    else loop(as.tail)

  loop(list)
}
