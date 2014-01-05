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

def linearsearch[A](list: List[A], key: A): Option[A] = {
  def search(as: List[A]): Option[A] = 
    if (as.isEmpty) None
    else if (as.head == key) Some(as.head)
    else search(as.tail)

  search(list)
}
