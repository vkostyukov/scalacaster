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
  def search(as: List[A]): A = 
    if (as.isEmpty) null.asInstanceOf[A]
    else if (as.head == key) as.head
    else search(as.tail)

  search(list)
}
