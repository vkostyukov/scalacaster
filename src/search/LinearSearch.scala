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

def linearsearchCond[A](list: List[A], key: A): A = {
  def loop(as: List[A]): A = 
    if (as.isEmpty) null.asInstanceOf[A]
    else if (as.head == key) as.head
    else loop(as.tail)

  loop(list)
}

def linearsearchPM[A](list: List[A], key: A): A = {
  def loop(as: List[A]): A = as match {
    case h :: t =>
      if (h == key) h
      else loop(t)
    case Nil => null.asInstanceOf[A]
  } 

  loop(list)
}

def linearsearchIter[A](list: List[A], key: A): A = {
  var that: List[A] = list
  var r: A = null.asInstanceOf[A]

  while (!that.isEmpty && r == null) {
    if (that.head == key) r = that.head
    that = that.tail
  }

  r
}
