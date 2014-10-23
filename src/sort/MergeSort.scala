/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Merge Sort http://en.wikipedia.org/wiki/Mergesort
 *
 * Worst - O(n log n)
 * Best - O(n log n)
 * Average - O(n log n)
 *
 * -Notes-
 *
 * Using mergesort is a common way to sort a singly-linked list in a functional environment.
 * This implementation fits into standard requirements about performance. The only difference
 * here with default imperative implementation is a way how to split a list into two parts.
 * This commonly solved by using length of the list, which costs O(n) since requires to walk
 * from the start to the end of this. In this implementation, list is splitted into two equal
 * parts by using 'halfify' function. This function perform very simple thing - it takes first
 * two elements of the list and appends them to two separate lists, which accumulate the result.
 * In other words, this function append all even nodes into first part, while odd nodes into the
 * second one. This can be done in O(n).
 */

def mergesort[A <% Ordered[A]](list: List[A]): List[A] = {
  def sort(p: (List[A], List[A])): List[A] = p match {
    case (Nil, Nil) => Nil
    case (ha :: Nil, Nil) => ha :: Nil
    case (Nil, hb :: Nil) => hb :: Nil
    case (as, bs) => merge(halfifyAndSort(as), halfifyAndSort(bs)) 
  }

  def halfify(as: List[A]): (List[A], List[A]) = {
    def loop(bs: List[A], fs: List[A], ss: List[A]): (List[A], List[A]) = bs match {
      case f :: s :: r => loop(r, f :: fs, s :: ss)
      case f :: Nil => (f :: fs, ss)
      case Nil => (fs, ss)
    }

    loop(as, Nil, Nil)
  }

  def merge(as: List[A], bs: List[A]): List[A] = {
    def loop(cs: List[A], ds: List[A], r: List[A]): List[A] = (cs, ds) match {
      case (ha :: ta, hb :: tb) => 
        if (ha < hb) loop(ta, ds, ha :: r)
        else loop(cs, tb, hb :: r)
      case (ha :: ta, Nil) => loop(ta, Nil, ha :: r)
      case (Nil, hb :: tb) =>  loop(Nil, tb, hb :: r)
      case (Nil, Nil) => Nil
    }

    loop(as, bs, Nil)
  }

  def halfifyAndSort(as: List[A]) = sort(halfify(as))

  halfifyAndSort(list)
}
