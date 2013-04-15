/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Merge Sort http://en.wikipedia.org/wiki/Quicksort
 *
 * Worst - O(n log n)
 * Best - O(n log n)
 * Average - O(n log n)
 */

def mergesort[T <% Ordered[T]](a: Array[T])(implicit mainfest: Manifest[T]): Array[T] = {
  
  def sort(b: Array[T]): Array[T] = 
    if (b.length == 1) b
    else merge(sort(b.slice(0, b.length / 2)), sort(b.slice(b.length / 2, b.length)), 
               0, 0, 0, new Array[T](b.length))

  def merge(l: Array[T], r: Array[T], i: Int, j: Int, k: Int, p: Array[T]): Array[T] = 
    if(i < l.length && j < r.length)
      if (l(i) < r(j)) { p(k) = l(i); merge(l, r, i + 1, j, k + 1, p) }
      else { p(k) = r(j); merge(l, r, i, j + 1, k + 1, p) }
    else if (i < l.length) { p(k) = l(i); merge(l, r, i + 1, j, k + 1, p) }
    else if (j < r.length) { p(k) = r(j); merge(l, r, i, j + 1, k + 1, p) }
    else p

  sort(a)
}
