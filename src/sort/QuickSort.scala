/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Quick Sort http://en.wikipedia.org/wiki/Quicksort
 *
 * Worst - O(n^2)
 * Best - O(n log n)
 * Average - O(n log n)
 */

def swap(a: Array[Int], i: Int, j: Int) = {
  val t = a(i)
  a(i) = a(j)
  a(j) = t
}

def partition(a: Array[Int], left: Int, right: Int): Int = {
  val pivot = a((left + right) / 2)
  var i = left - 1
  var j = right + 1

  while (i < j) {

    i += 1
    while (a(i) < pivot) i += 1
    j -= 1
    while (a(j) > pivot) j -= 1

    if (i < j) {
      swap(a, i, j)
    }
  }

  return j
}

def quicksort(a: Array[Int]): Array[Int] = quicksort(a.clone(), 0, a.length - 1)

def quicksort(a: Array[Int], left: Int, right: Int): Array[Int] = {
  if (left < right) {
    val pivot = partition(a, left, right)
    quicksort(a, left, pivot)
    quicksort(a, pivot + 1, right)
  }

  return a
}

assert { quicksort(Array(5, 2, 1, 3, 4)).deep == Array(1, 2, 3, 4, 5).deep }
