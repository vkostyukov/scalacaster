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

def swap(input: Array[Int], i: Int, j: Int) = {
  val t = input(i)
  input(i) = input(j)
  input(j) = t
}

def partition(input: Array[Int], left: Int, right: Int): Int = {
  val pivot = input((left + right) / 2)
  var i = left - 1
  var j = right + 1

  while (i < j) {

    i += 1
    while (input(i) < pivot) i += 1
    j -= 1
    while (input(j) > pivot) j -= 1

    if (i < j) {
      swap(input, i, j)
    }
  }

  return j
}

def quicksort(input: Array[Int]): Array[Int] = quicksort(input.clone(), 0, input.length - 1)

def quicksort(input: Array[Int], left: Int, right: Int): Array[Int] = {
  if (left < right) {
    val pivot = partition(input, left, right)
    quicksort(input, left, pivot)
    quicksort(input, pivot + 1, right)
  }

  return input
}

assert { quicksort(Array(5, 2, 1, 3, 4)).deep == Array(1, 2, 3, 4, 5).deep }
