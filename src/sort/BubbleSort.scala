/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Bubble Sort http://en.wikipedia.org/wiki/Bubble_sort
 *
 * Worst - O(n^2)
 * Best - O(n)
 * Average - O(n^2)
 */

def bubblesort(a: Array[Int]): Array[Int] = {
  var result = a.clone()
  for (k <- 0 until result.length) {
    for (i <- 0 until result.length - k - 1) {
      if (result(i) > result(i + 1)) {
        val t = result(i)
        result(i) = result(i + 1)
        result(i + 1) = t
      }
    }
  }
  return result
}

assert { bubblesort(Array(5, 2, 1, 3, 4)).deep == Array(1, 2, 3, 4, 5).deep }
