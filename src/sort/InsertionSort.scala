/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Insertion Sort http://en.wikipedia.org/wiki/Insertion_sort
 *
 * Worst - O(n^2)
 * Best - O(n)
 * Average - O(n^2)
 */

def insertionsort(a: Array[Int]): Array[Int] = {
  var result = a.clone()
  for (j <- 1 until result.length) {
    val key = result(j)
    var i = j - 1
    while (i >= 0 && result(i) > key) {
      result(i + 1) = result(i)
      i = i - 1
    }
    result(i + 1) = key
  }
  result
}

assert { insertionsort(Array(5, 2, 1, 3, 4)).deep == Array(1, 2, 3, 4, 5).deep }
