/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Selection http://en.wikipedia.org/wiki/Selection_algorithm 
 *
 * Worst - O(n^2)
 * Best - O(1)
 * Average - O(n)
 *
 * Doesn't work properly!!!
 */

def nth(a: Array[Int], n: Int): Int = nth(a.clone(), 0, a.length, n)

def nth(a: Array[Int], low: Int, hight: Int, n: Int): Int = {
  if (low == hight - 1) {
    return low
  }

  var p = (hight - low) / 2
  p = partition(a, low, hight, p)

  var k = p - low

  if (n < k) {
    return nth(a, low, p, n)
  } else if (n > k) {
    return nth(a, p, hight, n - k - 1)
  }

  return p
}

def partition(a: Array[Int], low: Int, hight: Int, p: Int): Int = {
  val pivot = a(p)
  swap(a, p, hight - 1)

  var i = low
  for (j  <- low until hight) {
    //println(j)
    if (a(i) <= pivot) {
      swap(a, i, j)
      i = i + 1
    }
  }

  //swap(a, hight - 1, i - 1)
  return i
}

def swap(a: Array[Int], i: Int, j: Int) = {
  val t = a(i)
  a(i) = a(j)
  a(j) = t
}

println(nth(Array(6, 7, 5, 8, 9), 1))

//assert { nth(Array(5, 2, 1, 3, 4), 1) == 2 }
