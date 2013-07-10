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
    return a(low)
  }

  var p = (low + hight) / 2
  val q = partition(a, low, hight, p)

  var k = q - low

  if (n < k) {
    return nth(a, low, q, n)
  } else if (n > k) {
    return nth(a, q + 1, hight, n - k - 1)
  }

  return a(q)
}

def partition(a: Array[Int], low: Int, hight: Int, p: Int): Int = {
  println("p = " + p)
  println(a.deep)

  val pivot = a(p)
  swap(a, p, hight - 1)

  var i = low
  for (j  <- low until hight - 1) {
    if (a(j) < pivot) {
      swap(a, i, j)
      i = i + 1
    }
  }

  swap(a, hight - 1, i)

  println("i = " + i + ", left = " + low + ", right = " + hight)
  println(a.deep)
  println("---------")
  return i
}

def swap(a: Array[Int], i: Int, j: Int) = {
  val t = a(i)
  a(i) = a(j)
  a(j) = t
}

println(nth(Array(6, 7, 5, 8, 9), 1))

//assert { nth(Array(5, 2, 1, 3, 4), 1) == 2 }
