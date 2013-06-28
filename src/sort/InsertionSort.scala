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

def insertionsort[A <% Ordered](list: List[A]): List[A] = {
  
}

// def insertionsort(a: Array[Int]): Array[Int] = {
//   var result = a.clone()
//   for (j <- 1 until result.length) {
//     val key = result(j)
//     var i = j - 1
//     while (i >= 0 && result(i) > key) {
//       result(i + 1) = result(i)
//       i = i - 1
//     }
//     result(i + 1) = key
//   }
//   result
// }

