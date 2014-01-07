/**
 * This file is part of Scalacaster project, https://github.com/vkostyukov/scalacaster
 * and written by Vladimir Kostyukov, http://vkostyukov.ru
 *
 * Heap http://en.wikipedia.org/wiki/Heap_(data_structure)
 * 
 * -Notes-
 * 
 * This is Okasaki's Leftist Heap implemetation in Scala.
 */

abstract sealed class Heap[+A <% Ordered[A]] {

}