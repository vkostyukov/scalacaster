Why Scalacaster?
----------------

[![Join the chat at https://gitter.im/vkostyukov/scalacaster](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/vkostyukov/scalacaster?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Since [Fender Stratocaster][0] is a classic guitar, **Scalacaster** is about classic algorithms and data structures in Scala. Scalacaster includes loads of widely used implementation techniques and approaches, which have been developed by best programmers and enthusiasts of functional programming. Studying purely functional data structures is always fun and challenge for researchers, since data structures in a functional setting are much elegant and smarter than in an imperative setting.

How to use Scalacaster?
-----------------------

Scalacaster is neither a library nor framework. Moreover, Scalacaster`s code is not supposed to be executed at all. Scalacaster's code is not for Scala compiler but for human beings, for enthusiasts and researchers of the Scala programming language and its application in the area of implementation of the purely functional data structures. So, the best way to use Scalacaster is to read through its source code and comments.

What is inside?
---------------

##### Primitive routines
* Numbers theory [`src/primitive/Numbers.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/primitive/Numbers.scala)
* Strings [`src/primitive/Strings.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/primitive/Strings.scala)

##### Simple Collections
* List [`src/collection/List.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/collection/List.scala)
* Queue [`src/collection/Queue.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/collection/Queue.scala)
* Stack [`src/collection/Stack.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/collection/Stack.scala)
* Set [`src/collection/Set.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/collection/Set.scala)

##### Heaps
* Standard Binary Heap [`src/heap/StandardHeap.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/heap/StandardHeap.scala)
* Leftist Heap [`src/heap/LeftistHeap.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/heap/LeftistHeap.scala)
* Pairing Heap [`src/heap/PairingHeap.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/heap/PairingHeap.scala)
* Skew Heap [`src/heap/SkewHeap.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/heap/SkewHeap.scala)


##### Trees
* Binary Search Tree [`src/tree/Tree.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/tree/Tree.scala)
* Red-Black Tree [`src/tree/RBTree.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/tree/RBTree.scala)
* AA Tree [`src/tree/AATree.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/tree/AATree.scala)


##### Graphs
* Graph [`src/graph/Graph.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/graph/Graph.scala)
* InductiveGraph [`src/graph/InductiveGraph.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/graph/InductiveGraph.scala)

##### Sorting Algorithms
* Quick Sort [`src/sort/QuickSort.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/sort/QuickSort.scala)
* Merge Sort [`src/sort/MergeSort.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/sort/MergeSort.scala)
* Bubble Sort [`src/sort/BubbleSort.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/sort/BubbleSort.scala)
* Insertion Sort [`src/sort/InsertionSort.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/sort/InsertionSort.scala)
* Selection Sort [`src/sort/SelectionSort.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/sort/SelectionSort.scala)

##### Searching Algorithms
* Selection Search (k-th order statistic) [`src/search/SelectionSearch.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/search/SelectionSearch.scala)
* Binary Search [`src/search/BinarySearch.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/search/BinarySearch.scala)
* Linear Search [`src/search/LinearSearch.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/search/LinearSearch.scala)

How to contribute?
------------------

* Give it a star
* Drop the feedback to the author [@vkostyukov](https://twitter.com/vkostyukov)
* Send a PR with fixes of typos/bugs/etc

What to read next?
------------------

* [Slides about Scalacaster][1]
* [Purely Functional Data Structures by Chris Okasaki][2]
* [What's new in Purely Functional DS since Okasaki][3]
* [A Functional Approach to Standard Binary Heaps][4] (see [`StandardHeap.scala`](https://github.com/vkostyukov/scalacaster/blob/master/src/heap/StandardHeap.scala))
* [Combinatorial Algorithms in Scala][5]
* [Designing a Purely Functional Data Structure][6]
* [Functional Data Structures in Scala by Daniel Spiewak][7]

----

<a rel="license" href="http://creativecommons.org/licenses/by/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by/4.0/88x31.png" /></a><br />This work is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by/4.0/">Creative Commons Attribution 4.0 International License</a>.

[0]: http://www.fender.com/guitars/stratocaster/
[1]: http://www.slideshare.net/vkostyukov/purely-functional-data-structures-in-scala-26175521
[2]: http://www.amazon.com/Purely-Functional-Structures-Chris-Okasaki/dp/0521663504
[3]: http://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki
[4]: http://arxiv.org/pdf/1312.4666v1.pdf
[5]: http://vkostyukov.net/posts/combinatorial-algorithms-in-scala/
[6]: http://vkostyukov.net/posts/designing-a-pfds
[7]: https://www.youtube.com/watch?v=pNhBQJN44YQ
