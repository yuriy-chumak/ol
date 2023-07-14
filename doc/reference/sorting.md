Sorting
========

Sorting functions works only with lists. Insertion sort, merge sort and quick sort are supported.

# sort
`(sort op list)`, *procedure*

Just a sort (in fact a mergesort, but may be changed in the future).

```scheme
(sort < '(1 2))          ==>  '(1 2)
(sort < '(2 1))          ==>  '(1 2)
(sort > '(1 2))          ==>  '(2 1)
(sort < '(1 9 2 8 3 7 4 6 5 0 6 4 7 3 8 2 9 1)) ==> '(0 1 1 2 2 3 3 4 4 5 6 6 7 7 8 8 9 9)
```

# isort
`(isort op list)`, *procedure*

Insertion sort.

```scheme
(isort < '(1 2))          ==>  '(1 2)
(isort < '(2 1))          ==>  '(1 2)
(isort > '(1 2))          ==>  '(2 1)
(isort < '(1 9 2 8 3 7 4 6 5 0 6 4 7 3 8 2 9 1)) ==> '(0 1 1 2 2 3 3 4 4 5 6 6 7 7 8 8 9 9)
```

# mergesort
`(mergesort op list)`, *procedure*

Merge sort.

```scheme
(mergesort < '(1 2))          ==>  '(1 2)
(mergesort < '(2 1))          ==>  '(1 2)
(mergesort > '(1 2))          ==>  '(2 1)
(mergesort < '(1 9 2 8 3 7 4 6 5 0 6 4 7 3 8 2 9 1)) ==> '(0 1 1 2 2 3 3 4 4 5 6 6 7 7 8 8 9 9)
```

# quicksort
`(quicksort op list)`, *procedure*

Quicksort.

```scheme
(quicksort < '(1 2))          ==>  '(1 2)
(quicksort < '(2 1))          ==>  '(1 2)
(quicksort > '(1 2))          ==>  '(2 1)
(quicksort < '(1 9 2 8 3 7 4 6 5 0 6 4 7 3 8 2 9 1)) ==> '(0 1 1 2 2 3 3 4 4 5 6 6 7 7 8 8 9 9)
```
