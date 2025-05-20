Implement commonly used combinations of `Iterator::map`

# Examples

```rust
# use itermaps::MapExt;
let arr = [[1, 2], [3, 4]];
let first: Vec<i32> = arr.iter().map_index(0).copied().collect();
assert_eq!(first, [1, 3]);

let arr = ["foo", "bar"];
let arr1: Vec<String> = arr.into_iter().map_to_owned().collect();
assert_eq!(arr1, arr);
```

```rust
# use itermaps::FilterExt;
let mut iter = [1, 2, 3].iter().filter_ne(&&2);
assert_eq!(iter.next(), Some(&1));
assert_eq!(iter.next(), Some(&3));
assert_eq!(iter.next(), None);
```
