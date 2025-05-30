odin-ts-mode

(main "package main

main :: proc() {" n > p q n "}")

(p "@(private)" n)
(pf "@(private=\"file\")" n)

(temp > "arena: mem.Dynamic_Arena" n
      > "mem.dynamic_arena_init(&arena)" n
      > "temp := mem.dynamic_arena_allocator(&arena)" n
      > "defer mem.dynamic_arena_destroy(&arena)" n)

(rd "raw_data(" p q ")")

(todo > "// TODO: ")
