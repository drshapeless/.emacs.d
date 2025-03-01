odin-ts-mode

(p "@(private)" n)
(pf "@(private=\"file\")" n)

(temp > "temp: mem.Dynamic_Arena" n
      > "mem.dynamic_arena_init(&temp)" n
      > "defer mem.dynamic_arena_destroy(&temp)" n)

(rd "raw_data(" p q ")")
