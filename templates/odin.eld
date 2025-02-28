odin-ts-mode

(p "@(private)" n)
(pf "@(private=\"file\")" n)

(temp > "temp: mem.Dynamic_Arena"
      > "mem.dynamic_arena_init(&temp)"
      > "defer mem.dynamic_arena_destroy(&temp)")

(rd "raw_data(" p q ")")
