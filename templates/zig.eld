zig-ts-mode

(arena
 > "var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);" n
 > "defer arena.deinit();" n
 > "const allocator = arena.allocator();" n p q
)

(todo > "// TODO: ")

(s "self: *Self")

(h "pub const " (string-inflection-pascal-case-function
             (file-name-nondirectory
              (file-name-sans-extension buffer-file-name)))
   " = @This();" n
   "const Self = @This();" n)

(std "const std = @import(\"std\");")

(init > "pub fn init(" p q ") !Self {" n n
      > "}" n)
