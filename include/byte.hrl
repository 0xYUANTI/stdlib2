-ifndef(__S2_BYTE_HRL).
-define(__S2_BYTE_HRL, true).

-define(BITS(Bytes), (Bytes*8)).
-define(BYTES(Bits), (Bits div 8)).

-define(KB(X), (X*1024)).
-define(MB(X), (X*1024*1024)).
-define(GB(X), (X*1024*1024*1024)).
-define(TB(X), (X*1024*1024*1024*1024)).
-define(PB(X), (X*1024*1024*1024*1024*1024)).

-endif.
