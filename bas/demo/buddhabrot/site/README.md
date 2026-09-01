# The pair that gets published

Two pages and two images, uploaded **as they are** into any folder of any server:

    index.html        what it is, how it is built, and the numbers
    buddhabrot.html   the demo that runs in the browser, module inside
    buddhabrot.png    the opening picture
    convergence.gif   the convergence film

⛔ This folder is the BUDDHABROT's, and nothing else belongs in it. `bubble_universe.html` lived
here for one commit and that was wrong: it is a different demo with a different story, and mixing
two of them into one page makes both of them worse. It sits beside its own source, in `bas/demo/`.

Nothing else is needed: no application server, no configuration, no external dependency. The links
between the two pages are **relative**, so the folder works at any address, subfolder included.

⛔ `buddhabrot.html` here is a **copy** of the one beside the source, with the way back added. The
WebAssembly module it carries is realigned by `bash bas/demo/buddhabrot/verify_wasm.sh --bless`, and
the same net **refuses to pass** when this copy carries a different module than the source compiles
to today: a page publishing last week's module looks perfectly fine and is showing something else.
