# The pair that gets published

Two pages and two images, uploaded **as they are** into any folder of any server:

    index.html        what it is, how it is built, and the numbers
    buddhabrot.html   the demo that runs in the browser, module inside
    buddhabrot.png    the opening picture
    convergence.gif   the convergence film

Nothing else is needed: no application server, no configuration, no external dependency. The links
between the two pages are **relative**, so the folder works at any address, subfolder included.

⛔ `buddhabrot.html` here is a **copy** of the one beside the source, with the way back added. The
WebAssembly module it carries is realigned by `bash bas/demo/buddhabrot/verify_wasm.sh --bless`, and
the same net **refuses to pass** when this copy carries a different module than the source compiles
to today: a page publishing last week's module looks perfectly fine and is showing something else.
